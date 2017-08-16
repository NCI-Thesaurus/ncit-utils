package org.renci.ncit

import java.io.File
import java.util.UUID

import scala.annotation.tailrec
import scala.collection.JavaConverters._

import org.backuity.clist._
import org.phenoscape.scowl._
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.RioTurtleDocumentFormat
import org.semanticweb.owlapi.model.AddImport
import org.semanticweb.owlapi.model.AddOntologyAnnotation
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLObjectProperty
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.reasoner.Node
import org.semanticweb.owlapi.reasoner.OWLReasoner

import com.typesafe.scalalogging.LazyLogging

object MaterializePropertyExpressions extends Command(description = "Materialize property expressions") with LazyLogging {

  var ontologyFile = arg[File](name = "ontology")
  var nonRedundantOutputFile = arg[File](name = "nonredundant")
  var redundantOutputFile = arg[File](name = "redundant")

  val prefix = "http://github.com/NCI-Thesaurus/thesaurus-obo-edition"

  def run(): Unit = {
    val manager = OWLManager.createOWLOntologyManager()
    val ontology = manager.loadOntologyFromOntologyDocument(ontologyFile)
    val properties = ontology.getObjectPropertiesInSignature(Imports.INCLUDED).asScala.toSet
    val classes = ontology.getClassesInSignature(Imports.INCLUDED).asScala.toSet
    val (newNonRedundantAxioms, newRedundantAxioms) = properties.map { property =>
      logger.info(s"Processing property: $property")
      val (propertyAxioms, mappings) = classes.map(createAxiom(property, _)).unzip
      val clsToRestriction = mappings.toMap
      inferAxioms(propertyAxioms, ontology, clsToRestriction)
    }.unzip
    val nonRedundantPropertiesOnt = manager.createOntology(newNonRedundantAxioms.flatten.asJava, IRI.create(s"$prefix/property-graph"))
    new AddOntologyAnnotation(nonRedundantPropertiesOnt, Annotation(RDFSComment, "This graph provides direct property relationships between classes to support more convenient querying of existential property restrictions. These relationships are derived from the OWL semantics of the main ontology, but are not compatible from an OWL perspective."))
    manager.saveOntology(nonRedundantPropertiesOnt, new RioTurtleDocumentFormat(), IRI.create(nonRedundantOutputFile))
    val redundantPropertiesOnt = manager.createOntology(newRedundantAxioms.flatten.asJava, IRI.create(s"$prefix/property-graph-redundant"))
    new AddOntologyAnnotation(redundantPropertiesOnt, Annotation(RDFSComment, "This graph provides direct property relationships between classes to support more convenient querying of existential property restrictions. These relationships are derived from the OWL semantics of the main ontology, but are not compatible from an OWL perspective."))
    manager.saveOntology(redundantPropertiesOnt, new RioTurtleDocumentFormat(), IRI.create(redundantOutputFile))
  }

  def createAxiom(property: OWLObjectProperty, cls: OWLClass): (OWLAxiom, (OWLClass, Restriction)) = {
    val namedPropertyExpression = Class(s"$prefix/expression/${UUID.randomUUID}")
    (namedPropertyExpression EquivalentTo (property some cls), namedPropertyExpression -> Restriction(property, cls))
  }

  def inferAxioms(startAxioms: Set[OWLAxiom], ontology: OWLOntology, mappings: Map[OWLClass, Restriction]): (Set[OWLAxiom], Set[OWLAxiom]) = {
    val manager = ontology.getOWLOntologyManager
    val factory = manager.getOWLDataFactory
    val expressionsOntology = manager.createOntology(startAxioms.asJava)
    manager.applyChange(new AddImport(expressionsOntology, factory.getOWLImportsDeclaration(ontology.getOntologyID.getOntologyIRI.get)))
    val reasoner = new ElkReasonerFactory().createReasoner(expressionsOntology)
    val (newNonRedundantAxioms, newRedundantAxioms) = traverse(List(reasoner.getTopClassNode), reasoner, Set.empty, Set.empty, Set.empty, mappings)
    reasoner.dispose()
    manager.removeOntology(expressionsOntology)
    (newNonRedundantAxioms, newRedundantAxioms)
  }

  /**
   * This is similar to InferredSubClassAxiomGenerator in OWL API, but much more efficient
   */
  @tailrec
  def traverse(nodes: List[Node[OWLClass]], reasoner: OWLReasoner, nonRedundantAcc: Set[OWLAxiom], redundantAcc: Set[OWLAxiom], traversed: Set[Node[OWLClass]], mappings: Map[OWLClass, Restriction]): (Set[OWLAxiom], Set[OWLAxiom]) = nodes match {
    case Nil                               => (nonRedundantAcc, redundantAcc)
    case node :: rest if traversed(node)   => traverse(rest, reasoner, nonRedundantAcc, redundantAcc, traversed, mappings)
    case node :: rest if node.isBottomNode => traverse(rest, reasoner, nonRedundantAcc, redundantAcc, traversed + node, mappings)
    case node :: rest =>
      val directSubclassNodes = reasoner.getSubClasses(node.getRepresentativeElement, true)
      val allSubclassNodes = reasoner.getSubClasses(node.getRepresentativeElement, false)
      val superclasses = node.getEntities.asScala
      val directSubclasses = directSubclassNodes.getFlattened.asScala
      val allSubclasses = allSubclassNodes.getFlattened.asScala
      val nonRedundantAxioms = for {
        superclass <- superclasses
        if mappings.contains(superclass)
        subclass <- directSubclasses
        if !mappings.contains(subclass)
        if (!subclass.isOWLNothing)
      } yield (subclass Annotation (AnnotationProperty(mappings(superclass).property.getIRI), mappings(superclass).filler))
      val redundantAxioms = for {
        superclass <- superclasses
        if mappings.contains(superclass)
        subclass <- allSubclasses
        if !mappings.contains(subclass)
        if (!subclass.isOWLNothing)
      } yield (subclass Annotation (AnnotationProperty(mappings(superclass).property.getIRI), mappings(superclass).filler))
      traverse(directSubclassNodes.getNodes.asScala.toList ::: rest, reasoner, nonRedundantAcc ++ nonRedundantAxioms, redundantAcc ++ redundantAxioms, traversed + node, mappings)
  }

  case class Restriction(property: OWLObjectProperty, filler: OWLClass)

}