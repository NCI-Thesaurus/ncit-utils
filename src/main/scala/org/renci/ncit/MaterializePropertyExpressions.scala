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
  var outputFile = arg[File](name = "outfile")

  val prefix = "http://github.com/NCI-Thesaurus/thesaurus-obo-edition"
  val onProperty = AnnotationProperty(s"$prefix/onProperty")
  val someValuesFrom = AnnotationProperty(s"$prefix/someValuesFrom")

  def run(): Unit = {
    val manager = OWLManager.createOWLOntologyManager()
    val factory = manager.getOWLDataFactory
    val ontology = manager.loadOntologyFromOntologyDocument(ontologyFile)
    val properties = ontology.getObjectPropertiesInSignature(Imports.INCLUDED).asScala.toSet
    val classes = ontology.getClassesInSignature(Imports.INCLUDED).asScala.toSet
    val axioms = properties.flatMap { property =>
      logger.info(s"Processing property: $property")
      val propertyAxioms = classes.flatMap(createAxioms(property, _))
      val inferredAxioms = inferAxioms(propertyAxioms, ontology)
      propertyAxioms ++ inferredAxioms
    }
    val newAxioms = axioms.filterNot(ontology.containsAxiomIgnoreAnnotations(_))
    val expressionsOntology = manager.createOntology(newAxioms.asJava, IRI.create(s"$prefix/expressions"))
    manager.saveOntology(expressionsOntology, new RioTurtleDocumentFormat(), IRI.create(outputFile))
  }

  def createAxioms(property: OWLObjectProperty, cls: OWLClass): Set[OWLAxiom] = {
    val namedPropertyExpression = Class(s"$prefix/expression/${UUID.randomUUID}")
    Set(
      namedPropertyExpression Annotation (onProperty, property),
      namedPropertyExpression Annotation (someValuesFrom, cls),
      namedPropertyExpression EquivalentTo (property some cls))
  }

  def inferAxioms(startAxioms: Set[OWLAxiom], ontology: OWLOntology): Set[OWLAxiom] = {
    val manager = ontology.getOWLOntologyManager
    val factory = manager.getOWLDataFactory
    val expressionsOntology = manager.createOntology(startAxioms.asJava)
    manager.applyChange(new AddImport(expressionsOntology, factory.getOWLImportsDeclaration(ontology.getOntologyID.getOntologyIRI.get)))
    val reasoner = new ElkReasonerFactory().createReasoner(expressionsOntology)
    //    val generator = new InferredOntologyGenerator(reasoner, List[InferredAxiomGenerator[_ <: OWLAxiom]](new InferredSubClassAxiomGenerator(), new InferredEquivalentClassAxiomGenerator()).asJava)
    val newAxioms = traverse(List(reasoner.getTopClassNode), reasoner, Set.empty)
    //generator.fillOntology(factory, expressionsOntology)
    reasoner.dispose()
    //val newAxioms = expressionsOntology.getAxioms(Imports.EXCLUDED).asScala.toSet
    manager.removeOntology(expressionsOntology)
    newAxioms
  }

  @tailrec
  def traverse(nodes: List[Node[OWLClass]], reasoner: OWLReasoner, acc: Set[OWLAxiom]): Set[OWLAxiom] = nodes match {
    case Nil                               => acc
    case node :: rest if node.isBottomNode => traverse(rest, reasoner, acc)
    case node :: rest =>
      val subclassNodes = reasoner.getSubClasses(node.getRepresentativeElement, true)
      val superclasses = node.getEntities.asScala
      val subclasses = subclassNodes.getFlattened.asScala
      val axioms = for {
        superclass <- superclasses
        subclass <- subclasses
        if (!subclass.isOWLNothing)
      } yield (subclass SubClassOf superclass)
      traverse(subclassNodes.getNodes.asScala.toList ::: rest, reasoner, acc ++ axioms)
  }

}