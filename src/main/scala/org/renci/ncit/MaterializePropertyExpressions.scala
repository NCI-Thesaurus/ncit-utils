package org.renci.ncit

import java.io.File
import java.util.UUID

import com.typesafe.scalalogging.LazyLogging
import org.backuity.clist._
import org.geneontology.whelk.{AtomicConcept, Bridge, ConceptInclusion, Reasoner}
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.RioTurtleDocumentFormat
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports

import scala.collection.JavaConverters._

object MaterializePropertyExpressions extends Command(description = "Materialize property expressions") with Common with LazyLogging {

  var ontologyFile = arg[File](name = "ontology")
  var nonRedundantOutputFile = arg[File](name = "nonredundant")
  var redundantOutputFile = arg[File](name = "redundant")

  val prefix = "http://github.com/NCI-Thesaurus/thesaurus-obo-edition"

  override def run(): Unit = {
    val manager = OWLManager.createOWLOntologyManager()
    val ontology = manager.loadOntologyFromOntologyDocument(ontologyFile)
    val properties = ontology.getObjectPropertiesInSignature(Imports.INCLUDED).asScala.toSet
    val classes = ontology.getClassesInSignature(Imports.INCLUDED).asScala.toSet - OWLThing - OWLNothing
    val whelkOntology = Bridge.ontologyToAxioms(ontology)
    val whelk = Reasoner.assert(whelkOntology)
    val (nonRedundantAxioms, redundantAxioms) = properties.par.map { property =>
      logger.info(s"Processing property: $property")
      val (propertyAxioms, mappings) = classes.map(createAxiom(property, _)).unzip
      val clsToRestriction = mappings.toMap
      val newAxioms = propertyAxioms.flatMap(Bridge.convertAxiom).collect { case x: ConceptInclusion => x }
      val updatedWhelk = Reasoner.assert(newAxioms, whelk)
      logger.info(s"Property classification done: $property")
      val taxonomy = updatedWhelk.computeTaxonomy
      logger.info(s"Parallel queries for: $property")
      val (nonRedundantAxiomsForProp, redundantAxiomsForProp) = clsToRestriction.keys.map { cls =>
        val Restriction(_, thisTerm) = clsToRestriction(cls)
        val whelkCls = AtomicConcept(cls.getIRI.toString)
        val (_, directSuperclasses) = taxonomy.getOrElse(whelkCls, (Set.empty, Set.empty))
        val nonRedundantAxiomsForClass = for {
          directSuperClass <- directSuperclasses
          directSuperClassOWL = Class(directSuperClass.id)
          Restriction(_, superTerm) <- clsToRestriction.get(directSuperClassOWL)
        } yield thisTerm Annotation(AnnotationProperty(property.getIRI), superTerm)
        val allSuperClasses = updatedWhelk.closureSubsBySubclass(whelkCls)
        val redundantAxiomsForClass = for {
          AtomicConcept(superClassID) <- allSuperClasses
          superClassOWL = Class(superClassID)
          Restriction(_, superTerm) <- clsToRestriction.get(superClassOWL)
        } yield thisTerm Annotation(AnnotationProperty(property.getIRI), superTerm)
        (nonRedundantAxiomsForClass, redundantAxiomsForClass)
      }.unzip
      (nonRedundantAxiomsForProp.seq.flatten.toSet, redundantAxiomsForProp.seq.flatten.toSet)

    }.unzip

    val nonRedundantPropertiesOnt = manager.createOntology(nonRedundantAxioms.flatten[OWLAxiom].asJava, IRI.create(s"$prefix/property-graph"))
    manager.applyChange(
      new AddOntologyAnnotation(nonRedundantPropertiesOnt, Annotation(RDFSComment, "This graph provides direct property relationships between classes to support more convenient querying of existential property restrictions. These relationships are derived from the OWL semantics of the main ontology, but are not compatible from an OWL perspective.")))
    manager.saveOntology(nonRedundantPropertiesOnt, new RioTurtleDocumentFormat(), IRI.create(nonRedundantOutputFile))
    val redundantPropertiesOnt = manager.createOntology(redundantAxioms.flatten[OWLAxiom].asJava, IRI.create(s"$prefix/property-graph-redundant"))
    manager.applyChange(
      new AddOntologyAnnotation(redundantPropertiesOnt, Annotation(RDFSComment, "This graph provides direct property relationships between classes to support more convenient querying of existential property restrictions. These relationships are derived from the OWL semantics of the main ontology, but are not compatible from an OWL perspective.")))
    manager.saveOntology(redundantPropertiesOnt, new RioTurtleDocumentFormat(), IRI.create(redundantOutputFile))
  }

  def createAxiom(property: OWLObjectProperty, cls: OWLClass): (OWLAxiom, (OWLClass, Restriction)) = {
    val namedPropertyExpression = Class(s"$prefix/expression/${UUID.randomUUID}")
    (namedPropertyExpression EquivalentTo (property some cls), namedPropertyExpression -> Restriction(property, cls))
  }

  case class Restriction(property: OWLObjectProperty, filler: OWLClass)

}