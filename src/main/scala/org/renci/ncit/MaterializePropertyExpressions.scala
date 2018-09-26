package org.renci.ncit

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import monix.eval._
import monix.execution.Scheduler.Implicits.global
import monix.reactive._
import org.backuity.clist._
import org.geneontology.whelk._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.RioTurtleDocumentFormat
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports

import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration

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
    val restrictions = for {
      property <- Observable.fromIterable(properties)
      cls <- Observable.fromIterable(classes)
    } yield Restriction(property, cls)
    val processed = restrictions.mapParallelUnordered(30) { case Restriction(property, cls) =>
      Task {
        val propertyID = property.getIRI.toString
        val clsID = property.getIRI.toString
        val queryConcept = AtomicConcept(s"$propertyID$clsID")
        val restriction = ExistentialRestriction(Role(propertyID), AtomicConcept(clsID))
        val annotationProperty = AnnotationProperty(property.getIRI)
        val axioms = Set(ConceptInclusion(queryConcept, restriction), ConceptInclusion(restriction, queryConcept))
        val updatedWhelk = Reasoner.assert(axioms, whelk)
        val (equivalents, directSubclasses) = updatedWhelk.directlySubsumes(queryConcept)
        val nonredundantAxioms = (directSubclasses ++ equivalents).map(sc => Class(sc.id) Annotation(annotationProperty, cls))
        val subclasses = updatedWhelk.closureSubsBySuperclass(queryConcept).collect { case (x: AtomicConcept) => x } - queryConcept
        val redundantAxioms = subclasses.map(sc => Class(sc.id) Annotation(annotationProperty, cls))
        (nonredundantAxioms, redundantAxioms)
      }
    }
    val (nonredundantGroups, redundantGroups) = processed.toListL.runSyncUnsafe(Duration.Inf).unzip
    val nonRedundantPropertiesOnt = manager.createOntology(nonredundantGroups.flatten.toSet[OWLAxiom].asJava, IRI.create(s"$prefix/property-graph"))
    manager.applyChange(
      new AddOntologyAnnotation(nonRedundantPropertiesOnt, Annotation(RDFSComment, "This graph provides direct property relationships between classes to support more convenient querying of existential property restrictions. These relationships are derived from the OWL semantics of the main ontology, but are not compatible from an OWL perspective.")))
    manager.saveOntology(nonRedundantPropertiesOnt, new RioTurtleDocumentFormat(), IRI.create(nonRedundantOutputFile))
    val redundantPropertiesOnt = manager.createOntology(redundantGroups.flatten.toSet[OWLAxiom].asJava, IRI.create(s"$prefix/property-graph-redundant"))
    manager.applyChange(
      new AddOntologyAnnotation(redundantPropertiesOnt, Annotation(RDFSComment, "This graph provides direct property relationships between classes to support more convenient querying of existential property restrictions. These relationships are derived from the OWL semantics of the main ontology, but are not compatible from an OWL perspective.")))
    manager.saveOntology(redundantPropertiesOnt, new RioTurtleDocumentFormat(), IRI.create(redundantOutputFile))
  }

  case class Restriction(property: OWLObjectProperty, filler: OWLClass)

}