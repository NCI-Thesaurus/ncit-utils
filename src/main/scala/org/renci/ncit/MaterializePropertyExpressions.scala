package org.renci.ncit

import java.io.{File, FileOutputStream}

import com.typesafe.scalalogging.LazyLogging
import monix.eval._
import monix.execution.Scheduler.Implicits.global
import monix.reactive._
import org.apache.jena.graph.{NodeFactory, Triple}
import org.apache.jena.riot.RDFFormat
import org.apache.jena.riot.system.StreamRDFWriter
import org.backuity.clist._
import org.geneontology.whelk._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports

import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration

object MaterializePropertyExpressions extends Command(description = "Materialize property expressions") with Common with LazyLogging {

  var ontologyFile = arg[File](name = "ontology")
  var nonRedundantOutputFile = arg[File](name = "nonredundant")
  var redundantOutputFile = arg[File](name = "redundant")

  override def run(): Unit = {
    val nonredundantOutputStream = new FileOutputStream(nonRedundantOutputFile)
    val nonredundantRDFWriter = StreamRDFWriter.getWriterStream(nonredundantOutputStream, RDFFormat.TURTLE_FLAT)
    nonredundantRDFWriter.start()
    val redundantOutputStream = new FileOutputStream(redundantOutputFile)
    val redundantRDFWriter = StreamRDFWriter.getWriterStream(redundantOutputStream, RDFFormat.TURTLE_FLAT)
    redundantRDFWriter.start()
    val manager = OWLManager.createOWLOntologyManager()
    val ontology = manager.loadOntologyFromOntologyDocument(ontologyFile)
    val whelkOntology = Bridge.ontologyToAxioms(ontology)
    logger.info("Running reasoner")
    val whelk = Reasoner.assert(whelkOntology)
    logger.info("Done running reasoner")
    val restrictions = extractAllRestrictions(ontology)
    val processed = restrictions.mapParallelUnordered(Runtime.getRuntime.availableProcessors)(r => Task(processRestriction(r, whelk)))
    processed.foreachL { case (nonredundant, redundant) =>
      nonredundant.foreach(nonredundantRDFWriter.triple)
      redundant.foreach(redundantRDFWriter.triple)
    }.runSyncUnsafe(Duration.Inf)
    nonredundantRDFWriter.finish()
    nonredundantOutputStream.close()
    redundantRDFWriter.finish()
    redundantOutputStream.close()
  }

  def extractAllRestrictions(ont: OWLOntology): Observable[Restriction] = {
    val properties = ont.getObjectPropertiesInSignature(Imports.INCLUDED).asScala.toSet
    val classes = ont.getClassesInSignature(Imports.INCLUDED).asScala.toSet - OWLThing - OWLNothing
    for {
      property <- Observable.fromIterable(properties)
      cls <- Observable.fromIterable(classes)
    } yield Restriction(property, cls)
  }

  def processRestriction(combo: Restriction, whelk: ReasonerState): (Set[Triple], Set[Triple]) = {
    val Restriction(property, cls) = combo
    val propertyID = property.getIRI.toString
    val clsID = cls.getIRI.toString
    val queryConcept = AtomicConcept(s"$propertyID$clsID")
    val restriction = ExistentialRestriction(Role(propertyID), AtomicConcept(clsID))
    val axioms = Set(ConceptInclusion(queryConcept, restriction), ConceptInclusion(restriction, queryConcept))
    val updatedWhelk = Reasoner.assert(axioms, whelk)
    val predicate = NodeFactory.createURI(property.getIRI.toString)
    val target = NodeFactory.createURI(cls.getIRI.toString)
    val (equivalents, directSubclasses) = updatedWhelk.directlySubsumes(queryConcept)
    val subclasses = updatedWhelk.closureSubsBySuperclass(queryConcept).collect { case x: AtomicConcept => x } - queryConcept - BuiltIn.Bottom
    if (!equivalents(BuiltIn.Bottom)) {
      val nonredundantAxioms = (directSubclasses - BuiltIn.Bottom ++ equivalents).map(sc => Triple.create(NodeFactory.createURI(sc.id), predicate, target))
      val redundantAxioms = subclasses.map(sc => Triple.create(NodeFactory.createURI(sc.id), predicate, target))
      (nonredundantAxioms, redundantAxioms)
    } else (Set.empty[Triple], Set.empty[Triple])
  }

  case class Restriction(property: OWLObjectProperty, filler: OWLClass)

}