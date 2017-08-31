package org.renci.ncit

import java.nio.charset.StandardCharsets
import java.util.Collections

import scala.collection.JavaConverters._

import org.backuity.clist._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.AddAxiom
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLLiteral
import org.semanticweb.owlapi.model.RemoveAxiom
import org.semanticweb.owlapi.search.EntitySearcher
import org.semanticweb.owlapi.util.OWLEntityRenamer

import com.typesafe.scalalogging.LazyLogging

import better.files._

object ReplaceMappedTerms extends Command(description = "Replace mapped terms") with Common with LazyLogging {

  var mappingFile = arg[String](name = "mappings")
  var ontology = arg[java.io.File](name = "ontology")
  var keepColumn = arg[Int](name = "keep")
  var dropColumn = arg[Int](name = "drop")
  var outputFile = arg[java.io.File](name = "output")

  override def run(): Unit = {
    val manager = OWLManager.createOWLOntologyManager()
    val ont = manager.loadOntologyFromOntologyDocument(ontology)
    val renamer = new OWLEntityRenamer(manager, Collections.singleton(ont))
    val newTerms = (for {
      line <- File(mappingFile).lines(StandardCharsets.UTF_8).drop(1)
    } yield {
      val items = line.split("\t", -1)
      val newTerm = IRI.create(items(keepColumn))
      val oldTerm = IRI.create(items(dropColumn))
      val changes = renamer.changeIRI(oldTerm, newTerm)
      val logicalChanges = changes.asScala.filter(c => c.isAxiomChange && c.getAxiom.isLogicalAxiom)
      manager.applyChanges(logicalChanges.asJava)
      val labelChanges = EntitySearcher.getAnnotationAssertionAxioms(oldTerm, ont).asScala.filter(_.getProperty == RDFSLabel).flatMap { oldLabelAxiom =>
        val oldLabel = oldLabelAxiom.getValue.asInstanceOf[OWLLiteral].getLiteral
        val newLabelAxiom = oldTerm Annotation (RDFSLabel, s"obsolete $oldLabel")
        Set(
          new AddAxiom(ont, manager.getOWLDataFactory.getDeprecatedOWLAnnotationAssertionAxiom(oldTerm)),
          new RemoveAxiom(ont, oldLabelAxiom),
          new AddAxiom(ont, newLabelAxiom))
      }
      manager.applyChanges(labelChanges.toList.asJava)
      Class(newTerm)
    }).toSet
    val hierarchyRemovals = for {
      ax @ SubClassOf(_, subClass @ Class(_), _) <- ont.getAxioms().asScala
      if newTerms(subClass)
    } yield new RemoveAxiom(ont, ax)
    val equivalenceRemovals = for {
      ax @ EquivalentClasses(_, classes) <- ont.getAxioms().asScala
      if classes.collectFirst { case c @ Class(_) if newTerms(c) => c }.nonEmpty
    } yield new RemoveAxiom(ont, ax)
    manager.applyChanges((hierarchyRemovals ++ equivalenceRemovals).toList.asJava)
    manager.saveOntology(ont, IRI.create(outputFile))
  }

}