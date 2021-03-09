package org.renci.ncit

import java.nio.charset.StandardCharsets
import java.util.Collections

import better.files._
import org.backuity.clist._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{AddAxiom, IRI, OWLLiteral, RemoveAxiom}
import org.semanticweb.owlapi.search.EntitySearcher
import org.semanticweb.owlapi.util.OWLEntityRenamer

import scala.jdk.CollectionConverters._

object ReplaceMappedTerms extends Command(description = "Replace mapped terms") with Common {

  var mappingFile = arg[String](name = "mappings")
  var ontology = arg[java.io.File](name = "ontology")
  var keepColumn = arg[Int](name = "keep")
  var dropColumn = arg[Int](name = "drop")
  var outputFile = arg[java.io.File](name = "output")

  private val TermReplacedBy = AnnotationProperty("http://purl.obolibrary.org/obo/IAO_0100001")

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
        val newLabelAxiom = oldTerm Annotation(RDFSLabel, s"obsolete $oldLabel")
        Set(
          new AddAxiom(ont, manager.getOWLDataFactory.getDeprecatedOWLAnnotationAssertionAxiom(oldTerm)),
          new RemoveAxiom(ont, oldLabelAxiom),
          new AddAxiom(ont, newLabelAxiom),
          new AddAxiom(ont, oldTerm.Annotation(TermReplacedBy, newTerm))
        )
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