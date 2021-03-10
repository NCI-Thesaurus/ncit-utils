package org.renci.ncit

import java.nio.charset.StandardCharsets
import java.util.Collections

import better.files._
import org.backuity.clist._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{AddAxiom, IRI, OWLAxiomChange, OWLLiteral, RemoveAxiom}
import org.semanticweb.owlapi.search.EntitySearcher
import org.semanticweb.owlapi.util.OWLEntityRenamer
import io.circe.yaml.parser

import scala.jdk.CollectionConverters._

object ReplaceMappedTerms extends Command(description = "Replace mapped terms") with Common {

  var mappingFile = arg[String](name = "mappings")
  var ontology = arg[java.io.File](name = "ontology")
  var keepColumn = arg[String](name = "keep", default = "object_id", required = false)
  var dropColumn = arg[String](name = "drop", default = "subject_id", required = false)
  var outputFile = arg[java.io.File](name = "output")

  private val TermReplacedBy = AnnotationProperty("http://purl.obolibrary.org/obo/IAO_0100001")

  def expandCurie(curie: String, curieMap: Map[String, String]): IRI =
    curieMap.find { case (prefix, _) => curie.startsWith(s"$prefix:") }
      .map { case (prefix, namespace) => curie.replaceFirst(s"$prefix:", namespace) }
      .map(IRI.create).getOrElse(throw new IllegalArgumentException(s"No prefix expansion found for $curie"))

  override def run(): Unit = {
    val manager = OWLManager.createOWLOntologyManager()
    val ont = manager.loadOntologyFromOntologyDocument(ontology)
    val renamer = new OWLEntityRenamer(manager, Collections.singleton(ont))
    val headerMap = (for {
      header <- File(mappingFile).lines(StandardCharsets.UTF_8).dropWhile(_.startsWith("#")).take(1).headOption
    } yield {
      val columns = header.split("\t", -1)
      columns.zipWithIndex.toMap
    }).getOrElse(throw new IllegalArgumentException("Unable to parse header"))
    val metadata = File(mappingFile).lines(StandardCharsets.UTF_8).takeWhile(_.startsWith("#")).map(_.replaceFirst("#+", "")).mkString("\n")
    println(metadata)
    val jsonMetadata = parser.parse(metadata).getOrElse(throw new IllegalArgumentException("Unparseable YAML metadata"))
    println(jsonMetadata)
    val curieMap = (for {
      meta <- jsonMetadata.asObject
      _ = println(s"Meta: $meta")
      curiesJson <- meta.toMap.get("curie_map")
      _ = println(s"curiesJson: $curiesJson")
      curies <- curiesJson.as[Map[String, String]].toOption
      _ = println(s"Curies: $curies")
    } yield curies).getOrElse(throw new IllegalArgumentException("Unexpected structure for CURIE map"))
    val keepColumnNum = headerMap.getOrElse(keepColumn, throw new IllegalArgumentException(s"No column: $keepColumn"))
    val dropColumnNum = headerMap.getOrElse(dropColumn, throw new IllegalArgumentException(s"No column: $dropColumn"))
    val newTerms = (for {
      line <- File(mappingFile).lines(StandardCharsets.UTF_8).dropWhile(_.startsWith("#")).drop(1)
      if line.nonEmpty
    } yield {
      val items = line.split("\t", -1)
      println(items.toList)
      val newTerm = expandCurie(items(keepColumnNum), curieMap)
      val oldTerm = expandCurie(items(dropColumnNum), curieMap)
      val changes = renamer.changeIRI(oldTerm, newTerm)
      val logicalChanges = changes.asScala.filter(c => c.isAxiomChange && c.getAxiom.isLogicalAxiom)
      manager.applyChanges(logicalChanges.asJava)
      val labelChanges = EntitySearcher.getAnnotationAssertionAxioms(oldTerm, ont).asScala.filter(_.getProperty == RDFSLabel).flatMap { oldLabelAxiom =>
        val oldLabel = oldLabelAxiom.getValue.asInstanceOf[OWLLiteral].getLiteral
        val newLabelAxiomChanges = if (!oldLabel.startsWith("obsolete ")) {
          val newLabelAxiom = oldTerm Annotation(RDFSLabel, s"obsolete $oldLabel")
          Set(new RemoveAxiom(ont, oldLabelAxiom), new AddAxiom(ont, newLabelAxiom))
        } else Set.empty[OWLAxiomChange]
        Set(
          new AddAxiom(ont, manager.getOWLDataFactory.getDeprecatedOWLAnnotationAssertionAxiom(oldTerm)),
          new AddAxiom(ont, oldTerm.Annotation(TermReplacedBy, newTerm))
        ) ++ newLabelAxiomChanges
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
