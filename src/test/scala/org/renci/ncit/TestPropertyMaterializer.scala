package org.renci.ncit

import monix.execution.Scheduler.Implicits.global
import org.apache.jena.graph.{Node, NodeFactory, Triple}
import org.geneontology.whelk.{Bridge, Reasoner}
import org.semanticweb.owlapi.apibinding.OWLManager
import utest._

import scala.concurrent.duration.Duration

object TestPropertyMaterializer extends TestSuite {

  private val Prefix = "http://example.org/test"
  private val P = NodeFactory.createURI(s"$Prefix#p")

  private def n: String => Node = NodeFactory.createURI _

  val tests = Tests {

    "Test materialized relations" - {
      val manager = OWLManager.createOWLOntologyManager()
      val ontology = manager.loadOntologyFromOntologyDocument(this.getClass.getResourceAsStream("materialize_test.ofn"))
      val restrictions = MaterializePropertyExpressions.extractAllRestrictions(ontology)
      val whelkOntology = Bridge.ontologyToAxioms(ontology)
      val whelk = Reasoner.assert(whelkOntology)
      val (nonredundant, redundant) = restrictions.map(MaterializePropertyExpressions.processRestriction(_, whelk))
        .reduce((left, right) => (left._1 ++ right._1, left._2 ++ right._2))
        .headL.runSyncUnsafe(Duration.Inf)
      assert(nonredundant(Triple.create(n(s"$Prefix#A"), P, n(s"$Prefix#D"))))
      assert(redundant(Triple.create(n(s"$Prefix#A"), P, n(s"$Prefix#D"))))
      assert(!nonredundant(Triple.create(n(s"$Prefix#C"), P, n(s"$Prefix#D"))))
      assert(redundant(Triple.create(n(s"$Prefix#C"), P, n(s"$Prefix#D"))))
      assert(nonredundant(Triple.create(n(s"$Prefix#F"), P, n(s"$Prefix#B"))))
      assert(redundant(Triple.create(n(s"$Prefix#F"), P, n(s"$Prefix#B"))))
      assert(!nonredundant(Triple.create(n(s"$Prefix#F"), P, n(s"$Prefix#C"))))
      assert(!redundant(Triple.create(n(s"$Prefix#F"), P, n(s"$Prefix#C"))))
      assert(nonredundant(Triple.create(n(s"$Prefix#E"), P, n(s"$Prefix#C"))))
      assert(redundant(Triple.create(n(s"$Prefix#E"), P, n(s"$Prefix#C"))))
      assert(!nonredundant(Triple.create(n(s"$Prefix#E"), P, n(s"$Prefix#A"))))
      assert(redundant(Triple.create(n(s"$Prefix#E"), P, n(s"$Prefix#A"))))
    }
  }

}
