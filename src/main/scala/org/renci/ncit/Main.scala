package org.renci.ncit

import org.backuity.clist._

object Main extends App {

  Cli.parse(args).withProgramName("blazegraph-runner").withCommands(MaterializePropertyExpressions, ReplaceMappedTerms).foreach(_.run)

}