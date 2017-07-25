package org.renci.ncit

import org.backuity.clist._

object Main extends App {

  Cli.parse(args).withProgramName("blazegraph-runner").withCommands(MaterializePropertyExpressions).foreach(_.run)

}