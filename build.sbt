enablePlugins(JavaAppPackaging)

organization  := "org.renci"

name          := "ncit-utils"

version       := "0.3.1"

licenses := Seq("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"))

homepage := Some(url("https://github.com/NCI-Thesaurus/ncit-utils"))

scalaVersion  := "2.12.3"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

scalacOptions in Test ++= Seq("-Yrangepos")

mainClass in Compile := Some("org.renci.ncit.Main")

fork in Test := true

libraryDependencies ++= {
  Seq(
    "org.phenoscape"              %% "scowl"                  % "1.3",
    "net.sourceforge.owlapi"      %  "owlapi-distribution"    % "4.3.1",
    "org.semanticweb.elk"         %  "elk-owlapi"             % "0.4.3",
    "org.backuity.clist"          %% "clist-core"             % "3.2.2",
    "org.backuity.clist"          %% "clist-macros"           % "3.2.2" % "provided",
    "com.typesafe.scala-logging"  %% "scala-logging"          % "3.7.1",
    "ch.qos.logback"              %  "logback-classic"        % "1.2.3",
    "org.codehaus.groovy"         %  "groovy-all"             % "2.4.6"
  )
}
