enablePlugins(JavaAppPackaging)

organization  := "org.renci"

name          := "ncit-utils"

version       := "0.5.2"

licenses := Seq("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"))

homepage := Some(url("https://github.com/NCI-Thesaurus/ncit-utils"))

scalaVersion  := "2.13.3"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

scalacOptions in Test ++= Seq("-Yrangepos")

mainClass in Compile := Some("org.renci.ncit.Main")

testFrameworks += new TestFramework("utest.runner.Framework")

fork in Test := true

val circeVersion = "0.13.0"

libraryDependencies ++= {
  Seq(
    "org.phenoscape"              %% "scowl"                  % "1.3.4",
    "net.sourceforge.owlapi"      %  "owlapi-distribution"    % "4.5.13",
    "org.semanticweb.elk"         %  "elk-owlapi"             % "0.4.3",
    "org.geneontology"            %% "whelk"                  % "0.6.1",
    "io.monix"                    %% "monix"                  % "3.2.2",
    "org.apache.jena"             %  "apache-jena-libs"       % "3.16.0" exclude("org.slf4j", "slf4j-log4j12"),
    "org.backuity.clist"          %% "clist-core"             % "3.5.1",
    "org.backuity.clist"          %% "clist-macros"           % "3.5.1" % Provided,
    "com.github.pathikrit"        %% "better-files"           % "3.9.1",
    "com.outr"                    %% "scribe-slf4j"           % "2.7.12",
    "io.circe"                    %% "circe-core"             % circeVersion,
    "io.circe"                    %% "circe-generic"          % circeVersion,
    "io.circe"                    %% "circe-parser"           % circeVersion,
    "io.circe"                    %% "circe-yaml"             % circeVersion,
    "com.lihaoyi"                 %% "utest"                  % "0.7.4"  % Test
  )
}
