//requires partner, apex, metadata and tooling jars generated by wsc and placed in ./lib folder

name := "tooling-force.com"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.3"

scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-unchecked",
    "-Xfatal-warnings",
    //"-Xlint",
    //"-Yno-adapted-args",
    //"-Ywarn-numeric-widen",
    //"-Ywarn-value-discard",
    "-Xfuture",
    "-Ywarn-unused-import"
)

// disable generation of scala-<version> folders, we do not need cross compilation
crossPaths := false

sources in doc in Compile := List() // do NOT generate Scaladoc, it is a waste of time

// Get rid of scala-{version} folders
sourceDirectories in Compile ~= ( dirs =>
        dirs.filterNot(_.absolutePath.endsWith("-2.11")).filterNot(_.absolutePath.endsWith("-2.12"))
    )

resolvers ++= Seq(
    "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
    "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)

val sfdcAPIVersion = "40.0.0"
libraryDependencies ++= Seq(
    "com.force.api" % "force-wsc" % "40.1.1",
    "com.force.api" % "force-partner-api" % sfdcAPIVersion,
    "com.force.api" % "force-metadata-api" % sfdcAPIVersion,
    "com.force.api" % "force-apex-api" % sfdcAPIVersion
)

// logging
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.7"
//libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.21"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    "com.typesafe.akka" %% "akka-actor" % "2.5.3"
)


libraryDependencies += "io.spray" %%  "spray-json" % "1.3.2"

// embedded web server
val jettyVersion="9.4.0.v20161208"
libraryDependencies ++= Seq(
    "org.eclipse.jetty" % "jetty-webapp" % jettyVersion
)

//exportJars := true

lazy val apexScanner = RootProject(file("../ApexScanner"))

lazy val main = Project(id = "tooling-force-com", base = file("."))
    .dependsOn(apexScanner % "compile->compile;test->test") // "compile->compile;test->test" is needed to allow referencing test code from ApexScanner from "tooling-force-com" module
    .settings(
    )
