//requires partner, apex, metadata and tooling jars generated by wsc and placed in ./lib folder

name := "tooling-force.com"

version := "0.1-SNAPSHOT"

scalaVersion := "2.13.4"

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
    //"-Xfuture",
    //"-Ywarn-unused-import"
    // Excluding -byname-implicit is required for Scala 2.13 due to https://github.com/scala/bug/issues/12072
    "-Xlint:_,-byname-implicit", // without this parameter circe deriveDecoder results in: Block result was adapted via implicit conversion
)

// disable generation of scala-<version> folders, we do not need cross compilation
crossPaths := false

sources in doc in Compile := List() // do NOT generate Scaladoc, it is a waste of time

// Get rid of scala-{version} folders
sourceDirectories in Compile ~= ( dirs =>
        dirs.filterNot(_.absolutePath.endsWith("-2.11")).filterNot(_.absolutePath.endsWith("-2.12"))
    )

resolvers ++= Seq(
    "Sonatype OSS Releases"  at "https://oss.sonatype.org/content/repositories/releases/",
    "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)

val sfdcAPIVersion = "50.0.0"
libraryDependencies ++= Seq(
    "com.force.api" % "force-wsc" % "50.0.0",
    "com.force.api" % "force-partner-api" % sfdcAPIVersion,
    "com.force.api" % "force-metadata-api" % sfdcAPIVersion,
    "com.force.api" % "force-apex-api" % sfdcAPIVersion
)

// logging
//libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
//libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
//libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.21"

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.1.1" % "test",
    "com.typesafe.akka" %% "akka-actor" % "2.6.9"
)


libraryDependencies += "io.spray" %%  "spray-json" % "1.3.5"

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
