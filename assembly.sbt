assembly / mainClass := Some("com.neowit.apex.Runner")

//uncomment line below to disable tests during build
assembly / test := {}

assembly / assemblyMergeStrategy := {
    case x if x.endsWith(".java") => MergeStrategy.discard
    case "commands.txt" => MergeStrategy.discard
    case x if x.endsWith(".g4") => MergeStrategy.discard
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case "module-info.class" => MergeStrategy.discard   // this is recommended here https://github.com/sbt/sbt-assembly/issues/391
                                                        // but looks very dodgy, consider changing o something more specific
    //case PathList(xs @ _*) =>
    //    println("================")
    //    println(xs.mkString("/"))
    //    println("================")
    //    MergeStrategy.deduplicate
    //case PathList("jackson-annotations-2.10.3.jar", xs @ _*) => MergeStrategy.last
    //case PathList("jackson-core-2.10.3.jar", xs @ _*) => MergeStrategy.last
    //case PathList("jackson-databind-2.10.3.jar", xs @ _*) => MergeStrategy.last
    case x =>
        val oldStrategy = (assembly / assemblyMergeStrategy).value
        oldStrategy(x)
}

//assemblyOption in assembly ~= { _.copy(includeScala = false) }
assembly / assemblyOption ~= { _.copy(includeScala = true) }
//assemblyOutputPath in assembly := baseDirectory.value
//logLevel in assembly := Level.Debug