mainClass in assembly := Some("com.neowit.apex.Runner")

//uncomment line below to disable tests during build
//test in assembly := {}

assemblyMergeStrategy in assembly := {
    case x if x.endsWith(".java") => MergeStrategy.discard
    case "commands.txt" => MergeStrategy.discard
    case x if x.endsWith(".g4") => MergeStrategy.discard
    case x =>
        val oldStrategy = (assemblyMergeStrategy in assembly).value
        oldStrategy(x)
}

assemblyOption in assembly ~= { _.copy(includeScala = false) }
