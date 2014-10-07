import AssemblyKeys._ // put this at the top of the file

assemblySettings

mainClass in assembly := Some("com.neowit.apex.Runner")

//uncomment line below to disable tests during build
//test in assembly := {}

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case x if x.endsWith(".java") => MergeStrategy.discard
    case x => old(x)
  }
}

assemblyOption in assembly ~= { _.copy(includeScala = false) }
