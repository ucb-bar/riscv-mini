resolvers += "jgit-repo" at "http://download.eclipse.org/jgit/maven"

resolvers += "simplytyped" at "http://simplytyped.github.io/repo/releases"

addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.5.3")

addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "0.8.1")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.0")

addSbtPlugin("com.simplytyped" % "sbt-antlr4" % "0.7.7")
