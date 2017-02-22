addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.5.3")

addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "0.8.1")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.6.1")

addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.3.3")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.0")

resolvers += Resolver.url("bintray-simplytyped", url("http://dl.bintray.com/simplytyped/sbt-plugins"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.simplytyped" % "sbt-antlr4" % "0.7.11")
