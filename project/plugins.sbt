resolvers += Resolver.url("co.blocke ivy resolver", url("http://dl.bintray.com/blocke/releases/"))(Resolver.ivyStylePatterns)
addSbtPlugin("co.blocke" % "gitflow-packager" % "0.1.3")
addSbtPlugin("me.lessis" % "bintray-sbt" % "0.3.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.2.10")
addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.5.1")