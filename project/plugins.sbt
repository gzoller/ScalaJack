resolvers += Resolver.url(
  "co.blocke ivy resolver",
  url("http://dl.bintray.com/blocke/releases/")
)(Resolver.ivyStylePatterns)
addSbtPlugin("co.blocke" % "gitflow-packager" % "0.1.7")
addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.5.4")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.6")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")
addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.2.5")
