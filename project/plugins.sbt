resolvers += Resolver.url(
  "co.blocke releases resolver",
  url("https://dl.bintray.com/blocke/releases/")
)(Resolver.ivyStylePatterns)
addSbtPlugin("co.blocke" % "gitflow-packager" % "0.1.9")
addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.5.6")
addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.4.6")