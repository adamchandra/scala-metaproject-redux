
resolvers += "Web plugin repo" at "http://siasia.github.com/maven2"

resolvers += "DatabinderRepo" at "http://databinder.net/repo"

// libraryDependencies <+= sbtVersion("com.github.siasia" %% "xsbt-web-plugin" % _)

libraryDependencies += "us.technically.spde"       % "spde-sbt-plugin"   % "0.4.2"
