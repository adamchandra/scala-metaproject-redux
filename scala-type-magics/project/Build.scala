import sbt._
import Keys._
import Scope.{GlobalScope, ThisScope}

object BuildSettings {
  val buildOrganization = "edu.umass.cs.iesl"
  val buildScalaVersion = "2.9.1"
  val buildVersion = "0.1-SNAPSHOT"


  val buildSettings = Defaults.defaultSettings ++
  Seq (
    organization := buildOrganization,
    scalaVersion := buildScalaVersion,
    version := buildVersion,
    parallelExecution := true,
    retrieveManaged := true,
    autoCompilerPlugins := true,
    externalResolvers <<= resolvers map { rs =>
      Resolver.withDefaultResolvers(rs, mavenCentral = true, scalaTools = true)},

    moduleConfigurations ++= Resolvers.moduleConfigurations,
    javacOptions ++= Seq("-Xlint:unchecked"),
    // publishTo := Some(Resolvers.IESLSnapshotRepo),
    publishTo <<= (version) { version: String => {
      val nexus = "http://iesl.cs.umass.edu:8081/nexus/content/repositories/"
      val selection = if (version.trim.endsWith("SNAPSHOT")) "snapshots" else "releases"
      Some(selection at nexus + selection + "/")
    }},
    publishArtifact in (Compile, packageDoc) := false,
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xcheckinit", "-encoding", "utf8"),
    shellPrompt := ShellPrompt.buildShellPrompt)

  // from http://comments.gmane.org/gmane.comp.lang.scala.simple-build-tool/1285

  lazy val publishLocalTo = SettingKey[Option[Resolver]]("publish-local-to", "The resolver to publish to, locally.")
  lazy val rootSettings = buildSettings ++ Seq(
    otherResolvers <++= publishLocalTo(_.toList),
    publishLocalTo in GlobalScope := Some(Resolver.file(".m2", Path.userHome / ".m2" / "repository")),
    publishLocalConfiguration <<= (packagedArtifacts, publishLocalTo, publishMavenStyle, deliverLocal, ivyLoggingLevel) map { (arts, pTo, mavenStyle, ivyFile, level) =>
      Classpaths.publishConfig(arts, if (mavenStyle) None else Some(ivyFile), resolverName = getLocalResolverName(pTo), logging = level)
    }
  )
  def getLocalResolverName(repo: Option[Resolver]) = if (repo.isDefined) repo.get.name else "local"

}

object ShellPrompt {
  object devnull extends ProcessLogger {
    def info (s: => String) {}
    def error (s: => String) { }
    def buffer[T] (f: => T): T = f
  }
  
  val current = """\*\s+([^\s]+)""".r
  
  def gitBranches = ("git branch --no-color" lines_! devnull mkString)
  def hgBranch = ("hg branch" lines_! devnull mkString)
  
  val buildShellPrompt = {
    (state: State) => {
      val currBranch = hgBranch
      val currProject = Project.extract (state).currentProject.id
      "%s:%s:%s> ".format (currBranch, currProject, BuildSettings.buildVersion)
    }
  }
}



object Resolvers {
  val IESLRepo                = "IESL Repo" at "http://iesl.cs.umass.edu:8081/nexus/content/repositories/releases"
  val IESLSnapshotRepo        = "IESL Snapshot Repo" at "http://iesl.cs.umass.edu:8081/nexus/content/repositories/snapshots"
  val LocalIvy                = Resolver.file("Local .ivy", Path.userHome / ".ivy2" / "local" asFile)
  val LocalM2                 = "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

  val moduleConfigurations = Seq(
  )
}

object Dependencies {
  // val scalaTest = "org.scalatest" % "scalatest_2.9.0" % scalaTestVersion % "test"
  // val scalaCheck = "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test->compile"
  // val specs = "org.scala-tools.testing" %% "specs" % specsVersion % "test->compile"
  // val specs2 = "org.specs2" %% "specs2" % specs2Version % "test->compile"
  // val slf4j = "org.slf4j" % "slf4j-api" % slf4jVersion
  // val logbackClassic = "ch.qos.logback"     %   "logback-classic"     % "0.9.24"
  // val logbackCore = "ch.qos.logback"     %   "logback-core"        % "0.9.24"

  // val scalazCore = "org.scalaz" %% "scalaz-core" % "6.0.3-SNAPSHOT"
  // val scalazHttp = "org.scalaz" % "scalaz-http" % "6.0.2-SNAPSHOT"
  // val scalaj = "org.scalaj" %  "scalaj-collection_2.9.0-1" % "1.1"
  // val scalak = "cc.rexa2" % "scalak_2.9.0" % "0.1-SNAPSHOT"
  // val rexa2Dep = "cc.rexa2" % "rexa2" % "0.1-SNAPSHOT"
  // val junit4 = "junit" % "junit" % "4.4"
}


object ScalaK extends Build {
  val buildShellPrompt = ShellPrompt.buildShellPrompt
  
  import Resolvers._
  import Dependencies._
  import BuildSettings._
  
  val commonDeps = Seq(
    // slf4j,
    // logbackClassic,
    // logbackCore, 
    // scalaTest,
    // junit4
  )

  val printClasspath = TaskKey[File]("print-class-path")

  def printCp = (target, fullClasspath in Compile, compile in Compile) map { (out, cp, analysis) =>
    println(cp.files.map(_.getName).mkString("\n"))
    println("----")
    println(analysis.relations.allBinaryDeps.toSeq.mkString("\n"))
    println("----")
    println(out)
    out
  }

  lazy val scalak:Project = Project(
    id = "scalak", 
    base = file("."), 
    settings = rootSettings ++ Seq (libraryDependencies := commonDeps)
  ) 
}

