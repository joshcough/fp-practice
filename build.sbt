name := "fp-practice"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.7"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8", // yes, this is 2 args
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  // "-Ywarn-dead-code", // N.B. doesn't work well with the ??? hole
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture")

libraryDependencies ++= Seq(
   "org.scalaz"     %% "scalaz-core" % "7.1.4"
  ,"org.scalacheck" %% "scalacheck"  % "1.12.5" % "test"
)

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")

// if your project uses multiple Scala versions, use this for cross building
addCompilerPlugin("org.spire-math" % "kind-projector" % "0.7.1" cross CrossVersion.binary)