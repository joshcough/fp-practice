name := "fp-practice"

lazy val fpPractice = Project(
  id = "fp-practice", base = file("."), aggregate = Seq(answers, writeme)
)

lazy val answers = Project(id = "answers", base = file("answers"),
  settings = standardSettings ++ Seq(name := "answers")
)

lazy val writeme = Project(id = "writeme", base = file("writeme"),
  settings = standardSettings ++ Seq(name := "writeme")
)

lazy val standardSettings: Seq[Def.Setting[_]] = {
  Seq[Def.Setting[_]](
     organization := "com.localytics"
    ,scalaVersion := "2.11.7"
    ,version      := "0.1"
    ,addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
    ,crossScalaVersions := Seq("2.10.6", "2.11.7")
    ,libraryDependencies ++= Seq(
       "org.scalaz"     %% "scalaz-core"   % "7.1.5"
      ,"org.scalaz"     %% "scalaz-effect" % "7.1.5"
      ,"org.scalacheck" %% "scalacheck"    % "1.12.5" % "test"
    )
    ,resolvers += Resolver.sonatypeRepo("releases")
    ,scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8", // yes, this is 2 args
      "-feature",
      "-unchecked",
      "-Xfatal-warnings",
      "-Xlint",
      "-Yno-adapted-args",
      "-Ywarn-dead-code", // N.B. doesn't work well with the ??? hole
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Xfuture")
  )
}
