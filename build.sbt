lazy val root = project
  .in(file("."))
  .settings(
    name := "fp-in-scala3",
    description := "Example sbt project that compiles using Scala 3",
    version := "0.1.0",

    scalaVersion := dottyLatestNightlyBuild.get,

    useScala3doc := true,
  )