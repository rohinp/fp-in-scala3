lazy val root = project
  .in(file("."))
  .settings(
    name := "fp-in-scala3",
    description := "FP in Scala3 by example tutorial",
    version := "0.1.0",
    scalaVersion := "3.0.0-M3",
    useScala3doc := true
  )
initialCommands in Compile in console :=
  """
    |import tdd.chapter1.Prelude._
    |import tdd.chapter1.Vect
    |import tdd.chapter1.Vect._
    |import tdd.chapter1.VectOps._
    |import tdd.chapter3.MatrixOps._
    |""".stripMargin
libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.1" withSources()