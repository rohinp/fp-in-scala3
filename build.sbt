lazy val root = project
  .in(file("."))
  .settings(
    name := "fp-in-scala3",
    description := "FP in Scala3 by example tutorial",
    version := "0.1.0",
    scalaVersion := "3.0.0-RC2",
    useScala3doc := true
  )
initialCommands in Compile in console :=
  """
    |import tdd.chapter1.Prelude._
    |import tdd.chapter3.MatrixOps._
    |import parsercombinator.Parser._
    |import parsercombinator.ParserOps._
    |""".stripMargin