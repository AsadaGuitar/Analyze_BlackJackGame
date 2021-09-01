
name := "Analyze_BlackJack"

version := "0.1"

scalaVersion := "3.0.0"


libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.3"

libraryDependencies += "io.monix" %% "monix" % "3.4.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.6.1",
  "org.typelevel" %% "cats-effect" % "2.5.2"
)