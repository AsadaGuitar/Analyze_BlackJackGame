name := "oldblackJack"

version := "0.1.1.2"

scalaVersion := "2.13.6"


libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"

libraryDependencies ++= {
  val akkaVersion = "2.6.8"
  Seq(
    "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
    "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
    "com.typesafe.akka" %% "akka-testkit" % akkaVersion % "test",
  )
}

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.3.0",
  "org.typelevel" %% "cats-effect" % "3.2.2"
)