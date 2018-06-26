name := "green-game"

version := "0.1"

scalaVersion := "2.12.6"

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.2.4")
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")

scalacOptions ++= List(
  "-Ypartial-unification",
  "-P:bm4:no-filtering:y"
)

libraryDependencies ++= List(
  "org.typelevel" %% "cats-core" % "1.1.0",
  "org.typelevel" %% "cats-effect" % "0.10.1",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)
