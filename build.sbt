name := "circe-empty-aware"

version := "0.3"

scalaVersion := "2.12.4"

val circeVersion = "0.9.1"

libraryDependencies ++=  Seq(
  "io.circe"                     %% "circe-generic"                  % circeVersion,
  "org.scalatest"                %% "scalatest"                      % "3.0.4"
)