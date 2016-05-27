name := "LMSz"

version := "1.4"

organization := "EPFL"

scalaVersion := "2.12.0-M3"

scalaSource in Compile <<= baseDirectory(_ / "src/main")

scalaSource in Test <<= baseDirectory(_ / "src/test")

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-library" % _ % "compile")

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _ % "compile")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.5-M3" % "test"
)

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.0" % "test"

// tests are not thread safe
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false

// continuations


autoCompilerPlugins := true


testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "3")

//testOptions in Test += Tests.Argument("-F")

//scalacOptions += "-P:continuations:enable"

val paradiseVersion = "2.1.0"

libraryDependencies ++= (
  if (scalaVersion.value.startsWith("2.10")) List("org.scalamacros" %% "quasiquotes" % paradiseVersion)
  else Nil
)

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _ % "compile")

addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)

// code coverage

//scoverage.ScoverageSbtPlugin.ScoverageKeys.coverageHighlighting := false