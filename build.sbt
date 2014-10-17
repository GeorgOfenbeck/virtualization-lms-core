name := "LMS"

version := "0.3.88"

scalaVersion := "2.11.1"

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test-src")

//scalacOptions += "-Yvirtualize"

//scalacOptions += "-Yvirtpatmat"

//scalacOptions in Compile ++= Seq(/*Unchecked, */Deprecation)


libraryDependencies += "org.scala-lang" % "scala-library" % scalaVersion.value % "test"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.5"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"


// tests are not thread safe
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false


//libraryDependencies += "org.neo4j" % "neo4j" % "2.1.4"

// continuations
//autoCompilerPlugins := true

//addCompilerPlugin("org.scala-lang.virtualized.plugins" % "continuations" % virtScala)

//scalacOptions += "-P:continuations:enable"
