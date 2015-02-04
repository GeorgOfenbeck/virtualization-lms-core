name := "LMS"

version := "0.4.004"

scalaVersion := "2.11.5"

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test-src")

libraryDependencies += "org.scala-lang" % "scala-library" % scalaVersion.value % "test"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.5"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.0"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xlint") //, "-Xlog-implicits"

//parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false

