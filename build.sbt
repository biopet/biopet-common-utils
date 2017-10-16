organization := "com.github.biopet"
name := "biopet-common-utils"

scalaVersion := "2.11.11"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "log4j" % "log4j" % "1.2.17"
libraryDependencies += "commons-io" % "commons-io" % "2.1"
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.3"

libraryDependencies += "com.github.biopet" %% "biopet-test-utils" % "0.1-SNAPTSHOT" % Test changing()

useGpg := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

import ReleaseTransformations._
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  releaseStepCommand("publishSigned"),
  setNextVersion,
  commitNextVersion,
  releaseStepCommand("sonatypeReleaseAll"),
  pushChanges
)
