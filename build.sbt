organization := "com.github.biopet"
organizationName := "Biopet"
name := "common-utils"

biopetUrlName := "common-utils"

startYear := Some(2014)

biopetIsTool := false

developers += Developer(id = "ffinfo",
                        name = "Peter van 't Hof",
                        email = "pjrvanthof@gmail.com",
                        url = url("https://github.com/ffinfo"))

scalaVersion := "2.11.12"

libraryDependencies += "log4j" % "log4j" % "1.2.17"
libraryDependencies += "commons-io" % "commons-io" % "2.6"
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.8"
libraryDependencies += "org.yaml" % "snakeyaml" % "1.17"
libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.25"

libraryDependencies += "com.github.biopet" %% "test-utils" % "0.3"
