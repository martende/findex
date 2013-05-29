name := "findex"

version := "1.0"

scalaVersion := "2.10.0"

// libraryDependencies += "optional" %% "optional" % "0.2"
libraryDependencies += "com.thoughtworks.paranamer" % "paranamer" % "2.2.1"

libraryDependencies += "org.apache.commons" % "commons-io" % "1.3.2"
            
libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.2"

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.2"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"


//resolvers += "sonatype-public" at "https://oss.sonatype.org/content/groups/public"

// set the main Scala source directory to be <base>/src
//scalaSource in Compile <<= baseDirectory(_ / "src")

// set the Scala test source directory to be <base>/test
//scalaSource in Test <<= baseDirectory(_ / "test")

