name := "findex"

version := "1.0"

scalaVersion := "2.9.2"

// libraryDependencies += "optional" %% "optional" % "0.2"
libraryDependencies += "com.thoughtworks.paranamer" % "paranamer" % "2.2.1"

libraryDependencies += "org.apache.commons" % "commons-io" % "1.3.2"
            
//resolvers += "sonatype-public" at "https://oss.sonatype.org/content/groups/public"

// set the main Scala source directory to be <base>/src
scalaSource in Compile <<= baseDirectory(_ / "src")

// set the Scala test source directory to be <base>/test
scalaSource in Test <<= baseDirectory(_ / "test")

