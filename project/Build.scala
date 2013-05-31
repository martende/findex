import sbt._
import Keys._

object Build extends Build {

  lazy val project = Project("root", file(".")).settings(

	name := "findex",

	version := "1.0",

	scalaVersion := "2.10.0",

	// libraryDependencies += "optional" %% "optional" % "0.2"
	libraryDependencies ++= Seq (
		"com.thoughtworks.paranamer" % "paranamer" % "2.2.1",
		"org.apache.commons" % "commons-io" % "1.3.2",
            	"com.github.scala-incubator.io" %% "scala-io-core" % "0.4.2",
		"com.github.scala-incubator.io" %% "scala-io-file" % "0.4.2",
		"org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
	),


    	// enable forking in run
	fork in run := true,

	// we need to add the runtime classpath as a "-cp" argument to the `javaOptions in run`, otherwise caliper
	// will not see the right classpath and die with a ConfigurationException
	javaOptions in run <++= (fullClasspath in Runtime) map { cp => Seq("-cp", sbt.Build.data(cp).mkString(":")) }

  )
}
