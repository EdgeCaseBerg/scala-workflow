import scalariform.formatter.preferences._

organization := "com.github.edgecaseberg"

name := "workflow"

version := "0.0.0-SNAPSHOT" 

scalaVersion := "2.11.7"

libraryDependencies ++= { 
	val anormVersion = "2.3.+"
	Seq(
		"com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
		"org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
	)
}

scalariformPreferences := scalariformPreferences.value
	.setPreference(DoubleIndentClassDeclaration, true)
	.setPreference(PreserveDanglingCloseParenthesis, true)
	.setPreference(AlignParameters, false)
	.setPreference(IndentWithTabs, true)
	.setPreference(MultilineScaladocCommentsStartOnFirstLine, true)