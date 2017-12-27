name := "Broodwar Bot, Scala!"

version := "1.0"

scalaVersion := "2.12.4"

unmanagedBase := baseDirectory.value / "lib"
unmanagedJars in Compile := (baseDirectory.value ** "*.jar").classpath

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.18"
libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.2.18"