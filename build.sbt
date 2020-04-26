lazy val commonSettings = Seq(
  name := "open-face-chinese-poker",
  version := "0.1",
  organization := "com.tomo",
  scalaVersion := "2.13.1"
)

lazy val common = (project in file("common")).settings(commonSettings, libraryDependencies ++= deps)
lazy val server = (project in file("server")).settings(commonSettings, libraryDependencies ++= deps).dependsOn(common)
lazy val client = (project in file("client")).settings(commonSettings, libraryDependencies ++= deps).dependsOn(common)

val akkaVersion = "2.6.4"
val akkaHttpVersion = "10.1.11"
val logbackVersion = "1.1.7"
val scalaTestVersion = "3.1.1"
val junitVersion = "4.13"
val deps = Seq (
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-remote" % akkaVersion,
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "com.typesafe.akka" %% "akka-http-core" % akkaHttpVersion,
  //"com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion % Test,
  "com.typesafe.akka" %% "akka-stream-testkit" % akkaVersion % Test,
  // Akka Classic Remote => Netty
  "io.netty" % "netty" % "3.10.6.Final",
// LOGGING
  "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
  "ch.qos.logback" % "logback-classic" % logbackVersion,
  // TESTING
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
  "junit" % "junit" % junitVersion % Test,
  "io.spray" %%  "spray-json" % "1.3.5"
)

fork in run := true