name := "prism"
version := "0.8.1"
scalaVersion := "2.12.6"
organization := "org.encryfoundation"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "1.0.0",
  "com.google.guava" % "guava" % "21.0",
  "org.scorexfoundation" %% "scrypto" % "2.1.2",
  "org.scodec" %% "scodec-core" % "1.10.3",
  "org.scalatest" %% "scalatest" % "3.0.3" % "test"
)

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
licenses in ThisBuild := Seq("GNU GPL 3.0" -> url("https://github.com/EncryFoundation/PrismLang/blob/master/LICENSE"))
homepage in ThisBuild := Some(url("https://github.com/EncryFoundation/PrismLang"))
publishMavenStyle in ThisBuild := true
publishArtifact in Test := false
publishTo in ThisBuild :=
  Some(if (isSnapshot.value) Opts.resolver.sonatypeSnapshots else Opts.resolver.sonatypeStaging)
pomExtra in ThisBuild :=
  <scm>
    <url>git@github.com:EncryFoundation/PrismLang.git</url>
    <connection>scm:git:git@github.com:EncryFoundation/PrismLang.git</connection>
  </scm>
  <developers>
    <developer>
      <id>Oskin1</id>
      <name>Ilya Oskin</name>
    </developer>
    <developer>
      <id>ugulavaGeorge</id>
      <name>Ugulava George</name>
    </developer>
    <developer>
      <id>kapinuss</id>
      <name>Stanislav Kapinus</name>
    </developer>
  </developers>