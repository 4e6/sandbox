import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences._

// settings

val commonSettings = Seq(
  version := "0.1-SNAPSHOT",
  scalaVersion := "2.11.4",
  scalacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-feature",
    "-unchecked",
    "-deprecation",
    "-Xlint",
    "-language:_"))

val formattingSettings = scalariformSettings ++ Seq(
  ScalariformKeys.preferences := ScalariformKeys.preferences.value
    .setPreference(RewriteArrowSymbols, true)
    .setPreference(AlignParameters, true)
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(DoubleIndentClassDeclaration, true))

val noPublishingSettings = Seq(
  publishArtifact := false,
  publishTo := None)

// projects

lazy val core = project
  .settings(commonSettings: _*)
  .settings(formattingSettings: _*)
  .settings(
    initialCommands in console := "import misc.examples._",
    libraryDependencies ++= Seq(
      "com.chuusai"    %% "shapeless"   % "2.0.0",
      "org.scalaz"     %% "scalaz-core" % "7.1.0",
      "org.spire-math" %% "spire"       % "0.9.0",
      "org.scalacheck" %% "scalacheck"  % "1.12.1" % "test"))

lazy val benchmark = project
  .dependsOn(core)
  .settings(commonSettings: _*)
  .settings(formattingSettings: _*)
  .settings(jmhSettings: _*)
  .settings(noPublishingSettings: _*)

lazy val root = Project("sandbox", file("."))
  .aggregate(core, benchmark)
  .settings(commonSettings: _*)
  .settings(noPublishingSettings: _*)
