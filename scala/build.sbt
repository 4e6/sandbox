import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences._

// settings

val commonSettings = Seq(
  version := "0.1-SNAPSHOT",
  scalaVersion := "2.11.7",
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
  .settings(commonSettings, formattingSettings)
  .settings(
    initialCommands in console :=
      """import misc.examples._
         import misc.trees._""",
    libraryDependencies ++= Seq(
      "com.chuusai"    %% "shapeless"   % "2.2.5",
      "org.scalaz"     %% "scalaz-core" % "7.1.3",
      "org.spire-math" %% "spire"       % "0.10.1",
      "org.scalacheck" %% "scalacheck"  % "1.12.4" % Test))

lazy val benchmark = project
  .dependsOn(core)
  .enablePlugins(JmhPlugin)
  .settings(commonSettings, formattingSettings, noPublishingSettings)

lazy val root = Project("sandbox", file("."))
  .aggregate(core, benchmark)
  .settings(commonSettings, noPublishingSettings)
