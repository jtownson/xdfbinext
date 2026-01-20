val scala3Version = "3.3.1"

lazy val scalameta = Seq("org.scalameta" %% "munit" % "0.7.29" % Test)
lazy val scalatest = Seq(
  "org.scalatest"     %% "scalatest"       % "3.2.15"   % Test,
  "org.scalatestplus" %% "scalacheck-1-17" % "3.2.18.0" % Test,
  "org.scalatestplus" %% "mockito-5-10"    % "3.2.18.0" % Test
)

lazy val scalaXml  = Seq("org.scala-lang.modules" %% "scala-xml" % "2.2.0")
lazy val fastParse = Seq("com.lihaoyi" %% "fastparse" % "3.0.2")
lazy val breeze = Seq(
  "org.scalanlp" %% "breeze"     % "2.1.0",
  "org.scalanlp" %% "breeze-viz" % "2.1.0"
)
lazy val commonsText = Seq("org.apache.commons" % "commons-text" % "1.11.0")
lazy val scopt       = Seq("com.github.scopt" %% "scopt" % "4.1.0")
lazy val a2lParser   = Seq("net.alenzen.a2l" % "a2lparser-with-dependencies" % "2.7.1-local")
lazy val graphviz    = Seq("guru.nidi" % "graphviz-java" % "0.18.1")
lazy val circe = Seq(
  "io.circe" %% "circe-core"    % "0.14.10",
  "io.circe" %% "circe-generic" % "0.14.10",
  "io.circe" %% "circe-parser"  % "0.14.10"
)
lazy val jfree = Seq(
  "org.jfree" % "jfreechart"    % "1.5.5",
  "org.jfree" % "org.jfree.svg" % "5.0.6"
)
lazy val opencsv = Seq("com.opencsv" % "opencsv" % "5.9")

lazy val root = project
  .in(file("."))
  .settings(
    name         := "xdfbinext",
    version      := "0.1",
    scalaVersion := scala3Version,
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= opencsv ++ jfree ++ circe ++ scalameta ++ scalaXml ++ scalatest ++ fastParse ++ breeze ++ commonsText ++ scopt ++ a2lParser ++ graphviz,
    assembly / mainClass := Some("net.jtownson.xdfbinext.MapCompare"),
    assemblyJarName      := "xbc.jar",
    scalacOptions ++= Seq("-Xmax-inlines", "64"),
    assemblyMergeStrategy := {
      case PathList("module-info.class") =>
        MergeStrategy.last
      case path if path.endsWith("/module-info.class") =>
        MergeStrategy.last
      case _ =>
        MergeStrategy.first
    },
    Test / fork := true,
    Test / javaOptions ++= Seq(
      "-Xms512M",
      "-Xmx3G",
      "-XX:MaxMetaspaceSize=512M",
      "-XX:+UseG1GC"
    )
  )