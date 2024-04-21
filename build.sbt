val scala3Version = "3.3.1"

lazy val scalameta = Seq("org.scalameta" %% "munit" % "0.7.29" % Test)
lazy val scalatest = Seq(
  "org.scalatest"     %% "scalatest"       % "3.2.15"   % Test,
  "org.scalatestplus" %% "scalacheck-1-17" % "3.2.15.0" % Test
)
lazy val scalaXml  = Seq("org.scala-lang.modules" %% "scala-xml" % "2.2.0")
lazy val fastParse = Seq("com.lihaoyi" %% "fastparse" % "3.0.2")
lazy val breeze = Seq(
  "org.scalanlp" %% "breeze"     % "2.1.0",
  "org.scalanlp" %% "breeze-viz" % "2.1.0"
)
lazy val commonsText = Seq("org.apache.commons" % "commons-text" % "1.11.0")
lazy val scopt       = Seq("com.github.scopt" %% "scopt" % "4.1.0")
lazy val a2lParser   = Seq("net.alenzen.a2l" % "a2lparser" % "2.6.2-local")
lazy val graphviz    = Seq("guru.nidi" % "graphviz-java" % "0.18.1")

lazy val root = project
  .in(file("."))
  .settings(
    name         := "xdfbinext",
    version      := "0.1",
    scalaVersion := scala3Version,
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= scalameta ++ scalaXml ++ scalatest ++ fastParse ++ breeze ++ commonsText ++ scopt ++ a2lParser ++ graphviz,
    assembly / mainClass := Some("net.jtownson.xdfbinext.MapCompare"),
    assemblyJarName      := "xbc.jar",
    assemblyMergeStrategy := {
      case PathList("module-info.class") =>
        MergeStrategy.last
      case path if path.endsWith("/module-info.class") =>
        MergeStrategy.last
      case _ =>
        MergeStrategy.first
    }
  )
