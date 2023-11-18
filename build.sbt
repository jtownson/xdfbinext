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
lazy val scopt    = Seq("com.github.scopt" %% "scopt" % "4.1.0")

lazy val root = project
  .in(file("."))
  .settings(
    name         := "xdfbinext",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= scalameta ++ scalaXml ++ scalatest ++ fastParse ++ breeze ++ commonsText ++ scopt
  )
