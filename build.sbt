// (5) shadow sbt-scalajs' crossProject and CrossType until Scala.js 1.0.0 is released
import sbtcrossproject.{crossProject, CrossType}

val scalaV = "2.13.3"
lazy val jQueryV = "3.4.1"
lazy val semanticV = "2.4.1"
lazy val elastic4sVersion = "7.3.4"
lazy val zioVersion = "1.0.0-RC21-2"
scalaVersion := "2.13.3"

val circeLibs = Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser",
  "io.circe" %% "circe-optics",
  "io.circe" %% "circe-generic-extras"
).map(_ % "0.12.0")

lazy val cmdserver = (project in file("cmdserver")).settings(
  scalaVersion := scalaV,
  assemblyJarName in assembly := "cmd-server.jar",
  mainClass in assembly := Some("apps.Application"),
  libraryDependencies ++= Seq(
    "com.vmunier" %% "scalajs-scripts" % "1.1.4",
    guice,
    filters,
    ws,
    "commons-io" % "commons-io" % "2.7",
    "dev.zio" %% "zio"          % zioVersion,
    "dev.zio" %% "zio-test"     % zioVersion % "test",
    "dev.zio" %% "zio-test-sbt" % zioVersion % "test",
    "com.typesafe.akka" %% "akka-testkit" % "2.6.5" % Test,
    "com.typesafe.akka" %% "akka-stream-testkit" % "2.6.5" % Test,
    "org.scalatestplus.play" %% "scalatestplus-play" % "5.1.0" % Test,
    "org.apache.httpcomponents" % "httpcore" % "4.4.9",
    "org.apache.httpcomponents" % "httpclient" % "4.5.5",
    "org.json" % "json" % "20180130",
    "com.arangodb" % "arangodb-java-driver" % "6.6.3",
    "org.apache.pdfbox" % "pdfbox" % "2.0.20",
    "org.apache.pdfbox" % "pdfbox-tools" % "2.0.20",
    "org.apache.tika" % "tika" % "1.24.1",
    "org.apache.tika" % "tika-parsers" % "1.24.1",
    "com.lihaoyi" %% "ammonite-ops" % "1.8.1",
    "net.codingwell" %% "scala-guice" % "4.2.9",
    "com.iheart" %% "ficus" % "1.4.7",
    "com.enragedginger" %% "akka-quartz-scheduler" % "1.8.4-akka-2.6.x",
    "com.monitorjbl" % "xlsx-streamer" % "2.1.0",
    "com.joestelmach" % "natty" % "0.13",
    //"com.lihaoyi" %% "ujson" % "0.7.1",
    "com.lihaoyi" %% "requests" % "0.5.1",
    "org.jsoup" % "jsoup" % "1.12.1",
    "net.rationalminds" % "DateParser" % "1.4",
    "org.seleniumhq.selenium" % "selenium-firefox-driver" % "3.141.59",
    "org.seleniumhq.selenium" % "selenium-chrome-driver" % "3.141.59",
    "org.elasticsearch.client" % "transport" % "7.8.0",
    "com.github.seratch" %% "awscala-iam" % "0.8.+",
    "com.github.seratch" %% "awscala-dynamodb" % "0.8.+",
    "com.github.seratch" %% "awscala-s3" % "0.8.+",
    "com.github.seratch" %% "awscala-sqs" % "0.8.+",
    "com.github.seratch" %% "awscala-stepfunctions" % "0.8.+",
    "org.awaitility" % "awaitility" % "3.0.0" % Test
  ) ++ circeLibs
).dependsOn(sharedJvm)

lazy val server = (project in file("server")).settings(
  scalaVersion := scalaV,
  scalaJSProjects := Seq(client),
  pipelineStages in Assets := Seq(scalaJSPipeline),
  pipelineStages := Seq(digest, gzip),
  // triggers scalaJSPipeline when using compile or continuous compilation
  compile in Compile := ((compile in Compile) dependsOn scalaJSPipeline).value,
  libraryDependencies ++= Seq(
    "com.vmunier" %% "scalajs-scripts" % "1.1.4",
    guice,
    filters,
    ws
    // webjars for Semantic-UI
    , "org.webjars" %% "webjars-play" % "2.8.0"
    //, "org.webjars" % "Semantic-UI" % semanticV
    , "org.webjars" % "jquery" % jQueryV
    ,
    "commons-io" % "commons-io" % "2.7",
    "com.digitaltangible" %% "play-guard" % "2.5.0",
    "dev.zio" %% "zio"          % zioVersion,
    "dev.zio" %% "zio-test"     % zioVersion % "test",
    "dev.zio" %% "zio-test-sbt" % zioVersion % "test",
    "com.typesafe.akka" %% "akka-testkit" % "2.6.5" % Test,
    "com.typesafe.akka" %% "akka-stream-testkit" % "2.6.5" % Test,
    "org.scalatestplus.play" %% "scalatestplus-play" % "5.1.0" % Test,
    "org.apache.httpcomponents" % "httpcore" % "4.4.9",
    "org.apache.httpcomponents" % "httpclient" % "4.5.5",
    "org.json" % "json" % "20180130",
    "com.arangodb" % "arangodb-java-driver" % "6.4.1",
    "org.apache.pdfbox" % "pdfbox" % "2.0.17",
    "org.apache.pdfbox" % "pdfbox-tools" % "2.0.17",
    "org.apache.tika" % "tika" % "1.22",
    "org.apache.tika" % "tika-parsers" % "1.22",
    "com.lihaoyi" %% "ammonite-ops" % "1.8.1",
    "net.codingwell" %% "scala-guice" % "4.2.9",
    "com.iheart" %% "ficus" % "1.4.7",
    "com.typesafe.play" %% "play-mailer" % "8.0.1",
    "com.typesafe.play" %% "play-mailer-guice" % "8.0.1",
    "com.enragedginger" %% "akka-quartz-scheduler" % "1.8.4-akka-2.6.x",
    //"org.apache.poi" % "poi-ooxml" % "4.1.1",
    "com.monitorjbl" % "xlsx-streamer" % "2.1.0",
    "com.joestelmach" % "natty" % "0.13",
    //"com.lihaoyi" %% "ujson" % "0.7.1",
    "com.lihaoyi" %% "requests" % "0.5.1",
    "org.jsoup" % "jsoup" % "1.12.1",
    "net.rationalminds" % "DateParser" % "1.4",
    "org.seleniumhq.selenium" % "selenium-firefox-driver" % "3.141.59",
    "org.seleniumhq.selenium" % "selenium-chrome-driver" % "3.141.59",
    "org.elasticsearch.client" % "transport" % "7.8.0",
    "com.github.seratch" %% "awscala-iam" % "0.8.+",
    "com.github.seratch" %% "awscala-dynamodb" % "0.8.+",
    //"com.github.seratch" %% "awscala-emr" % "0.8.+",
    //"com.github.seratch" %% "awscala-redshift" % "0.8.+",
    "com.github.seratch" %% "awscala-s3" % "0.8.+",
    //"com.github.seratch" %% "awscala-simpledb" % "0.8.+",
    "com.github.seratch" %% "awscala-sqs" % "0.8.+",
    //"com.github.seratch" %% "awscala-sts" % "0.8.+",
    "com.github.seratch" %% "awscala-stepfunctions" % "0.8.+",
//"com.sksamuel.elastic4s" %% "elastic4s-core" % elastic4sVersion,
    //"com.sksamuel.elastic4s" %% "elastic4s-client-esjava" % elastic4sVersion,
    //"com.sksamuel.elastic4s" %% "elastic4s-http-streams" % elastic4sVersion,
    //"com.sksamuel.elastic4s" %% "elastic4s-testkit" % elastic4sVersion % "test",
    //"com.sksamuel.elastic4s" %% "elastic4s-json-play" % elastic4sVersion,
    //"io.megl" %% "zio-elasticsearch-core" % "0.0.6-SNAPSHOT",
    //"io.megl" %% "elasticsearch-admin" % "0.0.6-SNAPSHOT",
    //"io.megl" %% "elasticsearch-cat" % "0.0.6-SNAPSHOT",
    //"io.megl" %% "zio-elasticsearch-sttp" % "0.0.6-SNAPSHOT",
    //"io.kamon" %% "kamon-bundle" % "2.1.0",
    //"io.kamon" %% "kamon-apm-reporter" % "2.1.0",
    "org.awaitility" % "awaitility" % "3.0.0" % Test
  ) ++ circeLibs,
  //assemblyJarName in assembly := "sajoli-server-api.jar",
  // to have routing also in ScalaJS
  // Create a map of versioned assets, replacing the empty versioned.js
  DigestKeys.indexPath := Some("javascripts/versioned.js"),
  // Assign the asset index to a global versioned var
  DigestKeys.indexWriter ~= { writer => index => s"var versioned = ${writer(index)};" }

).enablePlugins(PlayScala)
  .dependsOn(sharedJvm, cmdserver)

lazy val client = (project in file("client")).settings(
  scalaVersion := scalaV,
  scalaJSUseMainModuleInitializer := false,
  //scalaJSMainModuleInitializer := Some(...),
  //scalacOptions ++= Seq("-Xmax-classfile-name","78"),
  scalaJSUseMainModuleInitializer in Test := false,
  //addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  jsDependencies ++= Seq(
    "org.webjars" % "jquery" % jQueryV / "jquery.js" minified "jquery.min.js",
    "org.webjars" % "Semantic-UI" % semanticV / "semantic.js" minified "semantic.min.js" dependsOn "jquery.js"
  ),
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "1.0.0",
    "org.scala-lang.modules" %% "scala-xml" % "1.3.0",
    "com.typesafe.play" %%% "play-json" % "2.8.1",
    "org.lrng.binding" %%% "html" % "1.0.3",
    "com.thoughtworks.binding" %%% "futurebinding" % "12.0.0",
    //"fr.hmil" %%% "roshttp" % "2.2.4",
    // java.time supprot for ScalaJS
    "io.github.cquiroz" %%% "scala-java-time-tzdb" % "2.0.0",
    // jquery support for ScalaJS
    "be.doeraene" %%% "scalajs-jquery" % "0.9.5"
  )
).enablePlugins(ScalaJSWeb).
  dependsOn(sharedJs)

lazy val shared = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(scalaVersion := scalaV
    , libraryDependencies ++= Seq(
      "org.julienrf" %%% "play-json-derived-codecs" % "7.0.0",
      "io.github.cquiroz" %%% "scala-java-time" % "2.0.0"
      // logging lib that also works with ScalaJS
      //"biz.enef" %%% "slogging" % "0.6.0"
    ))
  .jsSettings(/* ... */) // defined in sbt-scalajs-crossproject
  .jvmSettings(
    libraryDependencies ++= Seq(
            "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided")
  )
  .jsConfigure(_.enablePlugins(ScalaJSWeb))

lazy val sharedJvm = shared.jvm
lazy val sharedJs = shared.js

scalafmtOnCompile := true
// loads the server project at sbt startup
//onLoad in Global := (Command.process("project server", _: State)) compose (onLoad in Global).value
onLoad in Global := (onLoad in Global).value andThen { s: State =>
    "project server" :: s
}
