scalaVersion := "3.3.1"

//enablePlugins(ScalaNativePlugin)

// set to Debug for compilation details (Info is default)
logLevel := Level.Info

// import to add Scala Native options
import scala.scalanative.build._

Compile / run / fork := true

libraryDependencies ++= Seq(
  "com.monovore" %%% "decline" % "2.4.1",
  "com.lihaoyi" %%% "pprint" % "0.8.1",
  "com.lihaoyi" %%% "fansi" % "0.4.0",
  "org.scodec" %%% "scodec-core" % "2.2.2",
  "org.typelevel" %%% "cats-effect" % "3.5.4",
  "com.softwaremill.sttp.client4" %%% "core" % "4.0.0-M11",
  "com.softwaremill.sttp.client4" %%% "cats" % "4.0.0-M11",
  "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % "2.28.4",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.28.4" % "provided"
)

// defaults set with common options shown
//nativeConfig ~= { c =>
//  c.withLTO(LTO.none) // thin
//    .withMode(Mode.debug) // releaseFast
//    .withGC(GC.immix) // commix
//    .withEmbedResources(true)
//}
