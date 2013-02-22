name                             := "scala-pipes"

version                          := "1.0.0-SNAPSHOT"

scalaVersion                     := "2.10.0"

scalacOptions                   ++=  Seq(
  "-optimize"
  ,
  "-feature"
  ,
  "-unchecked"
  ,
  "-explaintypes"
  ,
  "-deprecation"
  ,
  "-Xlint"
  ,
  "-Xmigration"
  ,
  "-Xverify"
  ,
  "-Ywarn-all"
  ,
  "-Yinline-warnings"
)

scalacOptions in Compile in doc ++=  Seq(
  "-doc-title"        , "scala-pipes"
  ,
  "-doc-version"      , "1.0.0"
  ,
  "-groups"
  ,
  "-implicits"
  ,
  "-implicits-sound-shadowing"
)

libraryDependencies             ++=  Seq(
  "org.scalaz"     %% "scalaz-core"               % "7.0.0-M8"
  ,
  "org.scalaz"     %% "scalaz-scalacheck-binding" % "7.0.0-M8"
  ,
  "org.scalacheck" %% "scalacheck"                % "1.10.0"
)
