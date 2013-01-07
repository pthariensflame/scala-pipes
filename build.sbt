name                :=  "scala-pipes"

version             :=  "1.0.0-SNAPSHOT"

crossScalaVersions  :=  Seq("2.9.2", "2.10.0")

libraryDependencies ++= Seq(
  "org.scalaz"     %% "scalaz-core"        % "7.0.0-M7"
  ,
  "org.scalaz"     %% "scalaz-scalacheck-binding" % "7.0.0-M7" % "test"
  ,
  "org.scalacheck" %% "scalacheck"         % "1.10.0"          % "test"
)
