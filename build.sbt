name := "erro-handling"

version := "0.1.0"

organization := "com.jobo"

scalaVersion := "2.12.8"

scalacOptions in ThisBuild ++= Seq( "-language:_", "-Ypartial-unification", "-Xfatal-warnings", "-unchecked" )

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")

val Http4sVersion  = "0.20.0-M1"
val LogbackVersion = "1.2.3"
val CirceVersion   = "0.10.0-M2"
val DoobieVersion  = "0.6.0"

libraryDependencies ++= Seq(
  "org.scalatest"  %% "scalatest" % "3.0.5" % "test",
  "ch.qos.logback" % "logback-classic"      % LogbackVersion,
  "org.http4s"     %% "http4s-blaze-server" % Http4sVersion,
  "org.http4s"     %% "http4s-circe"        % Http4sVersion,
  "org.http4s"     %% "http4s-dsl"          % Http4sVersion,
  "com.olegpy"     %% "meow-mtl"            % "0.1.1",
  "com.github.pureconfig" %% "pureconfig" % "0.10.0"
) ++ Circe ++ Doobie

val Circe = Seq(
  "io.circe" %% "circe-core"    % CirceVersion,
  "io.circe" %% "circe-generic" % CirceVersion,
  "io.circe" %% "circe-parser"  % CirceVersion
)

val Doobie = Seq(
  "org.tpolecat" %% "doobie-core"      % DoobieVersion,
  // And add any of these as needed
   "org.tpolecat" %% "doobie-h2"        % DoobieVersion,          // H2 driver 1.4.197 + type mappings.
   "org.tpolecat" %% "doobie-hikari"    % DoobieVersion,          // HikariCP transactor.
   "org.tpolecat" %% "doobie-postgres"  % DoobieVersion,          // Postgres driver 42.2.5 + type mappings.
  // "org.tpolecat" %% "doobie-specs2"    % "0.6.0" % "test", // Specs2 support for typechecking statements.
  "org.tpolecat" %% "doobie-scalatest" % DoobieVersion % "test", // ScalaTest support for typechecking statements
  "org.flywaydb" % "flyway-core" % "5.2.0"
)

