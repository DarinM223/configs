name := "functional-scala-starter"

version := "0.1"

scalaVersion := "2.12.7"

resolvers += Resolver.sonatypeRepo("releases")

val monocleVersion = "1.5.0"
val derivingVersion = "1.0.0"

libraryDependencies ++= Seq(
  compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.2.4"),

  compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8"),
  // if your project uses multiple Scala versions, use this for cross building
  compilerPlugin("org.spire-math" % "kind-projector" % "0.9.8" cross CrossVersion.binary),

  // the @deriving and @xderiving plugin and macro
  "org.scalaz" %% "deriving-macro" % derivingVersion,
  compilerPlugin("org.scalaz" %% "deriving-plugin" % derivingVersion),
  // the scalaz-deriving Altz / Decidablez / Deriving API and macros
  "org.scalaz" %% "scalaz-deriving" % derivingVersion,
  // instances for Show and Arbitrary
  "org.scalaz" %% "scalaz-deriving-magnolia" % derivingVersion,
  "org.scalaz" %% "scalaz-deriving-scalacheck" % derivingVersion,
  // shapeless alternatives to Deriving. See below for additional actions.
  "org.scalaz" %% "scalaz-deriving-shapeless" % derivingVersion,

  "com.github.mpilquist" %% "simulacrum" % "0.13.0",
  "io.estatico" %% "newtype" % "0.4.2",

  "com.github.julien-truffaut" %% "monocle-core" % monocleVersion,
  "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion,
  "com.github.julien-truffaut" %% "monocle-law" % monocleVersion,

  "org.scalaz" %% "scalaz-zio" % "0.3.1",
  "org.scalaz" %% "scalaz-core" % "7.2.26"
)