addSbtPlugin("ch.epfl.scala" % "sbt-bloop"    % "1.4.0-RC1")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.3.0")
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.2.17")

resolvers += "Akka library repository".at("https://repo.akka.io/maven")
addSbtPlugin("com.lightbend.akka.grpc" % "sbt-akka-grpc" % "2.4.0")