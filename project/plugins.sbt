libraryDependencies +=  "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.0"

addSbtPlugin("com.github.gseitz" % "sbt-release"        % "1.0.13")
addSbtPlugin("com.jsuereth"      % "sbt-pgp"            % "2.1.1")
addSbtPlugin("ch.epfl.scala"     % "sbt-scalafix"       % "0.9.28")
addSbtPlugin("org.scala-js"      % "sbt-jsdependencies" % "1.0.2")
addSbtPlugin("org.scala-js"      % "sbt-scalajs"        % "1.3.1")
addSbtPlugin("org.xerial.sbt"    % "sbt-sonatype"       % "3.9.5")
