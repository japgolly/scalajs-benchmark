ThisBuild / semanticdbEnabled := true

ThisBuild / semanticdbVersion := "4.4.6"

ThisBuild / scalafixDependencies ++= Seq(
  "com.github.liancheng" %% "organize-imports" % "0.4.4"
)
