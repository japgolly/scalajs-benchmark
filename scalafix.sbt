ThisBuild / semanticdbEnabled := true

ThisBuild / semanticdbVersion := "4.3.18"

ThisBuild / scalafixDependencies ++= Seq(
  "com.github.liancheng" %% "organize-imports" % "0.4.1"
)
