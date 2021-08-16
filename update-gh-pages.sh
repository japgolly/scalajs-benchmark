#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")"

function go {
  scalaVer=$1
  scalaJsVer=$2
  dir=scala-$scalaVer-sjs-$scalaJsVer
  res=res/$dir

  echo "--> $res"
  rm -rf $res $res.html demo/target
  SCALAJS_VERSION=$scalaJsVer sbt ++$scalaVer clean demo/fastOptJS demo/fullOptJS
  mkdir $res
  cp -v demo/target/scala-*/demo-*.js* $res
  cat > $res.html <<EOB
<!DOCTYPE html>
<html>
<head lang="en">
    <title>scalajs-benchmarks</title>
</head>
<body>
    <div id="body">Loading...</div>
    <script type="text/javascript" src="$dir/demo-jsdeps.min.js"></script>
    <script type="text/javascript" src="$dir/demo-opt.js"></script>
    <script type="text/javascript">main();</script>
</body>
</html>
EOB

  git add $res $res.html
  echo
}

go 2.13.6  1.7.0
go 3.0.1   1.7.0

git status
echo
echo "git commit -m 'Refresh gh-pages' && git push && git checkout master"
