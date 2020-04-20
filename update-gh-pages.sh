#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")"

vers=(
  2.12.11
  2.13.1
)

for v in "${vers[@]}"; do
  echo "--> $v"
  rm -rf res/scala-$v demo/target
  sbt ++$v clean demo/fastOptJS demo/fullOptJS
  mkdir res/scala-$v
  cp -v demo/target/scala-*/demo-* res/scala-$v
  git add res/scala-$v
  echo
done
git st
echo "git commit -m 'Refresh gh-pages' && git push && git checkout master" && echo
