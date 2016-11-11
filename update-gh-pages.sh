#!/bin/bash
cd "$(dirname "$0")" || exit 1

set -e

[ -e res ] && rm -rf res
mkdir res
cp -v demo/*.html res
sed -e 's!target/!!' -i res/*.html
sbt +clean +demo/fastOptJS +demo/fullOptJS
echo
for v in 2.11 2.12; do mkdir res/scala-$v; cp -v demo/target/scala-$v/demo-* res/scala-$v; done
git add res
git st
echo "git commit -m 'Refresh gh-pages' && git push && git checkout master" && echo

