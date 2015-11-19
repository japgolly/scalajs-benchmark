#!/bin/bash
cd "$(dirname "$0")" || exit 1

depS=demo/target/scala-2.11/demo-jsdeps.min.js
outS=demo/target/output.js

depT=res/jsdeps.js
outT=res/main.js
deps="$depT $outT"

rm -f $deps

sbt clean demo/fullOptJS \
&& echo \
&& cp -v $depS $depT \
&& cp -v $outS $outT \
&& git add $deps \
&& git st \
&& echo "git commit -m 'Refresh gh-pages' && git push && git checkout master" && echo

