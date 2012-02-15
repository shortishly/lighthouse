#!/bin/sh
cd `dirname $0`
exec erl -pa ebin -pa deps/*/ebin -s impel
