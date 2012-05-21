#!/bin/sh
cd `dirname $0`
exec erl +K true -pa ebin -pa deps/*/ebin -boot start_sasl -s rb -s sse -config dev.config -name sse
