#!/bin/sh
cd `dirname $0`
exec erl -pa ebin -pa deps/*/ebin -boot start_sasl -s rb -s impel -config dev.config -name zeroconf
