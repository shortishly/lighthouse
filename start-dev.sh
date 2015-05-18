#!/usr/bin/env bash
# Copyright (c) 2012-2015 Peter Morgan <peter.james.morgan@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
cd $(dirname $0)
APP=$(bin/app)

export ELASTICSEARCH_PORT_9200_TCP_ADDR=$(boot2docker ip)
export ELASTICSEARCH_PORT_9200_TCP_PORT=9200

export STATS_SAMPLER_TIMEOUT=30000

exec erl \
     +K true \
     -boot start_sasl \
     -config dev.config \
     -name ${APP} \
     -pa deps/*/ebin \
     -pa ebin \
     -s ${APP} \
     -s rb

