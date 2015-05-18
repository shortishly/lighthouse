# Lighthouse

Lighthouse is a small, fast, modular event push server written in
[Erlang](http://erlang.org).

The server presents a hierarchy of event source topics wrapped in the
[ATOM](http://www.ietf.org/rfc/rfc4287) syndication format. This
hierarchy may be used to model chat rooms, lobbies, etc, which can be
navigated by a client to reach a suitable event source.

## Building

Lighthouse uses [erlang.mk](https://github.com/ninenines/erlang.mk). To build run:

```
make
```

[![Build Status](https://travis-ci.org/shortishly/lighthouse.svg)](https://travis-ci.org/shortishly/lighthouse)



## Quick Start

Bring up an instance of
[elasticsearch](https://registry.hub.docker.com/_/elasticsearch/) and
also [kibana](https://www.elastic.co/products/kibana), which
[lighthouse](https://github.com/shortishly/lighthouse) uses for
reporting.

```sh
docker-compose up
```

Leave the above running and start a new terminal with:

```sh
./start-dev.sh
```

By default [lighthouse](https://github.com/shortishly/lighthouse) logs
some usage statstics to the elasticsearch instance started above every 30 seconds:

```sh
open http://$(boot2docker ip):5601/
```

If you are not using [boot2docker](http://boot2docker.io) replace the
above with the IP address of your docker host.

Tell kibana about the indexes that elastic has created by navigating
to "Settings" use the default index name or pattern as "logstash-*"
and select "timestamp" as the time-field name. If at first nothing
appears, wait 30 seconds or so, and click "refresh fields".

### Pushing some example data

In this example, we will imagine a collection of temperature sensors
that are in located in a house.  To push an event to the server, we
need 3 things: the **topic**, some **data** and an **event**. In this
example we will use the topic of **floors/ground/rooms/kitchen**, an
event of **temperature**, and data of **21**:

```sh
curl --include --data data=22 --data event=temperature --data topic=floors/ground/rooms/kitchen http://127.0.0.1:8181/events
```

In another terminal connect to the stream for the kitchen topic:

```sh
curl -i http://127.0.0.1:8181/topic/floors/ground/rooms/kitchen
```

Push some further temperature changes:


```sh
curl --include --data data=23 --data event=temperature --data topic=floors/ground/rooms/kitchen http://127.0.0.1:8181/events
curl --include --data data=24 --data event=temperature --data topic=floors/ground/rooms/kitchen http://127.0.0.1:8181/events
```

Observe the events that are emitted on the kitchen topic.


You can use your browser to look at the topic structure that has been
created, using the following URL:

```sh
curl -s http://127.0.0.1:8181/topics|xmllint --format -
```

This URL presents the topic hierarchy using the ATOM syndication
format. You can follow the structure in your browser, and also
navigate and view an event stream.
