# Lighthouse

Lighthouse is a small, fast, modular event push server written in
[Erlang](http://erlang.org).

The server presents a hierarchy of event source topics wrapped in the
[ATOM](http://www.ietf.org/rfc/rfc4287) syndication format. This
hierarchy may be used to model chat rooms, lobbies, etc, which can be
navigated by a client to reach a suitable event source.

## Building

EKC uses [erlang.mk](https://github.com/ninenines/erlang.mk). To build run:

```
make
```

[![Build Status](https://travis-ci.org/shortishly/lighthouse.svg)](https://travis-ci.org/shortishly/lighthouse)



## Quick Start

```sh
docker pull shortishly/lighthouse
```


## Starting the server

To start the server:

```sh
(sse home)/start.sh
```

## Pushing some example data

In this example, we will imagine a collection of temperature sensors
that are in located in a house.  To push an event to the server, we
need 3 things: the **topic**, some **data** and an **event**. In this
example we will use the topic of **floors/ground/rooms/kitchen**, an
event of **temperature**, and data of **21**:

```sh
(sse home)/sse-push --data 21 --event temperature --topic floors/ground/rooms/kitchen
```

For the lounge:

```sh
(sse home)/sse-push --data 23 --event temperature --topic floors/ground/rooms/lounge
```

For the 1st floor master bedroom:

```sh
(sse home)/sse-push --data 22 --event temperature --topic floors/first/rooms/master-bedroom
```

You can use your browser to look at the topic structure that has been
created, using the following URL:

`http://localhost:8080/topics`

This URL presents the topic hierarchy using the ATOM syndication
format. You can follow the structure in your browser, and also
navigate and view an event stream.

To listen to events being pushed out by the server:

```sh
(sse home)/sse-listen --topic floors/ground/rooms/kitchen
```

The above command will continue to display events that are pushed to
the topic **floors/ground/rooms/kitchen** until you quit by pressing
'Ctrl-C'. You can test this by running the following command in
another terminal window:

```sh
(sse home)/sse-push --data 20 --event temperature --topic floors/ground/rooms/kitchen
```


sse-push
--------

sse-push is used as a command line interface to push events onto the
SSE server. It takes the following parameters:

* data: the information to be communicated.
* event: ...
* topic: the topic

sse-listen
----------

sse-listen is used as a command line interface to listen to events
that have been pushed onto the SSE server. It takes the following
parameters:

* topic: the topic
* last: the last event that we wish to start from


HTTP endpoints
--------------

The SSE server has the following HTTP endpoints.

* event/push, the sse-push command is a wrapper for this HTTP
  endpoint.
