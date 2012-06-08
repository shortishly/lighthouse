Erlang Server-Side Events
=========================

Erlang SSE [dl] -- 2012.

[dl]: https://github.com/shortishly/erlang-sse.git

Introduction
------------

Erlang SSE is an server side implementation of the W3C working draft
[Server-Sent Events][sse], written in [Erlang][erlang], using the
[Cowboy][cowboy], [JSX][jsx] and [MDNS][mdns] libraries.

The server presents a hierarchy of event source topics wrapped in the
[ATOM][atom] syndication format. This hierarchy may be used to model
chat rooms, lobbies, etc, which can be navigated by a client to reach
a suitable event source.

[sse]: http://www.w3.org/TR/2012/WD-eventsource-20120426/
[erlang]: http://www.erlang.org/
[cowboy]: https://github.com/extend/cowboy.git
[jsx]: https://github.com/talentdeficit/jsx.git
[mdns]: https://github.com/shortishly/erlang-mdns.git
[atom]: http://www.ietf.org/rfc/rfc4287

Quick Start
-----------

To start the server:

(sse home)/start.sh

In this example, we will imagine a collection of temperature sensors
that are in located in a house.  To push an event to the server, we
need 3 things: the **topic**, some **data** and an **event**. In this
example we will use the topic of **floors/ground/rooms/kitchen**, an
event of **temperature**, and data of **21**:

(sse home)/sse-push --data 21 --event temperature --topic floors/ground/rooms/kitchen

For the lounge:

(sse home)/sse-push --data 23 --event temperature --topic floors/ground/rooms/lounge

For the 1st floor master bedroom:

(sse home)/sse-push --data 22 --event temperature --topic floors/first/rooms/master-bedroom

You can use your browser to look at the topic structure that has been
created, using the following URL:

http://localhost:8080/topics

This URL presents the topic hierarchy using the ATOM syndication
format. You can follow the structure in your browser, and also
navigate and view an event stream.

To listen to events being pushed out by the server:

(sse home)/sse-listen --topic floors/ground/rooms/kitchen

The above command will continue to display events that are pushed to
the topic **floors/ground/rooms/kitchen** until you quit by pressing
'Ctrl-C'. You can test this by running the following command in
another terminal window:

(sse home)/sse-push --data 20 --event temperature --topic floors/ground/rooms/kitchen



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
