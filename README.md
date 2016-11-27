# Simple EAP / WildFly server.log viewer
A simple web application for analysis of [EAP](http://developers.redhat.com/products/eap/overview/) / [WildFly](http://wildfly.org/) server.log files.
Back end is written in Haskell, using [snap framework](http://snapframework.com/), front end is written in [Elm](http://elm-lang.org/).

## Features & TODOs
- [x] Upload server.log files for analysis
- [x] Filter server.log messages based on log level
- [x] Configure which message fields are displayed (date, log level, logger, thread, payload and stack trace)
- [x] Configure date format of message timestamps
- [x] Count number of messages per log level
- [ ] Aggregated view of repeated messages and stack traces

## How to run it
To build the binary from the source you will need [stack](https://docs.haskellstack.org/en/stable/README/) and [elm](http://elm-lang.org/install) installed.
The build can be performed by running `./make.sh`.
Running the resulting binary `./run-sloview` starts the web application on `http://0.0.0.0:8000/` which you can view using your browser.
