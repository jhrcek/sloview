# Simple server.log viewer
A simple web application for analysis of EAP / wildfly server.log files.
Backend is written in Haskell, using [snap framework](http://snapframework.com/),
frontend is written in [Elm](http://elm-lang.org/).

## Features & TODOs
- [x] Uploading server.log files for analysis
- [x] Filtering of server.log messages based on log level
- [x] Configure which messages fields are displayed (date, log level, logger, thread, payload and stack trace)
- [x] Configure which message fields are displayed
- [x] Per log level aggregation of message counts
- [ ] Detection & aggregation of stack traces in messages

## How to run it
To run this you will need [stack](https://docs.haskellstack.org/en/stable/README/) and [elm](http://elm-lang.org/install) installed.
Then just run `./make.sh` to build it, `./run-sloview` to start http server and visit `http://0.0.0.0:8000/` using your browser.
