#!/bin/bash
elm make frontend/Main.elm --output=static/js/app.js
stack build
cp .stack-work/install/x86_64-linux/lts-7.0/8.0.1/bin/run-sloview .
