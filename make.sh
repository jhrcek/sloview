#!/bin/bash
elm make frontend/Main.elm --output=static/js/app.js
stack build
cp .stack-work/install/x86_64-linux/lts-6.10/7.10.3/bin/run-sloview .
