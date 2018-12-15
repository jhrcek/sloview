#!/bin/bash
elm make frontend/Main.elm --optimize --output=static/js/app.js && \
stack install --pedantic --test --local-bin-path .
