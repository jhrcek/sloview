#!/bin/bash
elm make frontend/Main.elm --output=static/js/app.js
stack install --local-bin-path .
