#!/bin/bash
elm make --warn frontend/Main.elm --output=static/js/app.js
stack install --test --local-bin-path .
