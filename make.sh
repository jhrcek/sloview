#!/bin/bash
elm make Main.elm
stack build
cp .stack-work/install/x86_64-linux/lts-6.10/7.10.3/bin/run-sloview .
