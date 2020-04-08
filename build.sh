#!/bin/sh
# Build server and GUI

stack setup
stack build

cd ui
elm make --yes src/Acquire.elm --output=acquire.js 


