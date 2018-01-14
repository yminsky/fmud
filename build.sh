#!/usr/bin/env bash

jbuilder build \
    nava/main.bc \
    viewer/main.bc.js \
    test/server.bc \
    test/client.bc \
    --dev
