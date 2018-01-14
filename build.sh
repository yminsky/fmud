#!/usr/bin/env bash

jbuilder build \
    mud/nava.bc \
    viewer/main.bc.js \
    mud/server.bc \
     mud/test_client.bc \
     --dev
