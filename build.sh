#!/usr/bin/env bash

jbuilder build \
    --root=$(hg root) \
    nava/main.bc \
    viewer/main.bc.js \
    --dev
