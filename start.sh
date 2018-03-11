#!/usr/bin/env bash

set -euxo pipefail

hg pull -u
jbuilder build \
	 nava/main.exe \
	 viewer/main.bc.js \
&& _build/default/nava/main.exe
