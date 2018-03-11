#!/usr/bin/env bash

set -euxo pipefail

eval `opam config env`
jbuilder build \
	 nava/main.exe \
	 viewer/main.bc.js.gz

MAIN=_build/default/nava/main.exe
sudo setcap CAP_NET_BIND_SERVICE=+eip $MAIN
$MAIN -p 80 | tee ~/mud-log.exe7
