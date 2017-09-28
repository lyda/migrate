#!/bin/bash
#
# bfg.sh
# Copyright (C) 2017 Kevin Lyda <kevin@phrye.com>

set -e

BFG_URL=http://repo1.maven.org/maven2/com/madgag/bfg/1.12.15/bfg-1.12.15.jar

base_dir=$(readlink -f $(dirname $0))

if [[ ! -f $base_dir/bfg.jar ]]; then
  wget -O $base_dir/bfg.jar $BFG_URL
fi

java -jar $base_dir/bfg.jar "$@"
