#!/bin/sh

set -e

if [ ! -e "warsaw_poland.osm.pbf" ]
then
  echo "Downloading latest map (warsaw_poland.osm.pbf)"
  wget https://s3.amazonaws.com/metro-extracts.mapzen.com/warsaw_poland.osm.pbf
fi

stack build
cd graphhopper
./graphhopper.sh build
./graphhopper.sh import ../warsaw_poland.osm.pbf
./graphhopper.sh web ../warsaw_poland.osm.pbf &
sleep 3
cd ..
stack exec precompute
pkill -P $$

set +e
