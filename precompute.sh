#!/bin/sh

# Download OSM map file
# Build and run graphhopper with provided map
# Precompute paths and save them to json file

set -e

if [ ! -e "Warsaw.osm.pbf" ]
then
  echo "Downloading latest map (Warsaw.osm.pbf)"
  wget http://download.bbbike.org/osm/bbbike/Warsaw/Warsaw.osm.pbf
fi

stack build

cp graphhopper-config.properties graphhopper/config.properties

cd graphhopper
./graphhopper.sh build

# Additional memory for graph building
export JAVA_OPTS="-Xmx2G -Xms2G"
./graphhopper.sh import ../Warsaw.osm.pbf

export JAVA_OPTS="-Xmx1G -Xms1G"
./graphhopper.sh web ../Warsaw.osm.pbf >> /dev/null 2>&1 &

# Launching graphhopper web takes a moment..
sleep 5
cd ..
stack exec precompute > paths.json
pkill -P $$

set +e
