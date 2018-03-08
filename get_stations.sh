#!/bin/sh

# Download bike stations info

wget -qO- http://nextbike.net/maps/nextbike-live.xml | xmllint --xpath '//city[@name="Warszawa"]' - > nextbike-live.xml
