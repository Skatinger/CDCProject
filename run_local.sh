#!/bin/bash

# GRIDSIZE=($(jq -r '.gridsize' /config/config.json))
# GRASS_COUNT=($(jq -r '.grasscount' /config/config.json))
# RABBIT_COUNT==($(jq -r '.rabbitcount' /config/config.json))
# FOX_COUNT==($(jq -r '.foxcount' /config/config.json))


# echo "gridsize: ${GRIDSIZE[@]}"






# works
cd ebin
erl -pa ebin -eval "master:start(5,5,5,5)." -eval "init:stop()"

