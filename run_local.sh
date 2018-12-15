#!/bin/bash

# used when simulation should be started with parameters from config files
#GRIDSIZE=($(jq -r '.gridsize' config/config.json))
#GRASS_COUNT=($(jq -r '.grasscount' config/config.json))
#RABBIT_COUNT=($(jq -r '.rabbitcount' config/config.json))
#FOX_COUNT=($(jq -r '.foxcount' config/config.json))


erl -pa ebin deps/**/ebin -eval "master:start(7)." -eval "init:stop()"
