#!/bin/bash

if [ "${1}" == "" ]; then
    echo "Usage: ${0} evi_10m ndmi_10m ndvi_10m savi_10m"
    exit 1
fi

parallel -j 1 ./build_index.sh ::: "$@" 

