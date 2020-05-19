#!/bin/bash

if [ "${1}" == "" ]; then
    echo "Usage: ${0} B02_10m B03_10m B04_10m B08_10m B11_10m B12_10m B8A_10m"
    exit 1
fi

parallel -j 8 ./build_vrt_brick.sh ::: "$@" 

