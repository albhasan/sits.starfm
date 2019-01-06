#!/bin/sh

# Subset a brick of the dry season

# brick_scene = "225063"
# brick_scene = "233067"
# brick_scene = "226064"
# brick_year = 2014
# brick_year = 2015
# brick_year = 2016
# brick_year = 2017

#brick_pathrow="225063"
#brick_pathrow="233067"
brick_pathrow="226064"
#brick_path="/home/alber/shared/brick_interp"
brick_path="/home/alber/shared/brick"
parallel gdal_translate -b 20 -b 21 -b 22 -b 23 {} {.}_fewclouds.tif ::: $(find $brick_path -type f -name "*$brick_pathrow*") 

