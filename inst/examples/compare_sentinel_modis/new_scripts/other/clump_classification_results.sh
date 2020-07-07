#!/bin/bash
# Compute the clumped polygons from the classification results.
# NOTE: Use user scidb on esensing-006.

sudo gdal_polygonize.py -8 "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results9/approx/samples_B_approx_3l/blue-bnir-green-nnir-red-swir1-swir2/Deforestatio-Forest-NonForest/random-forest_1000/sentinel-bricks_probs_class_bayesian_2018_8_2019_7_009.tif" -f GPKG /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results9/approx/samples_B_approx_3l/blue-bnir-green-nnir-red-swir1-swir2/Deforestatio-Forest-NonForest/random-forest_1000/bands_clumped.gpkg 

sudo gdal_polygonize.py -8 "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results9/approx/samples_B_approx_3l/evi-ndmi-ndvi/Deforestatio-Forest-NonForest/random-forest_1000/sentinel-bricks_probs_class_bayesian_2018_8_2019_7_009.tif" -f GPKG /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results9/approx/samples_B_approx_3l/evi-ndmi-ndvi/Deforestatio-Forest-NonForest/random-forest_1000/indices_clumped.gpkg 

exit 0
