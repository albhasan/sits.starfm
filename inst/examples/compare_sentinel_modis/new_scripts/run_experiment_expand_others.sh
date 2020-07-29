#!/bin/bash

script_dir="/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/scripts_bricks/new_scripts"

##---- Build bricks ----
#
## Build VRTs from the bands.
#"${script_dir}"/01_build_bricks/./build_vrt_base.sh bilinear /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/raster/sentinel_L2A/*/GRANULE/*/IMG_DATA/*/*.jp2
#
## Build VRTs from the FMASKs.
#"${script_dir}"/01_build_bricks/./build_vrt_base.sh nearest /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/raster/sentinel_L1C/S2A_MSIL1C_201*/GRANULE/*/FMASK_DATA/*.tif
#
##NOTE: build_index.sh already uses parallel to run.
#parallel -j 1 "${script_dir}"/01_build_bricks/./build_index.sh ::: evi_10m ndmi_10m ndvi_10m savi_10m
#
##NOTE: build_index.sh already uses parallel to run.
#parallel -j 1 "${script_dir}"/01_build_bricks/./mask_images.sh ::: B02_10m B03_10m B04_10m B08_10m B11_10m B12_10m B8A_10m evi_10m ndmi_10m ndvi_10m savi_10m
#
#parallel -j 8 "${script_dir}"/01_build_bricks/./build_vrt_brick.sh ::: B02_10m B03_10m B04_10m B08_10m B11_10m B12_10m B8A_10m evi_10m ndmi_10m ndvi_10m savi_10m
#
## NOTE: Build VRTs for masked bands and indeces
#parallel -j 8 "${script_dir}"/01_build_bricks/./build_vrt_brick.sh ::: B02_masked_10m B03_masked_10m B04_masked_10m B08_masked_10m B11_masked_10m B12_masked_10m B8A_masked_10m evi_masked_10m ndmi_masked_10m ndvi_masked_10m savi_masked_10m
#
#parallel -j 8 "${script_dir}"/01_build_bricks/./build_tif_brick.sh ::: B02_10m B03_10m B04_10m B08_10m B11_10m B12_10m B8A_10m evi_10m ndmi_10m ndvi_10m savi_10m
#
## NOTE: Build TIFs for masked bands and indeces
#parallel -j 8 "${script_dir}"/01_build_bricks/./build_tif_brick.sh ::: B02_masked_10m B03_masked_10m B04_masked_10m B08_masked_10m B11_masked_10m B12_masked_10m B8A_masked_10m evi_masked_10m ndmi_masked_10m ndvi_masked_10m savi_masked_10m
#
##---- Interpolate masked bricks ----
#
##TODO: Make paths relative.
#Rscript "${script_dir}"/01_build_bricks/interp_sentinel-2.R approx /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_masked_10m.tif  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_approx_10m.tif 
#Rscript "${script_dir}"/01_build_bricks/interp_sentinel-2.R approx /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_masked_10m.tif  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_approx_10m.tif
#Rscript "${script_dir}"/01_build_bricks/interp_sentinel-2.R approx /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_masked_10m.tif  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_approx_10m.tif
#Rscript "${script_dir}"/01_build_bricks/interp_sentinel-2.R approx /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_masked_10m.tif  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_approx_10m.tif
#Rscript "${script_dir}"/01_build_bricks/interp_sentinel-2.R approx /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_masked_10m.tif  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_approx_10m.tif
#Rscript "${script_dir}"/01_build_bricks/interp_sentinel-2.R approx /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_masked_10m.tif  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_approx_10m.tif
#Rscript "${script_dir}"/01_build_bricks/interp_sentinel-2.R approx /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_masked_10m.tif  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_approx_10m.tif
#Rscript "${script_dir}"/01_build_bricks/interp_sentinel-2.R approx /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_masked_10m.tif  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_approx_10m.tif
#Rscript "${script_dir}"/01_build_bricks/interp_sentinel-2.R approx /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_masked_10m.tif /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_approx_10m.tif
#Rscript "${script_dir}"/01_build_bricks/interp_sentinel-2.R approx /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_masked_10m.tif /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_approx_10m.tif
#Rscript "${script_dir}"/01_build_bricks/interp_sentinel-2.R approx /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_masked_10m.tif /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_approx_10m.tif
#
##---- Bricks of the first two images ----
#
#/usr/bin/gdalbuildvrt -b 1 -b 2 /disks/d3/brick_sentinel2/first_2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_approx_10m.vrt  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_approx_10m.tif 
#/usr/bin/gdalbuildvrt -b 1 -b 2 /disks/d3/brick_sentinel2/first_2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_approx_10m.vrt  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_approx_10m.tif 
#/usr/bin/gdalbuildvrt -b 1 -b 2 /disks/d3/brick_sentinel2/first_2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_approx_10m.vrt  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_approx_10m.tif 
#/usr/bin/gdalbuildvrt -b 1 -b 2 /disks/d3/brick_sentinel2/first_2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_approx_10m.vrt  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_approx_10m.tif 
#/usr/bin/gdalbuildvrt -b 1 -b 2 /disks/d3/brick_sentinel2/first_2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_approx_10m.vrt  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_approx_10m.tif 
#/usr/bin/gdalbuildvrt -b 1 -b 2 /disks/d3/brick_sentinel2/first_2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_approx_10m.vrt  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_approx_10m.tif 
#/usr/bin/gdalbuildvrt -b 1 -b 2 /disks/d3/brick_sentinel2/first_2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_approx_10m.vrt  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_approx_10m.tif 
#/usr/bin/gdalbuildvrt -b 1 -b 2 /disks/d3/brick_sentinel2/first_2/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_approx_10m.vrt  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_approx_10m.tif 
#/usr/bin/gdalbuildvrt -b 1 -b 2 /disks/d3/brick_sentinel2/first_2/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_approx_10m.vrt /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_approx_10m.tif
#/usr/bin/gdalbuildvrt -b 1 -b 2 /disks/d3/brick_sentinel2/first_2/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_approx_10m.vrt /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_approx_10m.tif
#/usr/bin/gdalbuildvrt -b 1 -b 2 /disks/d3/brick_sentinel2/first_2/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_approx_10m.vrt /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_approx_10m.tif
#
##---- Install SITS ----
#
#"${script_dir}"/other/install_sits.R
#
##---- Get time series ----
#
#"${script_dir}"/02_get_time_series/get_time_series.R /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/samples_all_bands.csv approx /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/samples_A_approx.rds & # approx_samples_all_bands_csv.rds &
#sleep 10
#"${script_dir}"/02_get_time_series/get_time_series.R /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/samples_indices.csv   approx /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/samples_B_approx.rds # approx_samples_indices_csv.rds
#
#"${script_dir}"/02_get_time_series/get_time_series.R /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/samples_all_bands.csv raw /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/samples_A_raw.rds & # raw_samples_all_bands_csv.rds &
#sleep 10
#"${script_dir}"/02_get_time_series/get_time_series.R /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/samples_indices.csv   raw /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/samples_B_raw.rds # raw_samples_indices_csv.rds

#"${script_dir}"/02_get_time_series/create_samples_2_labels.R
"${script_dir}"/02_get_time_series/create_samples_3_labels.R

#---- Compute K-Folds ----

#"${script_dir}"/03_kfolds/k-folds_analysis.R /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation_other/samples_B_approx_3l.rds /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/plot/kfold_approx_other

#---- Classify bricks ----

brick_dir="/disks/d3/brick_sentinel2"
samples_B_approx_5l="/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation_other/samples_B_approx_5l.rds"
five_labels="Deforestatio,Forest,NatNonForest,NonForest,Pasture"
bands="blue,bnir,green,nnir,red,swir1,swir2"
indices="evi,ndmi,ndvi"
version="009"
out_base_dir="/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results9_other"

# Classify full bricks.
"${script_dir}"/04_classify/classify_bricks.R approx "${brick_dir}" "${samples_B_approx_5l}" "${five_labels}" "${bands}"   "${version}" "${out_base_dir}"
"${script_dir}"/04_classify/classify_bricks.R approx "${brick_dir}" "${samples_B_approx_5l}" "${five_labels}" "${indices}" "${version}" "${out_base_dir}"

##---- Post-processing ----
#
## Apply the rules from a partial classification using 2 labels to a full classification using 3 labels.
##"${script_dir}"/05_post-processing/apply_rules.R "${out_base_dir}" "rules_2_3"
#
##---- Validation ----
#
## Run validation using samples A.
#result_bands_dir=/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results9/approx/samples_B_approx_3l/blue-bnir-green-nnir-red-swir1-swir2/Deforestatio-Forest-NonForest/random-forest_1000
#result_bands_tif="${result_bands_dir}"/postprocessing_first.tif
#result_bands_label="${result_bands_dir}"/sits_labels.txt
#
#result_indices_dir=/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results9/approx/samples_B_approx_3l/evi-ndmi-ndvi/Deforestatio-Forest-NonForest/random-forest_1000/
#result_indices_tif="${results_bands_dir}"/postprocessing_first.tif
#result_indices_label="${results_bands_dir}"/sits_labels.txt
#echo $result_indices_dir
#echo $result_indices_tif
#echo $result_indices_label
#
## TODO: validate_results.R isn't taking parameters. Besides it was deprecated.
#"${script_dir}"/06_validation/validate_results.R "${result_bands_tif}" "${result_bands_label}" "${samples_A_approx_3l}"
#"${script_dir}"/06_validation/validate_results.R 
#
#exit 0
