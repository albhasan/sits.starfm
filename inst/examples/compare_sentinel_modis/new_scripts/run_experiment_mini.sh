#!/bin/bash

script_dir="/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/scripts_bricks/new_scripts"

#---- Build bricks ----

# Build VRTs of a smaller region of the bricks.
"${script_dir}"/01_build_bricks/./build_minibrick.sh /disks/d3/brick_sentinel2 /disks/d3/brick_sentinel2/mini "269631.8475151583552361 8882286.6762695331126451 278186.6487185038859025 8890289.1282500829547644"

#---- Process samples ----

# Create samples C
"${script_dir}"/02_get_time_series/create_samples_C.R
"${script_dir}"/02_get_time_series/create_samples_3_labels.R

#---- Classify brikcs ----

mini_brick_dir="/disks/d3/brick_sentinel2/mini"
samples_A_approx="/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/samples_A_approx.rds"
samples_B_approx="/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/samples_B_approx.rds"
samples_C_approx="/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/samples_C_approx.rds"
samples_A_approx_3l="/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/samples_A_approx_3l.rds"
samples_B_approx_3l="/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/samples_B_approx_3l.rds"
samples_C_approx_3l="/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/samples_C_approx_3l.rds"
five_labels="Deforestatio,Forest,NatNonForest,NonForest,Pasture"
three_labels="Deforestatio,Forest,NonForest"
bands="blue,bnir,green,nnir,red,swir1,swir2"
indices="evi,ndmi,ndvi"
version="007"
out_base_dir="/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results7"

"${script_dir}"/04_classify/04_classify_bricks.R approx "${mini_brick_dir}" "${samples_B_approx}"    "${five_labels}"  "${bands}"   "${version}" "${out_base_dir}"
"${script_dir}"/04_classify/04_classify_bricks.R approx "${mini_brick_dir}" "${samples_B_approx_3l}" "${three_labels}" "${bands}"   "${version}" "${out_base_dir}"
"${script_dir}"/04_classify/04_classify_bricks.R raw    "${mini_brick_dir}" "${samples_B_approx}"    "${five_labels}"  "${bands}"   "${version}" "${out_base_dir}"
"${script_dir}"/04_classify/04_classify_bricks.R raw    "${mini_brick_dir}" "${samples_B_approx_3l}" "${three_labels}" "${bands}"   "${version}" "${out_base_dir}"
#-----------------------
"${script_dir}"/04_classify/04_classify_bricks.R approx "${mini_brick_dir}" "${samples_A_approx}"    "${five_labels}"  "${indices}" "${version}" "${out_base_dir}"
"${script_dir}"/04_classify/04_classify_bricks.R approx "${mini_brick_dir}" "${samples_A_approx_3l}" "${three_labels}" "${indices}" "${version}" "${out_base_dir}"
"${script_dir}"/04_classify/04_classify_bricks.R raw    "${mini_brick_dir}" "${samples_A_approx}"    "${five_labels}"  "${indices}" "${version}" "${out_base_dir}"
"${script_dir}"/04_classify/04_classify_bricks.R raw    "${mini_brick_dir}" "${samples_A_approx_3l}" "${three_labels}" "${indices}" "${version}" "${out_base_dir}"
#-----------------------
"${script_dir}"/04_classify/04_classify_bricks.R approx "${mini_brick_dir}" "${samples_C_approx}"    "${five_labels}"  "${bands}"   "${version}" "${out_base_dir}"
"${script_dir}"/04_classify/04_classify_bricks.R approx "${mini_brick_dir}" "${samples_C_approx_3l}" "${three_labels}" "${bands}"   "${version}" "${out_base_dir}"
"${script_dir}"/04_classify/04_classify_bricks.R raw    "${mini_brick_dir}" "${samples_C_approx}"    "${five_labels}"  "${bands}"   "${version}" "${out_base_dir}"
"${script_dir}"/04_classify/04_classify_bricks.R raw    "${mini_brick_dir}" "${samples_C_approx_3l}" "${three_labels}" "${bands}"   "${version}" "${out_base_dir}"
#-----------------------
"${script_dir}"/04_classify/04_classify_bricks.R approx "${mini_brick_dir}" "${samples_C_approx}"    "${five_labels}"  "${indices}" "${version}" "${out_base_dir}"
"${script_dir}"/04_classify/04_classify_bricks.R approx "${mini_brick_dir}" "${samples_C_approx_3l}" "${three_labels}" "${indices}" "${version}" "${out_base_dir}"
"${script_dir}"/04_classify/04_classify_bricks.R raw    "${mini_brick_dir}" "${samples_C_approx}"    "${five_labels}"  "${indices}" "${version}" "${out_base_dir}"
"${script_dir}"/04_classify/04_classify_bricks.R raw    "${mini_brick_dir}" "${samples_C_approx_3l}" "${three_labels}" "${indices}" "${version}" "${out_base_dir}"

