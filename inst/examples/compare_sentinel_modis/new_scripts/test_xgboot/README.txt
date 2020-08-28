# Change the classification from Randon Forest to XGBOOST.
# Change to the new SITS on R 4

# Build docker image.
sudo docker build -t sits .

# Start a container.
sudo docker run -it -v /disks/d3/brick_sentinel2:/root/data/bricks -v /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation:/root/data/samples -v /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results9_xgboost:/root/data/results -v /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/scripts_bricks/new_scripts/test_xgboot:/root/data/scripts sits /bin/bash

