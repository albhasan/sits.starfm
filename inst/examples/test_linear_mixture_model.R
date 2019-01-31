library(tidyverse)
library(devtools)

setwd("/home/alber/Documents/data/experiments/l8mod-fusion/Rpackage/sits.starfm")
devtools::load_all()

pix_fores  <- c(201, 487, 237, 4642, 1829, 620)
pix_defor  <- c(492, 746, 878, 2458, 2875, 1784)
pix_water  <- c(249, 305, 183,  121,   49,  26)

end_member_mt <- as.matrix(END_MEMBERS_LANDSAT_8[3:5])
names(pix_fores) <- names(pix_defor) <- names(pix_water) <- rownames(end_member_mt) <- END_MEMBERS_LANDSAT_8$band

pix_fores %*% end_member_mt 
pix_defor %*% end_member_mt
pix_water %*% end_member_mt

warning("pix_fores: substrate > vegetation")
warning("pix_water: vegetation && substrate > dark")

# (1 * 6) %*% (6 * 3) = (1 * 3)
