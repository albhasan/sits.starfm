#/bin/bash

# parallel -j 2 Rscript build_brick_hls.R --tile {1} --product {2} --from "2016-08-01" --to "2017-09-30" ::: T19LFJ T19LGK T19LGJ T19LDJ T19LDH T19LDL T19LDK T19LEJ T19LEH T19LEL T19LEK T19LFK ::: L30 S30
# parallel -j 2 Rscript build_brick_hls.R --tile {1} --product {2} --from "2016-08-01" --to "2017-09-30" ::: T19LFJ T19LGK T19LGJ T19LDJ T19LDH T19LDL T19LDK T19LEJ T19LEH T19LEL T19LEK T19LFK ::: L30 S30
parallel -j 2 Rscript build_brick_hls.R --tile {1} --product {2} --from "2016-08-01" --to "2017-09-30" ::: T19LFJ T19LGK T19LGJ T19LDJ T19LDH T19LDL T19LDK T19LEJ T19LEH T19LEL T19LEK T19LFK ::: S30
parallel -j 2 Rscript build_brick_hls.R --tile {1} --product {2} --from "2016-08-01" --to "2017-09-30" ::: T19LFJ T19LGK T19LGJ T19LDJ T19LDH T19LDL T19LDK T19LEJ T19LEH T19LEL T19LEK T19LFK ::: S30
#parallel  -j 1 Rscript build_brick_hls.R --tile {1} --product {2} --from "2017-08-01" --to "2018-09-30" ::: T19LFJ ::: S30 

