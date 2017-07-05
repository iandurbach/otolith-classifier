library(mgcv)

# main functions
source("make_otdata.R")
source("create_nefc_data.R")
source("stdize_by_length.R")

# smaller functions required by create_nefc_data.R
source("nef_set.R")
source("NEF.R")
source("efourier.R")
source("give_vector.R")

# anchovy
load("raw_data/anch_rawdata.RData")
ot <- make_otdata(X_co = anch)
save(ot, file = "proc_data/anch_nefc.RData")
rm(anch,ot)

# sardine
load("raw_data/sard_rawdata.RData")
ot <- make_otdata(X_co = sard)
save(ot, file = "proc_data/sard_nefc.RData")
rm(sard,ot)

# redeye
load("raw_data/redeye_rawdata.RData")
ot <- make_otdata(X_co = redeye)
save(ot, file = "proc_data/redeye_nefc.RData")
rm(redeye,ot)
