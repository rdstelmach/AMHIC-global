library(parallel)
library(doParallel)
library(foreach)

cl <- parallel::makeCluster(detectCores() - 1)
doParallel::registerDoParallel(cl)

foreach (i = 1:5787) %dopar% {
  source("/share/storage/AMHIC/AMH_PSA_model_code/model_bip_all_trt.R")
}

parallel::stopCluster(cl)
