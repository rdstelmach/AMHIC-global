library(parallel)
library(doParallel)
library(foreach)

interventions <- list.files("/share/storage/AMHIC/Model output files/PSA output")

cl <- parallel::makeCluster(detectCores() - 1)
doParallel::registerDoParallel(cl)

psa_runs_by_inter <- foreach (i = 1:length(interventions)) %dopar% {
    do.call(c, lapply(paste0("/share/storage/AMHIC/Model output files/PSA output/",
                  interventions[i]), list.files))
}

parallel::stopCluster(cl)

cl <- parallel::makeCluster(detectCores() - 1)
doParallel::registerDoParallel(cl)

psa_dat_list <- foreach (i = 1:length(interventions)) %dopar% {
    do.call(rbind, lapply(paste0("/share/storage/AMHIC/Model output files/PSA output/",
                          interventions[i], "/", psa_runs_by_inter[[i]]), read.csv))
}

parallel::stopCluster(cl)

for (i in 1:length(interventions)) {
  psa_dat_list[[i]]$intervention <- interventions[i]
  psa_dat_list[[i]] <- subset(psa_dat_list[[i]], !(is.na(roi)) & !(is.na(cost_per_daly_averted)))
}

names(psa_dat_list) <- interventions
lapply(psa_dat_list, nrow)


cl <- parallel::makeCluster(detectCores() - 1)
doParallel::registerDoParallel(cl)

foreach(i = 1:length(interventions)) %dopar% {
write.csv(psa_dat_list[[i]], row.names = FALSE,
          paste0("/share/storage/AMHIC/Model output files/Combined PSA output/PSA_",
                 interventions[i], ".csv"))
}

parallel::stopCluster(cl)

cl <- parallel::makeCluster(detectCores() - 1)
doParallel::registerDoParallel(cl)
