### AMH model code only ###
# Rachel Stelmach
# February 10, 2022

# set up------------------------------------------------------------------------
library(heemod)
library(xlsx)
library(tidyverse)
library(stringr)
library(beepr)
library(mc2d)

# read in cleaned datasets (from AMH data processing file)----------------------
file_path_clean_data <- 
  "/share/storage/AMHIC/Model definition files/AUTO GENERATED cleaned data frames/"
read_in_data <- function (file_name) {
  read.csv(paste0(file_path_clean_data, file_name, ".csv"))
}

bg_morbidity         <- read_in_data("background_morbidity")
days_lost            <- read_in_data("excess_days_lost_to_illness")
inc                  <- read_in_data("incidence")
inter_tot            <- read_in_data("intervention_total_costs")
life_table_w_suicide <- read_in_data("life_table_w_suicide")
pr_edu_and_emp_edu   <- read_in_data("pr_edu_and_emp_edu")
starting_data        <- read_in_data("starting_data")
wb_and_who           <- read_in_data("wb_and_who_countries")

# select interventions of interest----------------------------------------------
#### generate model
input_files <- list.files("/share/storage/AMHIC/Model definition files/")
input_files <- grep("amh_", input_files, value = TRUE)
input_files <- grep("standard", input_files, value = TRUE)
input_files <- grep("dep_mild_trt_internet_self", input_files, value = TRUE)

# select outcome(s) of interest-------------------------------------------------
effects_var <- c("benefits", "dalys") # options: benefits, dalys

# run model---------------------------------------------------------------------

file_path_model_define <-
  "/share/storage/AMHIC/Model definition files/AUTO GENERATED cleaned model inputs/"

for (j in 1:length(input_files)) {
  excel_sheet_with_data <- paste0("/share/storage/AMHIC/Model definition files/", 
                                  input_files[j])
  intervention <- gsub("amh_model_inputs_|_standard.xlsx", "", input_files[j])
  
  # generate a parameter input called "param" based on the model inputs files
  # and a random row of the hypercube sample
  model_params <- read.xlsx(excel_sheet_with_data, 
                            sheetName = "Parameters")
  
  hypercube_sample <- 
    read.csv(paste0("/share/storage/AMHIC/Model output files/Hypercube samples/hc_",
                    intervention, ".csv"))
  hypercube_sample_row <- floor(runif(1, min = 0, 
                                      max = nrow(hypercube_sample))) + 1
  hypercube_sample <- hypercube_sample[hypercube_sample_row, ]
  
  model_params <- subset(model_params, !is.na(parameter))
  
  for (n in 1:length(names(hypercube_sample))) {
    model_params$value[model_params$parameter == names(hypercube_sample)[n]] <-
      hypercube_sample[, names(hypercube_sample)[n]]
  }
  
  model_params <- paste(model_params$parameter,
                        model_params$value, 
                        sep = " = ")
  model_params <- paste(model_params, collapse = ", ")

  # in order to run the parameter input, need to save it as a separate file
  # and then call it; can't just run it from here
  write(noquote(paste0("param <- define_parameters(", 
                         model_params, ")")),
        paste0(file_path_model_define, "read_in_parameters.R"))
  
  source(paste0(file_path_model_define, "read_in_parameters.R"))
  
  # generate the list of mh_states for use throughout the model
  # generate the starting distribution of people, called start_dist
  model_starts <- read.xlsx(excel_sheet_with_data,
                            sheetName = "Starting states")
  
  mh_states <- as.character(model_starts$state)
  
  model_starts$state <- paste0("'", model_starts$state, "'")
  model_starts <- paste(model_starts$state, model_starts$n_start, sep = " = ")
  model_starts <- paste(model_starts, collapse = ", ")
  
  write(noquote(paste0("start_dist <- define_init(", 
                       model_starts, ")")), 
        paste0(file_path_model_define, "read_in_starting_states.R"))
  
  source(paste0(file_path_model_define, "read_in_starting_states.R"))
  
  # read in model transitions for both states, "base" and "invest"
  model_trans <- read.xlsx(excel_sheet_with_data,
                           sheetName = "Transition probabilities")
  
  model_trans_base <- subset(model_trans, .model == "base")
  model_trans_base <- rename(model_trans_base, c("prob_base" = "prob"))
  # "invest" should take values from base except where explicitly defined otherwise
  model_trans_invest <- subset(model_trans, .model == "invest")
  model_trans_invest <- rename(model_trans_invest, c("prob_invest" = "prob"))
  # get both transition states in a single data.frame
  model_trans_both <- merge(model_trans_base[, c("from", "to", "prob_base")],
                            model_trans_invest[, c("from", "to", "prob_invest")], 
                            all = TRUE)
  model_trans_both$prob_invest <- ifelse(!is.na(model_trans_both$prob_invest),
                                         as.character(model_trans_both$prob_invest),
                                         as.character(model_trans_both$prob_base))
  
  # keep input order for easier reading
  # also double-checks that state names are consistent with start_dist
  model_trans_both$from <- ordered(model_trans_both$from, mh_states)
  model_trans_both$to <- ordered(model_trans_both$to, mh_states)
  
  # get wide crosstab data.frames; these are convenient for double-checking
  # the transition matrix
  model_trans_base_wide <- reshape2::dcast(model_trans_both, from ~ to, 
                                           value.var = "prob_base")
  model_trans_base_wide <- model_trans_base_wide[, mh_states]
  row.names(model_trans_base_wide) <- mh_states
  
  model_trans_invest_wide <- reshape2::dcast(model_trans_both, from ~ to, 
                                             value.var = "prob_invest")
  model_trans_invest_wide <- model_trans_invest_wide[, mh_states]
  row.names(model_trans_invest_wide) <- mh_states
  
  model_trans_base <- paste(as.vector(t(model_trans_base_wide)), collapse = ", ")
  model_trans_invest <- paste(as.vector(t(model_trans_invest_wide)), collapse = ", ")
  
  trans_written <- noquote(paste0("mh_base <- define_transition(", 
                                  model_trans_base, ", ", "state_names = mh_states" ,")"))
  
  write(noquote(paste0("mh_base <- define_transition(", 
                       model_trans_base, ", ", "state_names = mh_states" ,")")), 
        paste0(file_path_model_define, "read_in_transitions_base.R"))
  
  write(noquote(paste0("mh_invest <- define_transition(", 
                       model_trans_invest, ", ", "state_names = mh_states" ,")")), 
        paste0(file_path_model_define, "read_in_transitions_invest.R"))
  
  source(paste0(file_path_model_define, "read_in_transitions_base.R"))
  source(paste0(file_path_model_define, "read_in_transitions_invest.R"))
  
  model_strats <- read.xlsx(excel_sheet_with_data,
                            sheetName = "State values")
  
  for (m in 1:length(effects_var)) {
    
    if (effects_var[m] == "benefits") {
      model_strats_base <- subset(model_strats, .model == "base")
      model_strats_base <- rename(model_strats_base, c("cost_base" = "cost",
                                                       "benefit_base" = "benefit"))
      # "invest" should take values from base 
      # except where explicitly defined otherwise
      model_strats_invest <- subset(model_strats, .model == "invest")
      model_strats_invest <- rename(model_strats_invest, 
                                    c("cost_invest" = "cost",
                                      "benefit_invest" = "benefit"))
    } else if (effects_var[m] == "dalys") {
      model_strats_base <- subset(model_strats, .model == "base")
      model_strats_base <- rename(model_strats_base, c("cost_base" = "cost",
                                                       "benefit_base" = "dalys"))
      # "invest" should take values from base except 
      # where explicitly defined otherwise
      model_strats_invest <- subset(model_strats, .model == "invest")
      model_strats_invest <- rename(model_strats_invest, 
                                    c("cost_invest" = "cost",
                                      "benefit_invest" = "dalys"))
    }

    # get both transition states in a single data.frame
    model_strats_both <- 
      merge(model_strats_base[, c("state", 
                                  "cost_base", "benefit_base")],
            model_strats_invest[, c("state", 
                                    "cost_invest", "benefit_invest")], 
            all = TRUE)
  
    model_strats_both$cost_invest <- 
      ifelse(!is.na(model_strats_both$cost_invest),
             as.character(model_strats_both$cost_invest),
             as.character(model_strats_both$cost_base))
    model_strats_both$benefit_invest <- 
      ifelse(!is.na(model_strats_both$benefit_invest),
             as.character(model_strats_both$benefit_invest),
             as.character(model_strats_both$benefit_base))
    
    # order states and check they're correct
    # if they're out of order, they don't run correctly!
    model_strats_both$state <- ordered(model_strats_both$state, mh_states)
    model_strats_both <- model_strats_both[order(model_strats_both$state), ]
    
    base_states <- 
      paste0(model_strats_both$state, 
             " = define_state(benefit = discount(", 
             model_strats_both$benefit_base, 
             ", discount_rate_annual, period = 12)",
             ", cost = discount(", model_strats_both$cost_base, 
             ", discount_rate_annual, period = 12))")
    invest_states <- 
      paste0(model_strats_both$state, 
             " = define_state(benefit = discount(", 
             model_strats_both$benefit_invest,
             ", discount_rate_annual, period = 12)",
             ", cost = discount(", model_strats_both$cost_invest, 
             ", discount_rate_annual, period = 12))")
    base_states <- paste(base_states, collapse = ", ")
    invest_states <- paste(invest_states, collapse = ", ")
    
    write(noquote(paste0("strat_base <- define_strategy(transition = mh_base, ", 
                         base_states, ")")), 
          paste0(file_path_model_define, "read_in_state_values_base.R"))
    write(noquote(paste0("strat_invest <- define_strategy(transition = mh_invest, ", 
                         invest_states, ")")), 
          paste0(file_path_model_define, "read_in_state_values_invest.R"))
    
    source(paste0(file_path_model_define, "read_in_state_values_base.R"))
    source(paste0(file_path_model_define, "read_in_state_values_invest.R"))

    # run whole model
    starting_pop_data <- subset(starting_data, 
      country == gsub("'", "", hypercube_sample$country) &
      sex == gsub("'", "", hypercube_sample$sex) &
      age_at_start == floor(hypercube_sample$age_init))
    
    res_mod <- run_model(
      base = strat_base,
      invest = strat_invest,
      parameters = param,
      cycles = 80 * 12,
      cost = cost,
      effect = benefit,
      init = start_dist
    )
    
    model_values <- get_values(res_mod)
    
    if (effects_var[m] == "dalys") {
      model_values_dalys <- subset(model_values, value_names == "benefit")
      model_values_dalys$value_names <- "dalys"
    } else if (effects_var[m] == "benefits") {
      model_values_cb <- model_values
    }
  } # this bracket closes the loop generating the different outcome variables
  
  ## getting the main results out
  model_values_subanalysis <- rbind(model_values_cb, model_values_dalys)
  
  # pull in country information
  res_summary <- model_values_subanalysis %>%
    group_by_(".strategy_names", "value_names") %>%
    summarize(value = sum(value))
  res_summary$hypercube_row <- hypercube_sample_row
  res_summary <- pivot_wider(res_summary, 
                             id_cols = c(hypercube_row), 
                             names_from = c(".strategy_names", "value_names"))
  res_summary$roi <- ((res_summary$invest_benefit - res_summary$base_benefit) - 
                       res_summary$invest_cost) / res_summary$invest_cost
  res_summary$cost_per_daly_averted <- res_summary$invest_cost / 
    (res_summary$base_dalys - res_summary$invest_dalys)
  res_summary <- res_summary[, c("roi", "cost_per_daly_averted", "hypercube_row")]
  res_summary <- cbind(res_summary, hypercube_sample)
  
  ## getting the results out
  write.csv(res_summary, row.names = FALSE,
            paste0("/share/storage/AMHIC/Model output files/PSA output/", 
                   intervention, "/", intervention, "_",
                   str_pad(sample(1:999999999, 1), width = 9, pad = "0"), 
                   ".csv"))
}
