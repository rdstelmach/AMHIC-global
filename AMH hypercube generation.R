library(lhs)
library(xlsx)
library(mc2d)

# read in cleaned datasets (from AMH data processing file)----------------------
file_path_clean_data <- 
  "Model definition files/AUTO GENERATED cleaned data frames/"
read_in_data <- function (file_name) {
  read.csv(paste0(file_path_clean_data, file_name, ".csv"))
}

days_lost            <- read_in_data("excess_days_lost_to_illness")
starting_data        <- read_in_data("starting_data")
wb_and_who           <- read_in_data("wb_and_who_countries")

input_files <- list.files("Model definition files/")
input_files <- grep("amh_", input_files, value = TRUE)
input_files <- grep("standard", input_files, value = TRUE)
input_files <- grep("all_interventions", input_files, value = TRUE)

for (i in 1:length(input_files)) {
  input_file <- input_files[i]
  intervention <- gsub("amh_model_inputs_|_standard.xlsx", "", input_file)
  
  excel_sheet_with_data <- paste0("Model definition files/", input_file)
  
  model_params <- read.xlsx(excel_sheet_with_data, 
                            sheetName = "Parameters")
  
  model_params <- subset(model_params, psa_distribution != "")
  
  difficult_params <- subset(model_params, grepl("[a-z]", value))
  simple_params <- subset(model_params, 
                          !(parameter %in% difficult_params$parameter))
  simple_params_pert <- subset(simple_params, psa_distribution == "PERT")
  simple_params_ln <- subset(simple_params, psa_distribution == "lognormal")
  
  get_simple_pert <- function (par_name) {
    qpert(hypercube[, par_name], 
          min = as.numeric(model_params$low[model_params$parameter == par_name]), 
          mode = as.numeric(model_params$value[model_params$parameter == par_name]), 
          max = as.numeric(model_params$high[model_params$parameter == par_name]))
  }
  
  set.seed(24601)
  hypercube <- data.frame(randomLHS(100, nrow(model_params) + 3))
  
  names(hypercube) <- c("country", "sex", "age_init",
                        model_params$parameter)
  
  hypercube$country <- floor(hypercube$country * nrow(wb_and_who)) + 1
  hypercube$country <- wb_and_who$country[hypercube$country]
  
  hypercube$sex <- floor(hypercube$sex * 2)
  hypercube$sex <- ifelse(hypercube$sex == 1, "Female", "Male")
  
  hypercube$age_init <- (hypercube$age_init * 10) + 10
  
  # simple variables only have numeric values
  get_simple_pert <- function (par_name) {
    qpert(hypercube[, par_name], 
          min = as.numeric(model_params$low[model_params$parameter == par_name]), 
          mode = as.numeric(model_params$value[model_params$parameter == par_name]), 
          max = as.numeric(model_params$high[model_params$parameter == par_name]))
  }
  
  hypercube[, simple_params_pert$parameter] <- 
    sapply(simple_params_pert$parameter, get_simple_pert)
  
  get_simple_lognormal <- function (par_name) {
    base_val = as.numeric(model_params$value[model_params$parameter == par_name])
    min_val = as.numeric(model_params$low[model_params$parameter == par_name])
    max_val = as.numeric(model_params$high[model_params$parameter == par_name])
    
    qlnorm(hypercube[, par_name],
           meanlog = log(base_val),
           sdlog = (log(max_val) - log(min_val)) / 3.96)
  }
  
  hypercube[, simple_params_ln$parameter] <-
    sapply(simple_params_ln$parameter, get_simple_lognormal)
  
  # complex variables vary by country
  ## Pr_miss_work is the same for all interventions
  miss_work <- function (quantile_value, country_name, condition_name) {
    base_val = days_lost$p_miss_work[days_lost$country == country_name &
                                     days_lost$condition == condition_name]
    min_val = days_lost$p_miss_work_min[days_lost$country == country_name &
                                       days_lost$condition == condition_name]
    max_val = days_lost$p_miss_work_max[days_lost$country == country_name &
                                       days_lost$condition == condition_name]
    
    qpert(quantile_value, min = min_val, mode = base_val, max = max_val)
  }
  
  hypercube$Pr_reduce_emp_bipolar <- mapply(miss_work,
    hypercube$Pr_reduce_productivity_bipolar,
    hypercube$country, 
    MoreArgs = list(condition_name = "bipolar"))
  hypercube$Pr_miss_work_anxiety <- mapply(miss_work,
    hypercube$Pr_miss_work_anxiety,
    hypercube$country, 
    MoreArgs = list(condition_name = "anxiety"))
  hypercube$Pr_miss_work_depression <- mapply(miss_work,
    hypercube$Pr_miss_work_depression,
    hypercube$country, 
    MoreArgs = list(condition_name = "depression"))
  
  ## Probability of receiving anx/dep treatment varies by intervention
  # interventions for suicide and bipolar don't need this
  if (intervention == "all_interventions") {
    internet_treatment <- function (quantile_value, country_name) {
      country_internet = 
        starting_data$Pr_access_internet[starting_data$country == country_name][1]
      
      base_val = ifelse(country_internet < (2/3), country_internet / 2, 1/3)
      min_val = 0
      max_val = country_internet
      
      qpert(quantile_value, min = min_val, mode = base_val, max = max_val)
    }
    
    in_person_treatment <- function (quantile_value, country_name) {
      country_internet = 
        starting_data$Pr_access_internet[starting_data$country == country_name][1]
      
      base_val = ifelse(country_internet < (2/3), 1 - country_internet, 1/3)
      min_val = 0
      max_val = 1
      
      qpert(quantile_value, min = min_val, mode = base_val, max = max_val)
    }
    
    hypercube$Pr_receive_anxiety_treatment_internet <- 
      mapply(internet_treatment,
             hypercube$Pr_receive_anxiety_treatment_internet, 
             hypercube$country)
    hypercube$Pr_receive_anxiety_treatment_individual <- 
      mapply(internet_treatment,
             hypercube$Pr_receive_anxiety_treatment_individual, 
             hypercube$country)
    hypercube$Pr_receive_depression_mild_treatment_internet <- 
      mapply(internet_treatment,
             hypercube$Pr_receive_depression_mild_treatment_internet,
             hypercube$country)
    hypercube$Pr_receive_depression_mild_treatment_individual <- 
      mapply(internet_treatment,
             hypercube$Pr_receive_depression_mild_treatment_individual, 
             hypercube$country)
    hypercube$Pr_receive_anxiety_treatment_group <- 
      mapply(in_person_treatment,
             hypercube$Pr_receive_anxiety_treatment_group, 
             hypercube$country)
    hypercube$Pr_receive_depression_mild_treatment_group <- 
      mapply(in_person_treatment,
             hypercube$Pr_receive_depression_mild_treatment_group, 
             hypercube$country)
  } else if (intervention %in% c("dep_mild_trt_internet_self",
                                 "dep_mild_trt_internet_individual")) {
    internet_treatment_sole <- function (quantile_value, country_name) {
      country_internet = 
        starting_data$Pr_access_internet[starting_data$country == country_name][1]
      
      base_val = ifelse(country_internet < 0.2, country_internet, 0.2)
      min_val = ifelse(country_internet < 0.1, country_internet, 0.1)
      max_val = ifelse(country_internet < 0.5, country_internet, 0.5)
      
      qpert(quantile_value, min = min_val, mode = base_val, max = max_val)
    }
    
    hypercube$Pr_receive_depression_mild_treatment_any <- 
      mapply(internet_treatment_sole,
             hypercube$Pr_receive_depression_mild_treatment_any, 
             hypercube$country)
  } else if (intervention %in% c("anx_trt_internet_self",
                                 "anx_trt_internet_individual")) {
    internet_treatment_sole <- function (quantile_value, country_name) {
      country_internet = 
        starting_data$Pr_access_internet[starting_data$country == country_name][1]
      
      base_val = ifelse(country_internet < 0.2, country_internet, 0.2)
      min_val = ifelse(country_internet < 0.1, country_internet, 0.1)
      max_val = ifelse(country_internet < 0.5, country_internet, 0.5)
      
      qpert(quantile_value, min = min_val, mode = base_val, max = max_val)
    }
    
    hypercube$Pr_receive_anxiety_treatment_any <- 
      mapply(internet_treatment_sole,
             hypercube$Pr_receive_anxiety_treatment_any, 
             hypercube$country)
  }
  
  # need single quotes around the character values
  hypercube$sex <- paste0("'", hypercube$sex, "'")
  hypercube$country <- paste0("'", hypercube$country, "'")
  
  #write.csv(hypercube, 
  #          paste0("Model output files/Hypercube samples/hc_",
  #                 intervention,
  #                 ".csv"),
  #          row.names = FALSE)
}