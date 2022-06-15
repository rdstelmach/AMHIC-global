# AMHIC-global
Code for : Stelmach R, Kocher EL, Kataria I et al. The global return on investment from preventing and treating adolescent mental disorders and suicide: a modelling study. BMJ Global Health 2022;0:e007759. doi:10.1136/bmjgh-2021-007759

Rachel Stelmach
June 16, 2022

# Introduction
This is the documentation of the files used for the global model adolescent mental health investment case. It flows in the order that files are used for the model.

# Abbreviations
* AMH: adolescent mental health
* PSA: probabalistic sensitivity analysis

# Contents

* The folder "Raw model input files" contains data exactly as downloaded from external databases. See "AMH data processing.Rmd" for citations.
* The code document "AMH data processing.Rmd" reads in the data from "Raw model input files", cleans and processes them, and saves the resulting files to "Model definition files/AUTO GENERATED cleaned data frames". DO NOT alter the files in "AUTO GENERATED cleaned data frames" by hand; all edits must be made in the "AMH data processing.Rmd" code.
* Within the "Model definition files" folder, the file "amh_model_inputs_all_interventions_global_final.xlsx" contains all of the user-defined model parameters. This is the *only* version that is hand-updated.
	* "Parameters" contains all of the model parameters. Use '' to enclose character values, not "". In order to run properly, if var_b references var_a, var_a must be defined in a previous line. Use SORT_BY column to ensure this sorting happens within the code.
	* "Starting states" defines the names of the states and the number of people starting in each state.
	* "Transition probabilities" defines the probabilities that an individual moves from the "from" state to the "to" state each round.
	* "State values" defines the cost, benefit, and utility associated with each state.
* The code document "Generate model definition files.Rmd" reads in the documentation from "amh_model_inputs_all_interventions_global_final.xlsx" and generates separate spreadsheets for each intervention. It works by first assigning all intervention-associated values to their null value (e.g. intervention population coverage to 0% and risk ratios to 1) to create a "no_interventions" spreadsheet and then adding back each intervention individually with any required edits. The resulting spreadsheets all have the suffix _standard. DO NOT make final edits to the _standard spreadsheets by hand, as those edits will be lost the next time "Generate model definition files.Rmd" is run.
* The code document "AMH model no PSA.R" runs the model and generates the base case results, which are saved to the folder "Model output files/Base case/". In the process, it generates and runs the code chunks within the folder "Model definition files/AUTO GENERATED cleaned model inputs".
* The code document "AMH hypercube generation" generates Latin hypercube samples (LHS) for each intervention and saves them to "Model output files/Hypercube samples".
* The folder "AMH_PSA_model_code" includes one .R document with model code for each intervention. When run, it references a single random row of the appropriate LHS for the intervention, runs the model with those parameter values, and saves a two-line .csv with the resulting ROI and cost per DALY averted to the folder "Model output files/PSA output/PSA output/[intervention_name]".
* The code document "run_parallel_PSA.R" runs the code saved in "AMH_PSA_model_code". Please note that the parallelization will only work on a Linux computer; the code should still run on Windows, but will only run in sequence.
* The code document "combine_PSA.R" loops over all folders within "Model output files/PSA output/" to combine the 2-line .csvs into one results file per intervention. These combined output files are saved to the folder "Model output files/PSA output/Combined PSA output/".
* The code document "AMH analyze PSA.Rmd" reads in the combined PSA output files and generates descriptive statistics.
