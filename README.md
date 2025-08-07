<<<<<<< HEAD
This code was created to estimate direct expenditure on periodontitis as a proportion of dental expenditure

The final code can be found in GBD_cleaned.R, or in a html version (from RMarkdown) in GBD_cleaned.html

GBD_working_draft.R involves the initial attempts at model development

=======
Monte Carlo Model Cleaned.R

This is the main model, which creates output files for each scenario: low, middle and high - these represent varying levels of dental utilisation

It involves:
  A function for running the monte carlo model
  A code for combining input files
  A function to call the first function, to loop the monte carlo model across the three scenarios (low,mid,high) and to chunk the countries into slices of 100 in case
  there is insufficient memory to run the code
  Code to run the function

Each scenario will have 3 output files: 
  country_combined where the cost for each country is aggregated; 
  country_sev_combined in which the cost for each combination of country and disease severity is aggregated;
  procedure_combined, in which the cost for each combination of country and procedure is aggregated

------------------------------------------------------------------------------------------------------------------------------------------------------

Prediction_data_wrangling.R

This file is to filter through all the scenarios, pick one for each country, and create global estimates

We first select the appropriate scenario based on dental expenditure - the selected model should not exceed 60% of dental expenditure
  In cases where even the low scenario exceeds 60% dental expenditure, the low scenario is selected

Selection of the appropriate scenario then allows the costs most representative of current expenditure to be selected, including
  Country level costs
  Country+severity costs 
  Procedure+country costsm
  
This allows the global total periodontitis spending to be calculated, along with costs for each procedure and costs for each disease severity group

------------------------------------------------------------------------------------------------------------------------------------------------------

Prediction_data_wrangling_retain_prev_scenario.R

This is for sensitivity analysis - to model changes in maintenance frequency while retaining the selected scenario from the base case analysis
  Otherwise, this may lead to downgrading of the selected scenario (as country costs will increase with increased maintenance) which leads to underestimated
  changes in global expenditure


------------------------------------------------------------------------------------------------------------------------------------------------------

Data Visualisation.R

This is for creation of data visualisations.
  
>>>>>>> 66db44b (Readme updated)

