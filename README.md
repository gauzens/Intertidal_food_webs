# Intertidal_food_webs
Code for simulating species dynamics and data analysis of "Biodiversity of intertidal food webs in response to warming across latitudes" NCC 2020

## R_code:
Contains the R scripts for analysing the data:
* PCAs.R: runs the multivariate analysis from fig. 2a and 2b
* TL_BM_and_ratios.R: Allometric relationhsips from fig. 2c
* do_figure_2.R
* stab_results.R: Analysis of the 4 degrees warming (fig. 3)
* plots_complete_gradient.R: analysis of the large warming gradient (0-50)
* NicheVsEmpiricalTopology.R: analyses of the structural differences between the empirical webs and the synthetic webs (fig. 5)
* dependance_to_K.R: effect of K on models output (SI V)
* get_differences.R: space for temperature substitution (SI VI)


## Simulations:

Contains the elements needed to run the dynamic model:
### codes:
* binzer_2016_interference.py: specifications of the model
* db_binzer_2016_interference.c: equations of the model
* food_webs.py: libaray of functions for food webs
* dynamic_nichesHPC.py, dynamic_threadHPC and expNW_nicheBM.py: define the experiments and run the simulations.

### launchers: 
Bash scripts to start processes on the HPC

### web_list:
the set of natural networks used
