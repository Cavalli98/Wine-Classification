# Wine quality classification in R
## Introduction
This is the final project for the course Bayesian Learning & MonteCarlo simulation attended at polytechnic of Milan during the academic year 2022.\
We have a dataset regarding the ”Vinho Verde” wine. In this dataset, some physical measurements taken from bottles of wine are combined with a sensory judgment about the quality (a vote from 0 to 10) of the wine itself. The objective is to determine which factors influence the quality of wine and to use the physical measurements to correctly classify the wines in their quality category.\
We use a Bayesian approach to find a model able to describe the relationship between the quality of the wine (target variable) and its features.\
To do so, we use a Gibbs sampler such as JAGS.
## Repository structures
- in the `\assignment` directory it is possible to find the different project proposals. We chose the 7th.
- in the `\chains` directory we store the Markov chains we got using JAGS. Make sure to have this folder in you working directory.
- in the `\data` directory we store the dataset that needs to be analysed
- `1_data_analysis.Rmd`: we make some exploratory analysis and make some considerations about the data. 
- `binomial_standardized.Rmd`: we build a bayesian binomial regression with differente priors and make some predictions. Note: make sure to add `\pictures` folder to
  your working directory, furthermore, you should add `\pictures\binomial`.
- `spikeslab.rmd`: we build a binomial regression with spikes and slab prior.
- `softmax_standardized.Rmd`: we build a bayesian categorical softmax classifier with Gaussian prior. Note: make sure to add `\pictures` folder to
  your working directory, furthermore, you should add `\pictures\categorical`.
- `utils_functions.R`: is a file in which you can find some ggplot wrappers functions.

## Requirements
- R
- JAGS 
