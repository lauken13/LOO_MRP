# Using LOO for MRP
Code to explore and investigate validating multilevel regression and poststratification (MRP) models using leave-one-out cross validation (LOO). The work is documented on our arxiv paper [here](https://arxiv.org/abs/2209.01773).

## How to navigate the folders and run the code 

In RStudio, first open `LOO_MRP.Rproj`, this will help set the working directory. 

The ``../code`` folder contains three folders ``simDesign1``, ``simDesign2``, and ``nhanes`` for the two simulation designs and the NHANES data application.


## Figures in the paper 

`../figures`:

- `main_paper.Rmd` is the RMarkdown file for generating the figures in the paper
- uses `res_list_N02_1000.rds`, `res_list_sae_1000_wtd.rds`, and `res_list_sae_500_wtd.rds` (for plots in the appendix).
