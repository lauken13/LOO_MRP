# LOO_MRP
Code to investigate MRP model validation using LOO. The work is documented on our arxiv paper [here](https://arxiv.org/abs/2209.01773).

## How to navigate the folders and run the code 

In R, first open `LOO_MRP.Rproj`, this will help set the working directory. 

`script_cluster.R` was used to generate simulated data 100 times on the Monash (MonARCH)[https://docs.monarch.erc.monash.edu/] cluster. The code has been adapted to run locally (note: possibily long run times on local computers!) 

Run `read_bash_loop.R` to generate data using `gen_dat_function.R` and `loo_wtd_functions.R` to compare results from 15 different models.





Run `gen_dat_function.R` to generate simulated MRP data. 


code > 



(The original code were ran using batch scripts)
