### Simulation design 1 

`../simDesign1/code`:

- The two files below were ran on the Monash [MonARCH](https://docs.monarch.erc.monash.edu/MonARCH/aboutMonArch.html) cluster: 

  - `00-script_cluster_base.R` generates simulated data 100 times using `func_gen_dat.R` on the HPC cluster. For each set of data, 15 different stan models (in the `/stancode` folder) were ran. The calculated PSIS-LOO values for all the models for each dataset are saved in individual files. You could extract the code in this file to run a for loop locally. (note: possibly long run times and take up space on local computers!) 

  - `00-read_bash_loop_base.R` reads the individual 100 files and calculates the individual sample, MRP estimates, LOO and weighted LOO values (using `func_loo_wtd.R`). The results are saved in `loo_N02_1000.RData` (the .RData files are not available on GitHub due to file size issues, available upon reasonable requests.) 
  
  - `bs1.sh` and `bs2.sh` are the batch script files.

- `01-tab_popn.R` uses the .RData file and tabulates all the individual, population, small area estimates using `tab_elpd.R`, `tab_samp.R`, `tab_SAE.R` (uses `tab_SAE_elpd_all.R` and `tab_SAE_elpd_wtd.R`), `tab_counts.R` for plotting. Results are saved in the `/data` folder.

`../simDesign1/data`:
- `res_list_N02_1000.rds` - contains all the necessary tables to plot the figures as in `../figures/main_paper.Rmd`.

### Simulation design 2

`../simDesign2/code`:

- Similar set up as Simulation design 1 above and generates `loo_sae_500_wtd.RData` (and `loo_sae_1000_wtd.RData` for the appendix).
- Generates `.rds` files and are saved in the `/data` folder.

`../simDesign2/data`:
- Two .rds files: `res_list_sae_500_wtd.rds` (and `res_list_sae_1000_wtd.rds` for the results in the appendix) contains the tables to plot the figures as in `../figures/main_paper.Rmd`.

### NHANES data application 

`../nhanes/code`:
- `datacleaning.R` 
  - cleaning and collating variables data extracted from the [NHANES website](https://wwwn.cdc.gov/Nchs/Nhanes/continuousnhanes/default.aspx?cycle=2017-2020).
  - takes subsamples and calculate respective weights using `bart()`
  - generates an articial sample and respective weights
  - data saved as `nhanes_final_gen.rds`
