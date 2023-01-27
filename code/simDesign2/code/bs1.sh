#!/bin/env bash
#SBATCH --job-name=sae500
#SBATCH --time=05:00:00
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --cpus-per-task=1
#SBATCH --output=/dev/null
#SBATCH --mem-per-cpu=8000
#SBATCH --array=6-100

module load R/4.0.5

R CMD BATCH --no-save --no-restore 00-script_cluster.R script_$SLURM_ARRAY_TASK_ID 

