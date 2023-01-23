#!/bin/env bash
#SBATCH --job-name=looN02
#SBATCH --time=10:00:00
#SBATCH --cpus-per-task=5
#SBATCH --output=/dev/null
#SBATCH --mem-per-cpu=4000

module load R/4.0.5

R CMD BATCH --no-save --no-restore 00-read_bash_loop_base.R
