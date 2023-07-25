#!/bin/bash
##
## example-array.slurm.sh: submit an array of jobs with a varying parameter
##
## Lines starting with #SBATCH are read by Slurm. Lines starting with ## are comments.
## All other lines are read by the shell.
##
#SBATCH --account=priority-briansmithers        #specify the account to use
#SBATCH --job-name=fire            # job name
#SBATCH --partition=priority              # queue partition to run the job in
#SBATCH --nodes=1                       # number of nodes to allocate
#SBATCH --ntasks-per-node=1             # number of descrete tasks - keep at one except for MPI
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1              # number of cores to allocate
#SBATCH --mem=400G                     # 2000 MB of Memory allocated; set --mem with care
##SBATCH --mem-per-cpu=100G
#SBATCH --time=1-00:00:00                 # Maximum job run time
##SBATCH --array=0-1847%10                  # Number of jobs in array
#SBATCH --mail-user=shuysman@gmail.com
#SBATCH --mail-type=ALL
#SBATCH --output=fire-%A-%a.out
#SBATCH --error=fire-%A-%a.err
##SBATCH --output=fire-%j.out
##SBATCH --error=fire-%j.err


MODELS=("NorESM1-M" "MRI-CGCM3" "MIROC5" "IPSL-CM5A-LR" "inmcm4" "HadGEM2-CC365" "CSIRO-Mk3-6-0" "CNRM-CM5" "CanESM2" "BNU-ESM" "CCSM4" "GFDL-ESM2G")
##SCENARIOS=("rcp45" "rcp85")
##FUTURE_FILE_DIR=${HOME}/data/gye/forecasts/rolling_sum/
##FILES="${FUTURE_FILE_DIR}Deficit_rolling_sum_7_${MODELS[$SLURM_ARRAY_TASK_ID]}_${SCENARIOS[$((${SLURM_ARRAY_TASK_ID} % 2))]}_*.nc"

date                            # print out the date
hostname -s                     # print a message from the compute node
module load Mamba
source $HOME/.bashrc
mamba activate r-test
Rscript projections-terra.R ${MODELS[$SLURM_ARRAY_TASK_ID]} rcp45
Rscript projections-terra.R ${MODELS[$SLURM_ARRAY_TASK_ID]} rcp85
date                            # print the date again
