#!/bin/bash
##
## example-array.slurm.sh: submit an array of jobs with a varying parameter
##
## Lines starting with #SBATCH are read by Slurm. Lines starting with ## are comments.
## All other lines are read by the shell.
##
#SBATCH --account=priority-briansmithers        #specify the account to use
#SBATCH --job-name=fire            # job name
#SBATCH --partition=legacy              # queue partition to run the job in
#SBATCH --nodes=5                       # number of nodes to allocate
#SBATCH --ntasks-per-node=4             # number of descrete tasks - keep at one except for MPI
#SBATCH --cpus-per-task=1              # number of cores to allocate
##SBATCH --mem=50G                     # 2000 MB of Memory allocated; set --mem with care
#SBATCH --mem-per-cpu=75G
#SBATCH --time=6-00:00:00                 # Maximum job run time
##SBATCH --array=0-3                  # Number of jobs in array
#SBATCH --mail-user=shuysman@gmail.com
#SBATCH --mail-type=ALL
#SBATCH --output=fire-%A-%a.out
#SBATCH --error=fire-%A-%a.err

## Run 'man sbatch' for more information on the options above.
FUTURE_FILE_DIR=/media/smithers/shuysman/data/nps_gridded_wb/gye/forecasts/rolling_sum/
FUTURE_FILES=(${FUTURE_FILE_DIR}/*)

date                            # print out the date
hostname -s                     # print a message from the compute node
module load Mamba
source $HOME/.bashrc
mamba activate r-test
Rscript projections-terra.R ${FUTURE_FILES[$SLURM_ARRAY_TASK_ID]}
date                            # print the date again
