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
#SBATCH --cpus-per-task=8              # number of cores to allocate
#SBATCH --mem=1024G                     # 2000 MB of Memory allocated; set --mem with care
#SBATCH --time=4-00:00:00                 # Maximum job run time
##SBATCH --array=1-3                  # Number of jobs in array
#SBATCH --mail-user=shuysman@gmail.com
#SBATCH --mail-type=ALL
#SBATCH --output=fire-%j.out
#SBATCH --error=fire-%j.err

## Run 'man sbatch' for more information on the options above.

date                            # print out the date
hostname -s                     # print a message from the compute node
module load Mamba
source $HOME/.bashrc
mamba activate r-test
Rscript projections-terra.R
date                            # print the date again
