#!/bin/bash

MODELS=("NorESM1-M" "MRI-CGCM3" "MIROC5" "IPSL-CM5A-LR" "inmcm4" "HadGEM2-CC365" "CSIRO-Mk3-6-0" "CNRM-CM5" "CanESM2" "BNU-ESM" "CCSM4" "GFDL-ESM2G")
N=${#MODELS[@]}

sbatch --array=0-$(($N-1))%2 fire.sbatch

