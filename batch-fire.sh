#!/bin/bash

FUTURE_FILE_DIR=${HOME}/data/gye/forecasts/rolling_sum/
FUTURE_FILES=(${FUTURE_FILE_DIR}/*)
N_FILES=${#FUTURE_FILES[@]}

sbatch --array=0-$(($N_FILES-1))%10 fire.sbatch

