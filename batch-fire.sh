#!/bin/bash

FUTURE_FILE_DIR=/media/smithers/shuysman/data/nps_gridded_wb/gye/forecasts/rolling_sum/
FUTURE_FILES=${FUTURE_FILE_DIR}/*
N_FILES=$(echo $FUTURE_FILES | wc -w)

sbatch --array=0-${N_FILES} fire.sbatch
