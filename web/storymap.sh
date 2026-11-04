#!/bin/bash
#SBATCH --account=rpp-gonzalez
#SBATCH --time=10:00:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=200G 
#SBATCH --job-name=storymap
##SBATCH --mail-user=francois.rousseu@usherbrooke.ca
##SBATCH --mail-type=ALL

## module load StdEnv/2023 gdal/3.9.1 udunits/2.2.28 r/4.5.0 
module load StdEnv/2026 gcc/15.3.0 r/4.6.1 geos/3.14.1 gdal/3.13.3 udunits/2.2.28 gsl/2.8

while true; do echo "[heartbeat] $(date '+%F %T')"; sleep 60; done &
HB=$!
trap "kill $HB" EXIT
Rscript web/storymap.r
