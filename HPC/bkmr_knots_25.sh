#!/bin/bash
#$ -cwd -S /bin/bash
#$ -l mem=26G
#$ -l time=:11000:
#$ -M eag2186@cumc.columbia.edu

$MODULESHOME/init/bash
module load R/3.6.0

clear
echo "BKMR Knots 1"> R-script.log

R CMD BATCH bkmr_loop_knots_25.R


