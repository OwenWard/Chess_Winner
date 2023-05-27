#! /bin/bash
#
#SBATCH --mem-per-cpu 16000
#SBATCH -c 4
#SBATCH -t 120:00
#SBATCH --mail-user=oward@sfu.ca
#SBATCH --mail-type=ALL

module load gcc r/4.2.2

echo "Launching R"
date

Rscript Fit_All.R

echo "Completed"
date

# end of script
