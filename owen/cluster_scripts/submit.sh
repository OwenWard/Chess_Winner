#! /bin/bash
#
#SBATCH --mem-per-cpu 12000
#SBATCH -c 8
#SBATCH -t 600:00
#SBATCH --mail-user=oward@sfu.ca
#SBATCH --mail-type=ALL

module load gcc r/4.2.2

echo "Launching R"
date

Rscript Large_Fit.R

echo "Completed"
date

# end of script
