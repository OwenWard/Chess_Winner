#! /bin/bash
#
#SBATCH --mem-per-cpu 8000
#SBATCH -c 8
#SBATCH -t 300:00
#SBATCH -a 6-100
#SBATCH --mail-user=oward@sfu.ca
##SBATCH --mail-type=ALL

module load gcc r/4.2.2

echo "Launching R"
date

Rscript perm_results.R

echo "Completed"
date

# end of script
