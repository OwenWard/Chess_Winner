#! /bin/bash
#
#SBATCH --mem-per-cpu 8000
#SBATCH -c 6
#SBATCH -J bullet_n10
#SBATCH -t 2000:00
#SBATCH -a 1-4
#SBATCH --mail-user=oward@sfu.ca
#SBATCH --mail-type=ALL

module load gcc r/4.2.2

echo "Launching R"
date

Rscript Large_Fit.R
#Rscript Large_Single_Prev.R
#Rscript Winner_Effects_Time.R
#Rscript perm_results.R

echo "Completed"
date

# end of script
