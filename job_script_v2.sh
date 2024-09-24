#!/bin/bash -l
#SBATCH --job-name=test_phylo_signal
#SBATCH --account=project_2007729
#SBATCH --output=output_%j.txt
#SBATCH --error=errors_%j.txt
#SBATCH --partition=test
#SBATCH --time=00:15:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --mem=16G
#SBATCH --mail-type=FAIL
#SBATCH --mail-type=END
#SBATCH --array=2

seff $SLURM_JOBID test_phylo_signal

# Load R 4.2.2 ( use r-env-singularity for the lastest version)
module load r-env/422

# Clean up .Renviron file in home directory
if test -f ~/.Renviron; then
sed -i '/TMPDIR/d' ~/.Renviron
sed -i '/OMP_NUM_THREADS/d' ~/.Renviron
fi

# Specify a temp folder path and the path to the matching R package
echo "TMPDIR==/scratch/project_2007729/avrodrigues/R/bbs_across_taxa" >> ~/.Renviron
echo "My SLURM_ARRAY_TASK_ID:" $SLURM_ARRAY_TASK_ID

# Run the R script
srun apptainer_wrapper exec Rscript --no-save script/do/07_1_calc_phylo_signal_birds.R  $SLURM_ARRAY_TASK_ID

