#!/bin/bash -l
#SBATCH --job-name=R_phylo_signal
#SBATCH --account=project_2007729
#SBATCH --output=output_%j.txt
#SBATCH --error=errors_%j.txt
#SBATCH --partition=longrun
#SBATCH --time=8-00:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=8G
#SBATCH --mail-type=FAIL
#SBATCH --mail-type=END
#SBATCH --array=1-100

seff $SLURM_JOBID R_BFmod_array

# Load R 4.2.2 ( use r-env-singularity for the lastest version)
module load r-env/422

# Clean up .Renviron file in home directory
if test -f ~/.Renviron; then
sed -i '/TMPDIR/d' ~/.Renviron
sed -i '/OMP_NUM_THREADS/d' ~/.Renviron
fi

# Match thread and core numbers
export APPTAINERENV_OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

# Thread affinity control
export APPTAINERENV_OMP_PLACES=cores
export APPTAINERENV_OMP_PROC_BIND=close


# Specify a temp folder path and the path to the matching R package
echo "TMPDIR==/scratch/project_2007729/avrodrigues/R/bbs_across_taxa" >> ~/.Renviron
echo "My SLURM_ARRAY_TASK_ID:" $SLURM_ARRAY_TASK_ID

# Run the R script
srun apptainer_wrapper exec Rscript --no-save ~/script/do/ $SLURM_ARRAY_TASK_ID

