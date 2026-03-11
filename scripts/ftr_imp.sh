#!/bin/bash
#
#SBATCH --job-name=ftr_imp
#SBATCH --time=32:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --qos=batch
#SBATCH --partition=compute
#SBATCH --mem=1024G
#SBATCH --array=1
#SBATCH --output=logs/output-%A_%a.txt

echo "Start time: $(date)"

module load slurm
export PATH=/cm/shared/apps/slurm/current/bin:$PATH
module load singularity
#singularity exec --bind /cm /projects/kumar-lab/sabnig/Builds/RML.sif \
singularity exec --env APPEND_PATH=/cm/shared/apps/slurm/current/bin --bind /cm --bind /run /projects/kumar-lab/USERS/sabnig/containers/RML.sif \
Rscript src/ftr_imp.R
