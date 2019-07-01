#!/bin/bash
#SBATCH -J my_job_name
#SBATCH -o skeleton.out

#SBATCH -n 1
#SBATCH -p small
#SBATCH -t 2
#SBATCH --reservation=Summerschool
aprun -e OMP_NUM_THREADS=3 -n 1 -d 3  ./prog 

