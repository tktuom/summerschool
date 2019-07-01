#!/bin/bash
#SBATCH -J my_job_name
#SBATCH -o skeleton.out

#SBATCH -n 4
#SBATCH -p small
#SBATCH -t 2
#SBATCH --reservation=Summerschool
aprun -n 4 ./prog 

