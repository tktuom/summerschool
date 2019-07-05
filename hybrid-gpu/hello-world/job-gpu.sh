#!/bin/bash
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -p gpu
#SBATCH -t 00:05:00
#SBATCH --gres=gpu:p100:1
#SBATCH --reservation=Summerschool
#SBATCH -o hi.out

srun ./hello