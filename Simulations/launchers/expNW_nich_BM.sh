
#!/bin/bash

#$ -S /bin/bash

#$ -N niche_exp_network

 # runtime hh:mm:ss
#$ -l h_rt=01:00:00

 # memory / RAM
#$ -l h_vmem=1G

# job output goes here
#$ -e /work/$USER/$JOB_NAME-$JOB_ID-$TASK_ID.err


# set working directory to where you checked out your python code
#$ -wd /home/gauzens/warmingHPC/codes/

#$ -binding linear:1

# load python software
module load python/2 lapack/gcc
source ~/WarmingWebs/bin/activate || exit 1


### using file parameter input:
line=$(sed -n "$SGE_TASK_ID p" $1)
# need only second and third line elements, accessed using ${line[1]} ${line[2]}
python /home/gauzens/warmingHPC/codes/expNW_nicheBM.py $line
