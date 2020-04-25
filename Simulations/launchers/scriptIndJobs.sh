
#!/bin/bash

#$ -S /bin/bash

#$ -N experimental_networks

 # runtime of 30 mn
#$ -l h_rt=00:40:00

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
python /home/gauzens/warmingHPC/codes/dynamic_threadHPC.py ${line[0]} ${line[1]}

# for the experimental networks
