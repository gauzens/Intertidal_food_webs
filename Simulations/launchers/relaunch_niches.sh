#!/bin/bash

#$ -S /bin/bash

#$ -N niche_networks_redo

 # memory / RAM
#$ -l h_vmem=2G

# job output goes here
#$ -e /work/$USER/$JOB_NAME-$JOB_ID-$TASK_ID.err


# set working directory to where you checked out your python code
#$ -wd /home/gauzens/warmingHPC/codes/

#$ -binding linear:1

# load python software
module load python/2 lapack/gcc
source ~/WarmingWebs/bin/activate || exit 1


### using file parameter input
# get the job id of line 
#line_id=($(sed -n "$SGE_TASK_ID p" $1))

# first line to read should correspond to the task_id
first_line=($(sed -n "$SGE_TASK_ID p" $1))
# $2 is the step_size used before
last_line=$(( $SGE_TASK_ID + $2 ))


sed -n "$first_line,$last_line p" /home/gauzens/warmingHPC/codes/parameter_file |
python /home/gauzens/warmingHPC/codes/dynamic_nichesHPC.py --increase_seed
# here the --change will be read as an argument. then i should check in argparse how to get options instead of arguments

