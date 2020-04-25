
#!/bin/bash

#$ -S /bin/bash

#$ -N warming_experiment

 # runtime of 30 mn
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

graph_dir=$1
shift
# start your script
# the "$@" bit means: insert all arguments given to the script here
# echo $graph_dir/graph$SGE_TASK_ID
# echo $1
# python dynamic_threadHPC.py ../web_list/graph$SGE_TASK_ID $2  "$@"
python /home/gauzens/warmingHPC/codes/warming_experimentHPC.py $graph_dir/graph$SGE_TASK_ID $1

# for the experimental networks
