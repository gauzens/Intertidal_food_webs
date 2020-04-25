# master script (this one is in python)

# ssh to eve
# ssh -fN idiv-gateway && ssh idiv1
source ~/WarmingWebs/bin/activate
###########################################
########## the warming experiment##########
###########################################
output="/home/gauzens/warmingHPC/outputs/warming_exp_k4-$(date +%s).csv"
for i in $(seq 0 1 20) ; do
  qsub -o "$output" -t 1-122 scriptWarmingExp.sh ~/warmingHPC/web_list $i
done


###########################################
########## complete temp gradient #########
###########################################
2-3-7

##### for experimental netorks ############
output="/home/gauzens/warmingHPC/outputs/experimentals_K5_h1.2-$(date +%s).csv"
linecount=$(wc -l < parameter_expNW)
qsub -o "$output" -t 1-$linecount scriptIndJobs.sh /home/gauzens/warmingHPC/launchers/parameter_expNW


#### experimental networks, but BM from usual niche appraoch
# to avoid multiples qsub calls, parameters are in a txt formated file
linecount=$(wc -l < parameter_expNW_nicheBM)
output="/home/gauzens/warmingHPC/outputs/expNW_nicheBM_k4-$(date +%s).csv"
qsub -o "$output" -t 1-$linecount expNW_nich_BM.sh /home/gauzens/warmingHPC/launchers/parameter_expNW_nicheBM




########3 launching niche models ############
# to avoid multiples qsub calls, parameters are in a txt formated file

linecount=$(wc -l < parameter_file)
stepsize=20
time_max=4
output="/home/gauzens/warmingHPC/outputs/niches_k4-$(date +%s).csv"
qsub -o "$output" -t 1-$linecount:$stepsize -l h_rt=$time_max:00:00 launchingNicheDynamics_chunked.sh /home/gauzens/warmingHPC/launchers/parameter_file


# will write in ~/lastid all tasks ids

echo $id >> ~/lastid
## > create new file (erase)
# >> append

# then the last id correspond to the latest job
# job is the qsub command and tasks are all the script launched
# the use: 
# module load grid-engine-tools
# qdiagnose-job <last job made>
# to get an output like:
# jobID.taskID <why job failed> <error code>
# task id is exactly matching the $SGE_TASK_ID
# then gettasks ids:
# 
# qdiagnose return all tasks from job 44613148
# then grep return all characters between . and \ e (i.e. the task ids)
# finally save everything to file to_return
#qdiagnose-job XXXX | grep -o -P '(?<=\.).*(?=\ e)' > /home/gauzens/warmingHPC/codes/to_rerun

# faster than qdiagnose
qacct -j 4724144 | qacct-failed | awk '$1 == "taskid" { print $2 }' > /home/gauzens/warmingHPC/codes/to_rerun

stepsize=2
time_max=4
linecount=$(wc -l < /home/gauzens/warmingHPC/codes/to_rerun)
output="/home/gauzens/warmingHPC/outputs/niches-redoNCC-$(date +%s).csv"
qsub -o "$output" -t 1-$linecount l h_rt=$time_max:00:00 relaunch_niches.sh /home/gauzens/warmingHPC/codes/to_rerun $stepsize
# then if all jobs were succesfull concatenate, or redo







