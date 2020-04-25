import os
path = '/homes/bg33novu/projects/WarmingWebs/WarmingHPC/web_list/'
webs = [name for name in os.listdir(path)]
print len(webs)
print webs


# for niche models, 100 replicates
# thefile = open('/homes/bg33novu/projects/WarmingWebs/WarmingHPC/launchers/parameter_file', 'w')
# comp = 0
# for web in webs:
# 	for temp in range(0, 62, 2):
# 		for repls in range(1, 101):
# 			to_write = ['/home/gauzens/warmingHPC/web_list/' + web, ' ', temp, ' ', comp, '\n']
# 			comp = comp + 1
# 			for item in to_write:
# 				thefile.write(str(item))



# for using the BM function from niche model in experimental networks
# 30 replicates are enough here

thefile = open('/homes/bg33novu/projects/WarmingWebs/WarmingHPC/launchers/parameter_expNW_nicheBM', 'w')
comp = 0
for web in webs:
	for temp in range(0, 62, 1):
		for repls in range(1, 31):
			comp = comp + 1
			to_write = ['/home/gauzens/warmingHPC/web_list/' + web, ' ', temp, ' ', comp, '\n']
			for item in to_write:
				thefile.write(str(item))




# # long gradient of experimental networks
# thefile = open('/homes/bg33novu/projects/WarmingWebs/WarmingHPC/launchers/parameter_expNW', 'w')
# for web in webs:
# 	graphfile = '/home/gauzens/warmingHPC/web_list/' + web
# 	print graphfile
# 	for temp in range(0, 620, 01):
# 		to_write = [graphfile, ' ', temp/10.0, '\n']
# 		for item in to_write:
# 			thefile.write(str(item))


# print len(webs)
# # print len(range(0, 62, 0.1))
# print len(range(1, 101))
# print comp == 122 * 100 * 26
# print comp
# print 122 * 100 * 26