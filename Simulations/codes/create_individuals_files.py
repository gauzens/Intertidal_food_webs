#create individual input files in the same directory
import os
import networkx as nx
import food_webs as fw
import numpy as np

path = '/homes/bg33novu/projects/WarmingWebs/'
data_path = path + 'data/'



# temperature = {}
# temperature['Canada_txt'] = 13
# temperature['Brasil(CE)_txt'] = 28
# temperature['Moz_txt'] = 27
# temperature['England_txt'] = 16
# temperature['Mad_txt'] = 22
# temperature['Portugal_txt'] = 19
# temperature['Brasil(SP)_txt'] = 26

temperature = {}
temperature['Canada_txt'] = 11.5
temperature['Brasil(CE)_txt'] = 28.4
temperature['Moz_txt'] = 27.4
temperature['England_txt'] = 16.4
temperature['Mad_txt'] = 22.9
temperature['Portugal_txt'] = 19.5
temperature['Brasil(SP)_txt'] = 26.3

def file2dic(path):
    fichier = open(path, 'r')
    dic_mass = {}
    dic_info = {}
    for line in fichier:
        if line[0] != '#':
            k, x, y = line.split('\t')
            while (y[-1] == '\r') | (y[-1] == '\n'):
                y = y[:-1]
            dic_mass[int(k)] = float(x)
            dic_info[int(k)] = y
    fichier.close()
    return (dic_mass, dic_info)


regions = [name for name in os.listdir(data_path) if '.' not in name]
pools_infos = np.load('/homes/bg33novu/projects/WarmingWebs/WarmingHPC/codes/pools_info.npy').item()
n = 1
for region in regions:
    temp = temperature[region]
    regional_path = data_path + region + '/'
    replicates = [name for name in os.listdir(
        regional_path) if '.' not in name]
    for repl in replicates:
        ind_temp = pools_infos[repl]['temp']
        # print g.graph[repl]['temp']
        final_path = regional_path + repl
        web_path = regional_path + repl + '/web2.txt'
        g = nx.read_adjlist(
            web_path, create_using=nx.DiGraph(), comments='P', nodetype=int)
        # fw.graph2fw(g)
        # print 'final_path: ', final_path
        mass, info = file2dic(final_path + '/masses.txt')
        nx.set_node_attributes(g, 'masses', mass)
        nx.set_node_attributes(g, 'morpho', info)

        basal = 0
        for cle in info:
            if info[cle] != "B":
                g.node[cle]["status"] = "non_basal"
            else:
                g.node[cle]["status"] = "basal"

        temp_see = temperature[region]
        g.graph['initTemp'] = float(ind_temp)
        # print g.graph['initTemp']
        g.graph['tempSee'] = temp_see
        g.graph['region'] = region
        g.graph['repl'] = repl
        g.graph['path'] = web_path
        g.graph['depth'] = float(pools_infos[repl]['depth'])
        g.graph['area'] = float(pools_infos[repl]['area'])
        con_comp = nx.number_connected_components(g.to_undirected())
        if con_comp == 1:
            name = 'graph' + str(n)
            nx.write_gml(g, path + 'web_list/' + name)
            n = n + 1
