# from __future__ import division # pour que / soit division reelle meme
# entre deux entiers
import networkx as nx
import food_webs as fw
import numpy as np
# import multiprocessing as mp
from scipy import integrate
import binzer_2016_interference as model
# import time
import argparse


#####################
# parametres #
####################
#3000 ans:
temps = 31536000000 * 3
#1000 ans:
# temps = 31536000000
# 100 ans: 
# temps = 3153600000
# temps = 300


BM_distribution = "empirical"
# BM_distribution = "niche"
parser = argparse.ArgumentParser(description='simulation on a rocky pool network')
parser.add_argument("network_file")
parser.add_argument("temperature", type=float)
input_args = parser.parse_args()

g = nx.read_gml(input_args.network_file)

if BM_distribution == "empirical":
    for n in g.nodes_iter():
        g.node[n]['body_masses'] = g.node[n]['masses']


fw.graph2fw(g)
fw.TL(g)

success = True
if BM_distribution == "empirical":
    for n in g.nodes_iter():
        # print g.node[n]
        g.node[n]['body_masses'] = g.node[n]['masses']
        # g.node[n]['status'] = g.node[n]['status2']
else:
    try:
        for sp in g.nodes_iter():
            g.node[sp]['body_masses'] = 0.01 * np.power(100, g.node[sp]['TL'] - 1 + np.random.normal(0, 0.1))
    except:
        success = False

if success:
    # m = model.model(g, 293.15 + temp + temp_increase)
    m = model.model(g, 273.15 + input_args.temperature)
    biomasses = m.ki * 0.8
    r = integrate.ode(m.Cderivates_call_spe).set_integrator('lsoda', nsteps=500000)
    r.set_initial_value(biomasses, 0.0)
    # t = [0]
    # y = [biomasses]
    try:
        r.integrate(temps)

        resilience = 0.0
        nb_b = g.graph['nb_b']
        nb_ext = (r.y < 10e-12).sum()
        nb_ext_basal = (r.y[:nb_b] < 10e-12).sum()
        nb_ext_non_basal = (r.y[nb_b:] < 10e-12).sum()

        res = ([g.graph['path'], g.graph['region'], g.graph['initTemp'], g.graph['nb_s'], input_args.temperature, nb_ext_non_basal, nb_ext, resilience])

    except:
        res = ([g.graph['path'], g.graph['region'], g.graph['initTemp'], g.graph['nb_s'], input_args.temperature, 'NA', 'NA', 'integration_err'])
else:
    res = ([g.graph['path'], g.graph['region'], g.graph['initTemp'], g.graph['nb_s'], input_args.temperature, 'NA', 'NA', 'BM_error'])

try:
    fw.omnivory(g)
    res.append(g.graph['mean_oi'])
except:
    res.append('NA')

res.append(g.graph['mean_TL'])
res.append(g.graph['C'])

print res

