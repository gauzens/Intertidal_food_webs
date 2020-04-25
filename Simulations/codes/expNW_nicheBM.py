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
import random

#####################
# parametres #
####################
#3000 ans:
temps = 31536000000 * 3
var_bm = 1

parser = argparse.ArgumentParser(description='simulation on a rocky pool network')
parser.add_argument("network_file")
parser.add_argument("temperature", type=int)
parser.add_argument("seed", type=int)

input_args = parser.parse_args()
random.seed(input_args.seed)

g = nx.read_gml(input_args.network_file)
fw.graph2fw(g)
fw.TL(g)

if g.graph['mean_TL'] == 'NA':
    print 'No_trophic_levels'
    exit()
high_tl = max([g.node[n]['TL'] for n in g.nodes_iter()])
if high_tl > 25:
    print 'high TLs'
    exit()

for sp in g.nodes_iter():
    g.node[sp]['body_masses'] = 0.01 * np.power(100, g.node[sp]['TL'] - 1 + np.random.normal(0, var_bm))

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

try:
    fw.omnivory(g)
    res.append(g.graph['mean_oi'])
except:
    res.append('NA')

res.append(g.graph['mean_TL'])
res.append(g.graph['C'])
res.append(var_bm)
print res

