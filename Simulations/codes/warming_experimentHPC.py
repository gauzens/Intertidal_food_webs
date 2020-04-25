# from __future__ import division # pour que / soit division reelle meme
# entre deux entiers
import networkx as nx
import food_webs as fw
import numpy as np
from scipy import integrate
import binzer_2016_interference as model
import argparse
parser = argparse.ArgumentParser(description='experimental warming')
parser.add_argument("network_file")
parser.add_argument("warming", type=int)
input_args = parser.parse_args()
# print 'args: ', input_args.network_file, '  ', input_args.warming
warming = input_args.warming / 5.
#####################
# parametres #
####################
#3000 ans:
temps = 31536000000 * 3


g = nx.read_gml(input_args.network_file)
# g.reverse(copy = False)
fw.graph2fw(g)
fw.TL(g)

for n in g.nodes_iter():
    g.node[n]['body_masses'] = g.node[n]['masses']
    # g.node[n]['status'] = g.node[n]['status2']




m = model.model(g, 273.15 + int(round(g.graph['tempSee'])) + warming)
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
    # nb_ext_basal = (r.y[:nb_b] < 10e-12).sum()
    nb_ext_non_basal = (r.y[nb_b:] < 10e-12).sum()

    res = ([g.graph['path'], g.graph['region'], g.graph['repl'], g.graph['depth'], g.graph['area'], g.graph['tempSee'], g.graph['initTemp'], g.graph['nb_s'], warming, nb_ext_non_basal, nb_ext, resilience])

except:
    res = ([g.graph['path'], 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', "integration error"])

try:
    fw.omnivory(g)
    res.append(g.graph['mean_oi'])
except:
    res.append('NA')

res.append(g.graph['mean_TL'])
res.append(g.graph['C'])
print res
