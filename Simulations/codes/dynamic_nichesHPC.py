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
import sys

# here is an optional argument to know if previous integration failed and I am reruning them with another seed
parser = argparse.ArgumentParser(description='dynamics of niche models')
parser.add_argument("--increase_seed", action="store_true")
args = parser.parse_args()


for line in sys.stdin:
    network_file, temperature, seed = line.split(' ')
    temperature = int(temperature)
    if args.increase_seed:
        seed = int(seed) + 99999999
    else:
        seed = int(seed)

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

    # parser = argparse.ArgumentParser(description='dynamics of niche models')
    # parser.add_argument("network_file")
    # parser.add_argument("temperature", type=int)
    # parser.add_argument("seed", type=int)
    # input_args = parser.parse_args()

    random.seed(seed)

    try:
        g = nx.read_gml(network_file)
    except:
        print 'unable to read', network_file
    fw.graph2fw(g)
    s = g.number_of_nodes()
    l = g.number_of_edges()
    c = float(l) / (s * s)
    g1 = None
    while g1 is None:
        try:
            g1 = fw.niche_model(s, c)
        except:
            g1 = None
        try:
            fw.graph2fw(g1)
        except:
            print 'issue in fw library, graph2fw, with graph: ', network_file
        try:
            fw.TL(g1)
            #  !!!!!!!!!!!!! check why sometime TL are super high...
            high_tl = max([g1.node[n]['TL'] for n in g1.nodes_iter()])
            if high_tl > 25:
                g1 = None
            else:
                for sp in g1.nodes_iter():
                    g1.node[sp]['body_masses'] = 0.01 * np.power(100, g1.node[sp]['TL'] - 1 + np.random.normal(0, 1))
        except:
            g1 = None

    # m = model.model(g, 293.15 + temp + temp_increase)
    try:
        m = model.model(g1, 273.15 + temperature)
        biomasses = m.ki * 0.8
        r = integrate.ode(m.Cderivates_call_spe).set_integrator('lsoda', nsteps=500000)
        r.set_initial_value(biomasses, 0.0)
    except:
        print 'issue in initialisation, graph: ', network_file
    # t = [0]
    # y = [biomasses]
    try:
        r.integrate(temps)
        resilience = 0.0
        nb_b = g1.graph['nb_b']
        nb_ext = (r.y < 10e-12).sum()
        nb_ext_basal = (r.y[:nb_b] < 10e-12).sum()
        nb_ext_non_basal = (r.y[nb_b:] < 10e-12).sum()
        res = ([g.graph['path'], g.graph['region'], g.graph['initTemp'], g.graph['nb_s'], temperature, nb_ext_non_basal, nb_ext, resilience])
    except:
        res = ([g.graph['path'], g.graph['region'], g.graph['initTemp'], g.graph['nb_s'], temperature, 'NA', 'NA', 'integration_err'])

    try:
        fw.omnivory(g1)
        res.append(g1.graph['mean_oi'])
    except:
        res.append('NA')

    res.append(g1.graph['mean_TL'])
    res.append(g1.graph['C'])
    res.append(seed)

    print res
