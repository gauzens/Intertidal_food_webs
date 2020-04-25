# -*- coding: utf8 -*-

'''
Model from Binzer et al. 2016
Interactive effects of warming, eutrophication and size structure: impacts on biodiversity and food-web structure.
Global Change Biology (2016) 22, 220–227, doi: 10.1111/gcb.13086
'''

# allometric interference:
# Body mass constraints on feeding rates determine the consequences of predator loss
# Schneider et al. 2012, ELE
# http://onlinelibrary.wiley.com/doi/10.1111/j.1461-0248.2012.01750.x/full

# get activation energy from
# Systematic variation in the temperature dependence of physiological and ecological traits
# Dell et al. 2011, PNAS

from __future__ import division  # pour que / soit division reelle meme entre deux entiers
import numpy as np
import sys
import ctypes
# from numpy.ctypeslib import ndpointer
import os
import networkx as nx
import time

class model:
	def __init__(self, g, temperature):
		try:
			self.name = g.graph['path']
		except:
			pass
		self.nb_s = g.graph['nb_s']
		self.nb_b = g.graph['nb_b']

		self.t = temperature
		self.t0 = 293.15 # coorespond to 0 celsius
		self.boltzmann = 8.6173324e-5
		self.q = 1.2  # hill coeficient

		self.basales = [i for i in g.nodes_iter() if g.node[i]['status'] == 'basal']
		self.pred = [i for i in g.nodes_iter() if g.node[i]['status'] != 'basal']

		self.body_masses = np.array([g.node[i]['body_masses'] for i in self.basales + self.pred])
		self.efficiencies = np.zeros(self.nb_s)  # predators conversion efficiencies
		# less extinctions when efficiency is low
		self.efficiencies[:self.nb_b] = 0.545  # warning, not taken into account in baizer et al.
		self.efficiencies[self.nb_b:] = 0.906


		# adjacency matrix with basal species first, predators then
		self.adjacency = nx.to_numpy_matrix(g, self.basales + self.pred, weight=None)
		self.adjacency = np.array(self.adjacency)
		# nx.from_numpy_matrix(self.adjacency, create_using = g)
		self.parameters = {
			# intercept, species I, E
			'ki': ([4, 0.28, 0.71]),
			'ri': ([-15.68, -0.25, -0.84]),
			'xi': ([-16.54, -0.31, -0.69]),
			# intercept, speciesi, predator of i, E
			'aji': ([-13.1, 0.25, -0.8, -0.38]),
			'Thji': ([9.66, -0.45, 0.47, 0.26]),
			'ci': ([0, 0.21, -0.65]) # took activation energy from this review paper...
		}

		self.mat_a = self.scaling_param_double(self.body_masses, self.parameters['aji'], self.t, g)
		self.mat_th = self.scaling_param_double(self.body_masses, self.parameters['Thji'], self.t, g)
		self.ri = self.scaling_param_simple(self.body_masses, self.parameters['ri'], self.t)
		self.ki = self.scaling_param_simple(self.body_masses, self.parameters['ki'], self.t)
		self.xi = self.scaling_param_simple(self.body_masses, self.parameters['xi'], self.t)
		self.ci = np.zeros(self.nb_s)
		self.db = np.zeros(self.nb_s)
		self.mat_f = np.zeros((self.nb_s, self.nb_s))

		self.jacob = np.zeros((self.nb_s, self.nb_s))

		c_lib = os.path.dirname(os.path.abspath(sys.argv[0])) + "/db_binzer_2016_interference.so"
		self.Cfun = ctypes.CDLL(c_lib)
		self.Cderivates = self.Cfun.db
		self.Cjacobian = self.Cfun.jacobian
		# vector pointer
		self.p1 = np.ctypeslib.ndpointer(dtype=ctypes.c_double, ndim=1, flags='C_CONTIGUOUS')
		# matrix / 2Darray pointer
		self.p2 = np.ctypeslib.ndpointer(dtype=ctypes.c_double, ndim=2, flags='C_CONTIGUOUS')

		# dbs: vec	biomasses: vec, body_masses: vec, mat_a: mqt, mat_th: mat, ri: vec, ki: vec, xi: vec, efficienies: vec, q: float
		self.Cderivates.argtypes = [self.p1, self.p1, self.p2, self.p2, self.p1, self.p1, self.p1, self.p1, self.p1, ctypes.c_double, ctypes.c_int, ctypes.c_int]
		self.Cderivates.restype = None
		self.Cjacobian.argtypes = [self.p2, self.p1, self.p1, self.p2, self.p2, self.p1, self.p1, self.p1, self.p1, self.p1, ctypes.c_double, ctypes.c_int, ctypes.c_int, ctypes.c_double]
		self.Cjacobian.restype = None

		self.b = [0] * self.nb_s

	def scaling_param_simple(self, body_masses, params, T):  # eq. 5b
		vec = np.exp(params[0]) * np.power(body_masses, params[1]) * np.exp((params[2] * (self.t0 - T)) / (self.boltzmann * self.t0 * T))
		return vec

	def scaling_param_double(self, body_masses, params, T, g):  # eq. 5a
		# d * exp(...)
		numbers = np.exp(params[0]) * np.exp((params[3] * (self.t0 - T)) / (self.boltzmann * self.t0 * T))
		# then (adjacency matrix * lines multiplication with prey biomasses) * col multiplication with preb biomasses
		m = (self.adjacency * np.power(body_masses, params[1])[:, np.newaxis]) * np.power(body_masses, params[2])[np.newaxis, :]
		return m * numbers

	def interference_comp(self, body_masses, biomasses, params, T):
		biomasses[biomasses < 0.] = 0.
		indiv = biomasses / body_masses
		indiv[indiv < 1.0] = 1.0

		vec = np.exp(params[0]) * np.power(body_masses, params[1]) * (indiv - 1) * np.exp((params[2] * (self.t0 - T)) / (self.boltzmann * self.t0 * T))
		return vec

	# def equations(self):
	# 	Cderivates_call(self, init, time)

	# integration using the switch method
	def Cderivates_call(self, biomasses, t):  # body_masses, mat_a, mat_th, ri, ki, xi, q):
		self.ci = self.interference_comp(self.body_masses, biomasses, self.parameters['ci'], self.t)
		self.Cderivates(self.db, biomasses, self.mat_a, self.mat_th, self.ri, self.ki, self.xi, self.ci, self.efficiencies, self.q, self.nb_s, self.nb_b)
		return self.db

	# integration using specified method
	def Cderivates_call_spe(self, t, biomasses):
		self.ci = self.interference_comp(self.body_masses, biomasses, self.parameters['ci'], self.t)
		# time.sleep(5)
		self.Cderivates(self.db, biomasses, self.mat_a, self.mat_th, self.ri, self.ki, self.xi, self.ci, self.efficiencies, self.q, self.nb_s, self.nb_b)
		return self.db

	def jacobian(self, biomasses):
		# estimate jacobian using the five-point stencil numerical approach
		# f'(x) = (-f(x+2h) + 8f(x+h) - 8f(x-h) + f(x-2h))/12h + O(h^4)
		# h is set as sart(eps)*x (x !=0) and a classical precision for the scale is eps of the order 2.2*10e-16

		# eps = 2.2 * 1e-14
		# h = np.sqrt(eps) * bioms
		temp = np.exp((self.parameters['ci'][2] * (self.t0 - self.t)) / (self.boltzmann * self.t0 * self.t))
		# self.ci = self.interference_comp(self.body_masses, biomasses, self.parameters['ci'], self.t)
		biomasses[biomasses < 0.] = 0.
		self.Cjacobian(self.jacob, biomasses, self.body_masses, self.mat_a, self.mat_th, self.ri, self.ki, self.xi, self.ci, self.efficiencies, self.q, self.nb_s, self.nb_b, temp)
		return self.jacob
