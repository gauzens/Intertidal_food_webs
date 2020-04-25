# pour que / soit division reelle meme entre deux entiers
from __future__ import division
import numpy as np
import networkx as nx
import traceback
from random import choice
try:
    import matplotlib.pyplot as plt
except:
    pass


def graph2fw(g):
	'''
	define some basic ecological property for graph g
	'''
	if not nx.is_directed(g):
	    # check more carefuly exeptions type in python
	    raise AttributeError('graph input is not directed')

	g.graph['nb_t'] = 0
	g.graph['nb_i'] = 0
	g.graph['nb_b'] = 0
	g.graph['nb_s'] = g.number_of_nodes()
	g.graph['mat'] = nx.to_numpy_matrix(g).astype(float)

	for i in g.nodes_iter():
	    g.node[i]['index'] = i
	    if g.in_degree(i) == 0:
	        g.node[i]['status'] = 'basal'
	        g.graph['nb_b'] = g.graph['nb_b'] + 1
	    # a top species can be a canibalist species
	    elif (g.out_degree(i) == 0) or (g.out_degree(i) == 1 and g.successors(i) == [i]):
	        # g.graph['non_basales']
	        g.node[i]['status'] = 'top'
	        g.graph['nb_t'] = g.graph['nb_t'] + 1
	    else:
	        g.node[i]['status'] = 'intermediate'
	        g.graph['nb_i'] = g.graph['nb_i'] + 1

	stat = nx.get_node_attributes(g, 'status')

	# renvoie l'ensemble des especes basales
	g.graph['basales'] = [n for n in g.nodes_iter() if stat[n] == 'basal']
	g.graph['tops'] = [n for n in g.nodes_iter() if stat[n] == 'top']
	g.graph['non_basales'] = [n for n in g.nodes_iter() if stat[n] != 'basal']

	g.graph['LD'] = g.size() / g.number_of_nodes()  # link density
	g.graph['C'] = g.size() / (g.number_of_nodes() *
	                           g.number_of_nodes())  # connectance
	# g.graph['mean_gene'] = g.size() / (g.number_of_nodes() - g.graph['nb_b'])
	g.graph['mean_TL'] = 'NA'

	return(g)


def add_vector_info(g, attr_name, vec):
    '''
    suppose that vector of attributes is sorted like species in the graph
    warning: only for np vectors... change name to add numpy infos?
    for list, networkx library has its own function
    '''
    if np.size(vec) != g.number_of_nodes():
        print 'error, length of information vector differs from nummber of species'
        return
    j = 0
    for i in g.nodes_iter():
        g.node[i][attr_name] = vec[j]
        j = j + 1


def TL(g):
    # trophic_level. Method from levine 1980.
    # Q is the interaction matrix between non basal species.

    Q = np.array(nx.to_numpy_matrix(g, g.graph['non_basales']))

    # sums of influxes set to one (! still take into account for energy from
    # basal species)
    som = [g.in_degree(n, weight='weight') for n in g.graph['non_basales']]

    # conversion en array necessaire pour la division ensuite... comprends as
    # bien pourquoi
    som = np.array(som)

    # in Levine, interactions are row: pred, col: prey,  so:
    Q = np.transpose(Q)
    Q = Q / som[:, None]

    Id = np.identity(g.graph['nb_s'] - g.graph['nb_b'])
    try:
        TL = np.linalg.inv(Id - Q)

        TL = np.sum(TL, axis=1) + 1

        c = 0
        for i in g.graph['non_basales']:
            g.node[i]['TL'] = TL[c]
            c = c + 1

        for i in g.graph['basales']:
            g.node[i]['TL'] = 1

        # mean trophic level of the web. TL of basal species is 1
        g.graph['mean_TL'] = (sum(TL) + g.graph['nb_b']) / g.graph['nb_s']
        return 1
    except:
        g.graph['mean_TL'] = 'NA'
        for i in g.nodes_iter():
            g.node[i]['TL'] = 'NA'
        return 0

def omnivory(g):
	'''
	Set omnivory for each species, adding a them attribute 'oi'
	Set mean graph omnivory using graph attribute 'mean_oi'
	Omnivory of a species is defined as the variance of its prey Trophic Level
	'''
	if g.graph['mean_TL'] == 'NA':
	    # pas super propre... voir pour lever une exception
	    # print 'error: no trophic levels'
	    g.graph['mean_oi'] = 'NA'
	    for i in g.nodes_iter():
	        g.node[i]['oi'] = 'NA'
	    return
	oi_tot = 0  # for calculation of mean food web omnivory
	for i in g.graph['basales']:  # omnivory of basal species is 0
	    g.node[i]['oi'] = np.nan

	for i in g.graph['non_basales']:
	    if g.node[i]['TL'] == 2:  # omnivory of pure herbivores is 0
	        g.node[i]['oi'] = 0
	    else:
	        # variance as (1/n * sum(xi^2)) - m^2. n = number of prey, m: mean
	        # trophic level of prey
	        v = 0
	        # mean of prey trophic level, by definition
	        m = g.node[i]['TL'] - 1
	        for j in g.predecessors_iter(i):
	            v = v + g.node[j]['TL'] * g.node[j]['TL']
	        g.node[i]['oi'] = v / g.in_degree(i) - m * m
	        oi_tot = oi_tot + g.node[i]['oi']
	g.graph['mean_oi'] = oi_tot / g.graph['nb_s']


def average_chain_length(g):
	'''
	compute acerage chain length between all nodes of the graph
	Warning, can be quite time consuming for large networks
	'''
	nb_chains = 0  # number of chains in the graph
	tot_len = 0  # total length of chains
	for i in g.graph['basales']:
	    for j in g.graph['tops']:
	    	#  get all chains between i and j
	        chains = nx.all_simple_paths(g, i, j)
	        for chain in chains:
	            nb_chains = nb_chains + 1
	            tot_len = tot_len + len(chain) - 1
	g.graph['mean_chain_length'] = tot_len / nb_chains


def page_rank(g):
	'''
	compute the google page ranking algorithm adapted by
	Allesina and Pascual 2009: Googling food webs. Plos Comp Biol
	as measure of species importance
	'''
	g.add_node('root')
	g.node['root']['status'] = 'root'
	for n in g.nodes_iter():
	    if g.node[n]['status'] == 'basal':
	        g.add_edge('root', n)
	    if n != 'root':
		    g.add_edge(n, 'root')

	adjacency = np.array(nx.to_numpy_matrix(g, dtype=float))
	# normalisation of the adjacency matrix
	col_sums = np.sum(adjacency, axis=0)
	stoc_adjacency = adjacency / col_sums[np.newaxis, :]

	# get eigens values and vectors
	w, v = np.linalg.eig(stoc_adjacency)
	eigs = np.real(v[:, 0])

	# delete value for the root node
	eigs = eigs[:-1]

	# normalisation of values
	eigs = eigs / np.sum(eigs)

	g.remove_node('root')
	add_vector_info(g, 'PageRankValue', eigs)


# a tester !!!! regarderles effets de bord...
def robustness(g, method='random', n_replicates=1, model=None, **kwargs):
    # in case of use of dynamics, first run to delete extinct species

    # def deleting_nodes(g, to_delete):
    # 	try:
    # 		m.delete_species(to_delete)
    # 	except AttributeError: # if no method delete exists in class model
    # 		g1.delete_nodes_from(to_delete) #delete nodes on g and rebuilt the class
    # 		m = mod.model(g1)

    def extinction_sequence(g1, method, model=None, **kwargs):
        # , node_list): # test what can be done using matrix approac (deleting rows col when sumcol = 0 and mat2graph)
        def extinction_chain(g1, succ, model, **kwargs):
            for n1 in succ:								# maybe the numpy function on matrices are faster...
                # obliger de egarder si n1 a pas deja ete enleve car
                # succ.remove(n1) non effectif (deja emplile?)
                if g1.nodes() == [] or n1 in node_list or g1.in_degree(n1) > 0:
                    return node_list
                # if g1.in_degree(n1) > 0:
                    # return node_list
                else:
                    node_list.append(n1)
                    if g1.out_degree(n1) == 0:
                        g1.remove_node(n1)
                        # print node_list
                        return node_list
                    else:  # si je n'ai plus de proies et des successeurs
                        next_set = g1.successors(n1)
                        g1.remove_node(n1)
                        extinction_chain(g1, next_set)  # , node_list)

        nb_extinct = np.zeros(g1.number_of_nodes())
        # g1 = g.copy()
        g1.remove_edges_from(g1.selfloop_edges())
        i = 0
        while g1.number_of_nodes() > 1:
            print 'i=', i
            # select the node to remove
            if method == 'random':  # random choice of node to delete
                selected_node = choice(g1.nodes())
            if method == 'max_connected':  # extinct most connected nodes first
                max_d = max(g1.degree().values())
                keys = [x for x in g1.nodes_iter() if g1.degree(x) == max_d]
                selected_node = choice(keys)
            if method == 'min_connected':  # extinct less connected nodes first
                min_d = min(g1.degree().values())
                keys = [x for x in g1.nodes_iter() if g1.degree(x) == min_d]
                selected_node = choice(keys)

            # proceding extinctions in networks
            # if species has no predators, no TOPOLOGICAL cascading extinctions
            if g1.out_degree(selected_node) == 0:
                if model is None:
                    g1.remove_node(selected_node)
                    nb_extinct[i] = 1
                else:
                    deleted = len(init[init == 0])
                    init[g1.node[selected_node]['index']] = 0
                    # g1.remove_node(selected_node)
                    res_dyn = dynamics(m.equations, time, steps, init)[-1, :]
                    to_delete = np.where(res_dyn < 10e-12)[0]
                    nb_extinct[i] = len(to_delete) - deleted
                    init[to_delete] = 0
                    deletion = ([])
                    for n in g1.nodes_iter():
                        if g1.node[n]['index'] in to_delete:
                            deletion.append(n)
                    if deletion:
                        print 'deleted node(s): ', deletion
                        g1.remove_nodes_from(deletion)
            else:
                if model is None:
                    succ = g1.successors(selected_node)
                    g1.remove_node(selected_node)
                    node_list = []
                    extinction_chain(g1, succ)  # , ([]))
                    removed = node_list
                    len(removed) + 1
                    nb_extinct[i] = len(removed) + 1
                else:
                    deleted = len(init[init == 0])
                    init[g1.node[selected_node]['index']] = 0
                    # print 'init1: ', init
                    res_dyn = dynamics(m.equations, time, steps, init)[-1, :]
                    # print res_dyn
                    to_delete = np.where(res_dyn < 10e-12)[0]
                    # print 'init2: ', init
                    # print 'res_dyn', res_dyn
                    # print 'to_delete1: ', to_delete
                    nb_extinct[i] = len(to_delete) - deleted
                    init[to_delete] = 0
                    deletion = ([])
                    for n in g1.nodes_iter():
                        # print 'rgadsgedtg', g1.node[n]['index'],' ',
                        # to_delete
                        if g1.node[n]['index'] in to_delete:
                            deletion.append(n)
                    if deletion:
                        print 'deleted node(s): ', deletion
                        g1.remove_nodes_from(deletion)
            i = i + 1
        return nb_extinct

    g1 = g.copy()
    nb = g1.number_of_nodes()
    # using ectinctions plus dynamic system from model
    if model is not None:
        mod = __import__(model)
        m = mod.model(g1)
        try:
            time = kwargs['time']
            steps = kwargs['step']
            init = kwargs['init']
        except:
            print "error: missing arguments for launching model"
            traceback.print_exc()
            return
        index = np.arange(nb)

        add_vector_info(g1, "index", index)
        # res_temp = dynamics(m.equations, time, steps, init)
        # get final species biomasses
        res_dyn = dynamics(m.equations, time, steps, init)[-1, :]
        # deleting nodes for modelling = give them a biomass of 0
        to_delete = np.where(res_dyn < 10e-12)[0]
        init[to_delete] = 0
        nb = g1.number_of_nodes() - (init == 0).sum()
        deletion = ([])
        for i in g1.nodes_iter():
            if g1.node[i]['index'] in to_delete:
                deletion.append(i)
        if deletion:
            g1.remove_nodes_from(deletion)
            print 'initial deletion: ', deletion

    res = np.zeros(nb)
    for i in range(0, n_replicates):  # would gain to be distributed...
        x = extinction_sequence(g1, method, model)
        res = res + x

    extinctions = res / n_replicates
    remaining = np.full(nb, nb) - np.cumsum(extinctions)

    robustness = np.sum(remaining) / (nb * nb)

    # What function should return? only number of secondary extinction or
    # secondary + the ones deleted "manually"?
    return robustness, extinctions


def dynamics(equation, time, step, init):
    from scipy.integrate import odeint
    vec_time = np.arange(0, time, step)
    res = odeint(equation, init, vec_time)
    return res


def niche_model(s, c):
    '''
    Create a graph accordingly to the niche model (Williams and Martinez 2000 Science)
    Correction from Allesina et al. 2008 are used
    s: number of species
    c: connectance
    '''

    def create_niche(s, c):
        # set species niche (ni)
        niche = np.random.rand(s)
        niche.sort()

        # attributing the feeding range, accordingly to the correction from Allesina et al. 2008, Science
        diet = np.random.beta(1, ((s - 1) / (2 * s * c)) - 1, size=s) * niche

        # computing centers, still using the Allesina correction
        center = [np.random.uniform(r / 2, n) if n + r / 2 <= 1 else np.random.uniform(r / 2, 1 - r / 2) for r, n in zip(diet, niche)]

        g = nx.DiGraph()
        species = np.arange(s)
        # print 'niche: ', niche
        # print 'diet: ', diet
        # print 'center: ', center

        for sp in species:
            g.add_node(sp)
            preys = species[((center[sp] - diet[sp] / 2 <= niche) & (niche <= center[sp] + diet[sp] / 2))]
            # print sp, preys
            for prey in preys:
                g.add_edge(prey, sp)
            g.node[sp]['niche'] = niche[sp]
        return g

    con_comp = 0
    while con_comp != 1:
        g = create_niche(s, c)
        con_comp = nx.number_connected_components(g.to_undirected())
    return g


def plot(g, **kwargs):  # can be largely optimised
    if g.graph['mean_TL'] == 'NA':
        print 'error: no trophic levels for plotting'  # pas super propre...
        return
    tlss = nx.get_node_attributes(g, 'TL')
    max_tl = max(nx.nodes(g), key=lambda(n): tlss[n])
    max_tl = tlss[max_tl]

    tls = np.array([])  # recupere un vecteur des trophic levels
    for n in g.nodes_iter():
        tl = g.node[n]['TL']
        tls = np.append(tls, round(tl))

    maxx = 0
    # print 'levels: ', range(1, int(max_tl) + 2)
    for i in range(1, int(max_tl) + 1):  # find the TL with the highest number of species
        nb = np.size(tls[tls == i])
        maxx = max(nb, maxx)

    for i in range(1, int(max_tl) + 2):  # on determine les positions pour chaque round(tl)
        nb = np.size(tls[tls == i])

        if nb == maxx:  # for centering species on x axis, p is the starting position on the x axis
            p = 0
        elif nb < maxx / 2:
            # if few species on an axis, space between them increases
            p = (maxx - nb) / 2. - 0.5 * nb
        elif nb < maxx / 4:
            p = (maxx - nb) / 2. - 1 * nb
        elif nb < maxx / 8:
            p = (maxx - nb) / 2. - 2 * nb
        else:
            p = (maxx - nb) / \
                2.  # half of species on the left of the center, half on the right

        for sp in g.nodes_iter():
            a = g.node[sp]['TL']
            if round(a) == i:
                if nb < maxx / 2:
                    p = p + 2
                elif nb < maxx / 4:
                    p = p + 4
                elif nb < maxx / 8:
                    p = p + 6
                else:
                    p = p + 1
                g.node[sp]['pos'] = (p, g.node[sp]['TL'])

    stat = nx.get_node_attributes(g, 'pos')
    fig = nx.draw_networkx(g, stat, **kwargs)
    cur_axes = plt.gca()
    cur_axes.axes.get_xaxis().set_visible(False)


def trophic_groups_fitness(groups):
    fitness = 0
    return groups, fitness


def genetic_clustering(g, nb_groups, fitness_function, pop_size=500, nb_generation=100, cross_over_prob=0.8, mut_prob=0.05):
    import multiprocessing as mp

    def offspring(population, fitness):
        fitness_np = np.array(fitness)
        new_pop = ([])
        couples = ([])

        # first, elitist selection of the 5% best individuals,
        # cf https://stackoverflow.com/questions/6910641/how-to-get-indices-of-n-maximum-values-in-a-numpy-array
        nb_inds = pop_size * 5 / 100
        best_5_index = np.argpartition(fitness_np, - nb_inds)[-nb_inds:]
        # make couples for these best 5%
        for i in nb_inds:
            couple = (population[best_5_index[i]], population[np.random.choice(indexes, size=1, replace=True, p=fitness_np)])
            couples.append(couple)
        # and then biased selection for the others
        for i in nb_groups / 2 - nb_inds:
            selection = np.random.choice(indexes, size=2, replace=True, p=fitness_np)
            couple = (population[selection[0]], population[selection[1]])
            couples.append(couple)

        # then, each couple give birth two two child with 1) crossing over and 2) mutation
        for couple in couples:
            new_pop.append(couple[0])
            new_pop.append(couple[1])

            # cross breeding
            if np.random.rand(1) < cross_over_probs:
                cross_points = np.random.randint(0, nb_s, 2)
                first_child = couple[0][:cross_points[0]] + couple[1][cross_points[0]:cross_points[0]] + couple[0][cross_points[1]:]
                second_child = couple[1][:cross_points[0]] + couple[0][cross_points[0]:cross_points[0]] + couple[1][cross_points[0]:]
            else:
                first_child = couple[0]
                second_child = couple[1]
            # mutations
            mut_probs = np.random.rand(nb_s)
            for i in range(nb_s):
                if mut_probs[i] < mut_prob:
                    first_child[i] = np.random.randint(0, nb_groups, 1)
            mut_probs = np.random.rand(nb_s)
            for i in range(nb_s):
                if mut_probs[i] < mut_prob:
                    second_child[i] = np.random.randint(0, nb_groups, 1)

            # Then adding new child to the new generation
            new_pop.append(first_child)
            new_pop.append(second_child)
        return new_pop

    def Hill_climbing(chromosome):
        # look to best solution in the neighboorhood, repeat until nothing is found
        # here, neighboorhood is the set of vectors with one difference
        # means that for each species I will try to put it in all groups
        # thus a lot of loops...
        fitness_max = fitness_function(chromosome)
        groups = np.arange(0, nb_groups, 1)
        optimising = True
        while optimising:
            optimising = False
            for i in range(0, nb_s):
                for new_group in groups:
                    if new_group != chromosome[i]:
                        chromosome_temp = chromosome
                        chromosome_temp[i] = new_group
                        fitness = fitness_function(chromosome_temp)
                        if fitness > fitness_max:
                            chromosome = chromosome_temp
                            fitness_max = fitness
                            optimising = True
        return chromosome, fitness

    # # Main function
    # # nb_s = g.number_of_nodes()
    # nb_s = 10
    # population = np.random.randint(0, nb_groups, size=(pop_size, nb_s))
    # print type(population)
    # population = population.tolist()
    # print type(population[0])
    # print population
    # # a = Hill_climbing(population[0])

    # p = mp.Pool(mp.cpu_count())

    # # first, running the Hill climbing procedure
    # population, fitness = p.map(Hill_climbing, population)
    # population, fitness = p.map(fitness_function, population)

    # # then, genetic algorithm
    # indexes = np.range(0, nb_s, 1)
    # # need to check for a proper termination
    # for i in range(200):
    #     population = offspring(population, fitness)
    #     population, fitness = p.map(fitness_function, population)
    # return population, fitness

# genetic_clustering(5, 12, trophic_groups_fitness, pop_size=4, nb_generation=2, cross_over_prob=0.8, mut_prob=0.05)
