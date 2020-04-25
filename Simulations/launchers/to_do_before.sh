# Do before from terminal


# load python, what about the librairies?
	module load python/2
	# and create a virtual environment
	virtualenv ~/WarmingWebs
	#load virtual env	
	source ~/WarmingWebs/bin/activate
	# then installing librairies
	pip install --upgrade pip
	pip install numpy
	pip install networkx==1.11
	# there is a specific help section about numpy
	# then specific way for scipy
	export BLAS=/usr/local/lapack/3.5.0-2/lib/libblas.so
	export LAPACK=/usr/local/lapack/3.5.0-2/lib/liblapack.so
	export LDFLAGS="-shared $LDFLAGS"
	# only after that install scipy:
	pip install scipy
	module load gcc/4
    module load lapack/gcc
	# not sure I need to load gsl
	# module load gsl/1.16-3

	# I think I should compile my c library here, however, options might differs among nodes?
	gcc $CPPFLAGS $LDFLAGS db_binzer_2016_interference.c  \
      -shared -Wl,-soname,db_binzer_2016_interference \
      -mavx -msse -msse2 -mssse3 -msse4.1 -msse4.2 -mfpmath=sse \
      -lpython2.7 -o db_binzer_2016_interference.so -fPIC \
      -llapack -lblas
