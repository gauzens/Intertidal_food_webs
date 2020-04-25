#include <python2.7/Python.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
// C code for functional response fram Binzer et al. 2016

// compile with: 
// gcc db_binzer_2016_interference.c  -shared -Wl,-soname,db_binzer_2016_interference   -lpython2.7 -L/usr/include/gsl -lgsl  -lgslcblas -o db_binzer_2016_interference.so -fPIC


void db(double *db, double *bioms, double *mat_a, double *mat_th, double *ri, double *ki, double *xi, double *ci, double *ei, double q, int nb_s, int nb_b){
                
double sumF, sum, sum2, xx;	
int i, j, k;


double fij(int i, int j){
	sumF = 0;
	for (k = 0; k < nb_s; k++){
		if (mat_a[k * nb_s + i] > 0) {
			sumF = sumF + mat_th[k * nb_s + i] * mat_a[k * nb_s + i] * pow(bioms[k], q);
		}
	}
	return(mat_a[j * nb_s + i] * pow(bioms[j], q)) / (1 + sumF + ci[i]);	
}

	/*
	db: values of derivates
	bioms: vector of biomasses
	bodyM: vector of body masses
	mat_a,mat_th: from eq. 5a
	ri, ki, xi: from eq. 5b
	q: hill exponent
	nb_s, nb_b: number of sepcies, basal
	ei: vector of efficiencies
	*/

	//mat[ligne * dimension + colonne] => mat[prey * dimension + pred]
	// first, basal species	
	for (i = 0; i < nb_b; i++){
		if (bioms[i] < 0){
			bioms[i] = 0;
		}
	}

	for (i = 0; i < nb_b; i++){ 
		db[i] = 0;
		if (bioms[i] > 1e-12){
			// what is eaten from basal species i
			sum = 0;
			for (j = nb_b; j < nb_s; j++){ // set of potential predators
				if ((mat_a[i * nb_s + j] > 0) && (bioms[j] > 1e-12 )){
					sum = sum + bioms[j] * fij(j,i);
					// printf("\t %i:", j);
				}
			}
			// value of the derivate
			db[i] = ri[i] * bioms[i] * (1 - bioms[i]/ki[i])	- sum;
		}
	}

	// then non basal
	for (i = nb_b; i < nb_s; i++){
		db[i] = 0;
		if (bioms[i] > 1e-12) {
			// inputs
			sum = 0;
			for (j = 0; j < nb_s; j++){ // j: set of preys
				if ((mat_a[j * nb_s + i]) > 0 && (bioms[j] > 1e-12 )){
					sum = sum + ei[j] * bioms[i] * fij(i,j);
				}
			}
			// outputs
			sum2 = 0;
			for (j = nb_b; j < nb_s; j++){ // j: set of predators
				if ((mat_a[i * nb_s + j] > 0) && (bioms[j] > 1e-12 )){
					sum2 = sum2 + fij(j,i) * bioms[j];
				}
			}
			db[i] = sum - sum2 - xi[i]*bioms[i];
		}
	}
	// db[0] = 0;
	// db[nb_b] = 0;
}



/*
estimate jacobian using the five-point stencil numerical approach
f'(x) = (-f(x+2h) + 8f(x+h) - 8f(x-h) + f(x-2h))/12h + O(h^4)
h is set as sart(eps)*x (x !=0) and a classical precision for the scale is eps of the order 2.2*10e-16
TODO:
Use an internal copy of the codes from db. need thereafter to make a third function that distibute calculation on index i maybe
*/
void jacobian(double *jac, double *bioms, double *BodyM, double *mat_a, double *mat_th, double *ri, double *ki, double *xi, double *ci, double *ei, double q, int nb_s, int nb_b, double temp){
	

	double derivates(int i, double *bioms, double *BodyM, double *mat_a, double *mat_th, double *ri, double *ki, double *xi, double *ci, double *ei, double q, int nb_s, int nb_b, double temp){

		double sumF, sum, sum2, xx, c, db;	
		int j, k;


		double max(double a, double b){
			if (a>b) {return a;}
			else {return b;}
		}

		double fij(int i, int j){
			sumF = 0;
			c = pow(BodyM[i], 0.5)*(max(bioms[i]/BodyM[i], 1.0) - 1) * temp;
			for (k = 0; k < nb_s; k++){
				if (mat_a[k * nb_s + i] > 0) {
					sumF = sumF + mat_th[k * nb_s + i] * mat_a[k * nb_s + i] * pow(bioms[k], q);
				}
			}
			return(mat_a[j * nb_s + i] * pow(bioms[j], q)) / (1 + sumF + c);	
		}

		//mat[ligne * dimension + colonne] => mat[prey * dimension + pred]
		//basal species	
		if (i < nb_b){ 
			db = 0;
			if (bioms[i] > 1e-11){
				// what is eaten from basal species i
				sum = 0;
				for (j = nb_b; j < nb_s; j++){ // set of potential predators
					if ((mat_a[i * nb_s + j] > 0) && (bioms[j] > 1e-12 )){
						sum = sum + bioms[j] * fij(j,i);
						// printf("\t %i:", j);
					}
				}
				// value of the derivate
				db = ri[i] * bioms[i] * (1 - bioms[i]/ki[i])	- sum;
			}
		}

		//non basal
		else{
			db = 0;
			if (bioms[i] > 1e-11) {
				// inputs
				sum = 0;
				for (j = 0; j < nb_s; j++){ // j: set of preys
					if ((mat_a[j * nb_s + i]) > 0 && (bioms[j] > 1e-12 )){
						sum = sum + ei[j] * bioms[i] * fij(i,j);
					}
				}
				// outputs
				sum2 = 0;
				for (j = nb_b; j < nb_s; j++){ // j: set of predators
					if ((mat_a[i * nb_s + j] > 0) && (bioms[j] > 1e-12 )){
						sum2 = sum2 + fij(j,i) * bioms[j];
					}
				}
				db = sum - sum2 - xi[i]*bioms[i];
			}
		}
		return db;
	}


	 //vectors containing biomasses values +-h or 2h 
	double *hs = calloc(nb_s, sizeof(double));
	double *hs2 = calloc(nb_s, sizeof(double));
	double *mhs = calloc(nb_s, sizeof(double));
	double *mhs2 = calloc(nb_s, sizeof(double));


	// double containing the value of the function in (line, col) at the +-h
	double dbPlusH, dbPlus2H, dbMinus2H, dbMinusH;
	double eps = 2.2*1e-14;
	int i,j, line, col = 0;
	double h = 0;
	int species = 0;
	// set the value for the precision of numerical solution

	
	// mat[prey * dimension + pred]
	// get the values for the different f(x+h) terms
	// hs vectors set the small modification of biomasses vectors
	for (line = 0; line < nb_s; line++){		
		for (col = 0; col < nb_s; col++){
			h = sqrt(eps) * bioms[col];
			//compute d(line)/d(biomass(col))
			// first modify of (h, 2*h, ...)the biomass of the species in col
			for (j = 0; j < nb_s; j++){
				// printf("%i  %i\n", j, nb_s);
				if (j == col){
					hs[j] = bioms[col] + h;
					hs2[j] = bioms[col] + 2 * h;
					mhs[j] = bioms[col] - h;
					mhs2[j] = bioms[col] - 2*h;					
				}
				else{
					hs[j] = bioms[j];
					hs2[j] = bioms[j];
					mhs[j] = bioms[j];
					mhs2[j] = bioms[j];				
				}
			}
			// then compute derivate for all the species to complete a row of the jacobian (Di/Dspecies)	
			dbPlusH = derivates(line, hs, BodyM, mat_a, mat_th, ri, ki, xi, ci, ei, q, nb_s, nb_b, temp);
			dbPlus2H = derivates(line, hs2, BodyM, mat_a, mat_th, ri, ki, xi, ci, ei, q, nb_s, nb_b, temp);
			dbMinusH = derivates(line, mhs, BodyM, mat_a, mat_th, ri, ki, xi, ci, ei, q, nb_s, nb_b, temp);
			dbMinus2H = derivates(line, mhs2, BodyM, mat_a, mat_th, ri, ki, xi, ci, ei, q, nb_s, nb_b, temp);


			// fill the jacobian
			if (h >0){
				jac[line * nb_s + col] = (-dbPlus2H + 8*dbPlusH - 8*dbMinusH + dbMinus2H)/(12*h);
			}
			else{
				jac[line * nb_s + col] = 0;
			}

		}
	}


}