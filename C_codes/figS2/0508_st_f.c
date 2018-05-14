// Mulple generations pop model for within/among discussion 0603, 2017
/* To execute
 gcc 0814_rmt.c gen_beta.h gen_beta.c -lm

 gcc 0508_st_f.c gen_beta.h gen_beta.c -lm -stdlib=libstdc++
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <time.h>
#include "gen_beta.h"
#include "../dSFMT-src-2.2.3/dSFMT.c"

// Functions
void message_error(char error_text[]);
double *d_vector(long size); //vector creator
double **d_matrix(long size_row, long size_column); //matrix creater
void free_d_vector(double *x);
void free_d_matrix(double **x);
double mean (double x[], int length);
double var (double x[], int length);

// Global variables


// Main function
int main(void){
	// non-specified variables
		int i,j,k,s,t,u;				// For loop counters
		clock_t start_t, end_t;			// Clock
	// Generation settings
		int N_rep= 10000;				// Repetitions (not effective here)
	// Breeding settings
		// Breeding general settings
		int c_max= 11;					// Upper limit of clutch size <- leave one additional space
		int N_stg= 11;					// Number of strategies
		double *st= d_vector(N_stg);	// Vector of strategies
		for(i=1; i<=N_stg; i++) st[i]= i*c_max/N_stg;		// Filling the values (1:10)
		double lw=250.0;				// Basic energy reserve
		int T= 1500;					// Season length
		int tr= 24;						// Incubation period
		int s1= 1; 						// Switch of compensating foraging
		int s2= 1;						// Switch of environmental fluctuation (Nest failure)
		int s3= 0;						// Switch of adult mortality (winter survival)
        int s4= 1;						// Switch of clutch size dependent predation
        int s5= 0;                      // Switch of clutch size specific breeding cost
        int s6= 1;                      // SWitch of reducing clutch size
		// Breeding energetic settings
		double c_n= 0.0;				// Nesting cost
		double c_b= 0.0;				// Cost of creating each offspring
		double c_r= 1.0;				// Cost of caring an offspring per step
		double f= 1.0;					// Foraging efficiency
        int T_reduce= 10000;             // Time when clutch size reduce to 1
	// Stochasticity generators
		// Environmental change (Nest failure)
		double o_m= 0.97;						// Average survival rate (for clutch size= 1)
		double o_v= 0.01;						// Coefficient related to variance
		double o_a= o_m/o_v, o_b= (1-o_m)/o_v;	// Parameters of Beta distribution
		double o_coe= 0.1;						// Coefficient of clutch-size dependent nest predation
		double o_npc= (1-o_m)*o_coe/(N_stg-2)*s4;// Unit difference in mean failure rate
		double o_ms= 0.0;						// temp space for actual mean survival rate
	// Recording and state variables
		int t1, t2, p1, p2, p3, p4, p5;		// t1: reproduction counter, p1: offspring counter, p2: offspring adder, p3: death counter, p4: fledgling counter, p5: print temp
        double p6;
		double pp= 0.0, e= 0.0;			// pp: Temp space of random number, e: Energy state
		double thr= 0.0;				// Current environmental condition
		int ee, c, r;					// ee: number of offspring, c: current strategy (clutch size), r: forag/ reproduce switch
	// Output
		FILE *out, *sum;
		out= fopen("out.txt","w");
	// Initialization
		// Beta generator
		gen_beta_param o_p;				// nest failure parameters
    	// Random number genertor
		int seed;
		dsfmt_t dsfmt;
		seed= time(NULL);
		if(seed==0)seed= 1;
		dsfmt_init_gen_rand(&dsfmt,seed);
        
		fprintf(out,"%d steps, %d repititions, hazard %lf, tr %d\n",T,N_rep,1-o_m,tr);
		// Set initial population

		// fprintf(out,"clutch size\t");

        fprintf(out,"food\t");
		for(t=1; t<= N_stg; t++){
			// p5= st[t];
            p6= (st[t]-1)*0.5+1;
			if(t<N_stg) fprintf(out, "%f\t", p6);
			else fprintf(out, "%f\n", p6);
		}
	// Start of Simulation
	start_t= clock();
	for(k= 1; k<= N_rep; k++){		// Each repetition
		fprintf(out,"R%d\t\t",k);
		for(j= 1; j<= N_stg;j++){	// Each strategy
			// Initialization of an individual
                p1= 0;					// reset offspring counter
                e= lw;
                ee= t1= t2= r= 0;
                /*
                o_coe= st[j]*0.1;
		        o_npc= (1-o_m)*o_coe/(N_stg-2)*s4;// Unit difference in mean failure rate
                */
                c= 9;
                f= (st[j]-1)*0.5+1;
            // Redefine mean failure rate for specific clutch size
                o_ms= o_m-c*o_npc;
                o_a= (o_ms)/o_v;
                o_b= (1-o_ms)/o_v;	// Parameters of Beta distribution
                gen_beta_initialize(&o_p, o_a, o_b);
            // Specific breeding cost per unit offspring
                if(s5==1) c_r= 1+ c/5;
			// Core loop, one season, one individual
			for(i=1; i<=T; i++){	// i is current time
                if (s6==1){
                    if (c>1){
                        c= round(c- c*i/T_reduce);
						if (c<=0) c=1;
                    }
                }
                // Foraging decision
				if(r<1){
					if(e>= (lw+ c_n+ c*c_b+ c*c_r*tr)){
						e= e- c_n- c*c_b;
						ee= c;
						r= 1;
						t1= i;
					}
					else e+= f;
				}
				if(r==1 && i> t1) e= e- c*c_r+ f* (1-c/c_max)* s1;// Cost of breeding
				if(r==1 && i>t1 && s2==1){// If there is environmental fluctuation
					thr= gen_beta(&o_p);
                    // printf("%lf\n",thr);
                    // pp= gen_unif(&o_p);
					pp= dsfmt_genrand_open_open(&dsfmt);
					if(pp>thr){			// Nest failure
						r=0;
						t1=0;
						ee= 0;
						t2= i;
					}
				}
				if(r==1 && i>= t1+ tr){// && i>= T- tr- c*c_n/f	// Success
					p1+= ee;		// Number of offspring within this season
					r= 0;
					ee= 0;
					t2= i;
				}
				if (i == T) p5= T- t2; // Duration of wasted time
			}						// End of season
			if(j<N_stg) fprintf(out,"%d\t",p1);
			else fprintf(out,"%d\n",p1);
		}// End of population
	}// End of one generation (individuals, strategy)
	/*
		// Print current population size
		fprintf(out,"G%d\t\t",s);
		for(t=1; t<= N_stg; t++) {
			if(pop[t]<0) pop[t]=0;
			p5= pop[t];
			if(p5>100*pop_0) {
				printf("Dynamics exploded at generation %d.\n",s);
				goto END;
			}
			if(t<= N_stg) fprintf(out, "%d\t", p5);
			//else fprintf(out, "%d\n", p5);
			off[t]= 0;
			mor[t]= 0;
		}
	*/

	free_d_vector(st);
	fclose(out);
	fclose(sum);
    end_t= clock();
    t= (end_t-start_t)/1E+6;
    printf("The simulation lasts %d seconds.\n",t);
	return 0;
}

// Function descriptions
void message_error(char error_text[]) //standard error handler
{
	printf("There are some errors...\n");
	printf("%s\n",error_text);
	printf("...now existing to system...\n");
	exit(1);
}
double *d_vector(long size) 
{
	double *x;

	x= (double *) malloc((size_t)((size+1)*sizeof(double)));
	if(x==NULL) message_error("Allocation failure in d_vector()");
	return x;
}
double **d_matrix(long size_row, long size_column)
{
	double **x;
	long i;
	long size_row_P= size_row+1;
	long size_column_P= size_column+1;

	x= (double **) malloc((size_t)(size_row_P*sizeof(double *))); //first dimension
	if (x==NULL) message_error("Allocation failure in d_vector()");
	x[0]= (double *) malloc((size_t)(size_row_P*size_column_P*sizeof(double))); //second dimension
	if (x[0]==NULL) message_error("Allocation failure in d_vector()");
	for(i=1;i<size_row_P;i++) x[i]= x[0]+ i*size_column_P;
	return x;
}
void free_d_matrix(double **x)
{
	free(x[0]);
	free(x);
}
void free_d_vector(double *x) {	free(x);}
double mean (double x[], int length)
{
	int i;
	double tmp=0.0;
	for (i=1; i<= length; i++){
		tmp+= x[i];
	}
	return tmp/length;
}
double var (double x[], int length)
{
	int i;
	double mean, t1, t2;
	mean= t1= t2= 0;
	for(i=1; i<= length; i++){
		t1+= x[i];
	}
	mean= t1/length;
	for(i=1; i<= length; i++){
		t2+= (x[i]-mean)*(x[i]-mean);
	}
	return t2/(length-1); // variance of samples, not matrix
}