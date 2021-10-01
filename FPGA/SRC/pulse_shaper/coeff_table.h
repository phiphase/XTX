/**************************************************************************************************
* Copyright (C) 2018 Phiphase Limited. All rights reserved
*
* File: coeff_table.h
* Description:
*     Coefficient table class
*
*************************************************************************************************/
#ifndef COEFF_TABLE_H_
#define COEFF_TABLE_H_
#include <cmath>
using namespace std;

namespace ppl {

	// Enumerated type for filter impulse response type
/*
	 typedef enum {
	   RC = 0, SRRC
	} filter_t;

	typedef enum {
		ALPHA_0p5 = 0, ALPHA_0p35
	} alpha_t;
*/
	template<class TC, int NS, int OSR, filter_t FT>
	class CoeffTable
	{
		TC coeff_a0[2][NS][NS*OSR];
		TC coeff_a1[2][NS][NS*OSR];
		const int Nwf;
		const double Ts;
		const int Ns;

		// Function to compute a coefficient value given alpha and k.
		double genCoeff(double alpha, unsigned k)
		{
			double x;
			double t = ((double)k - Nwf/2.0)/Ts;  // Normalised to Ts
			switch(FT) {
				case RC:
					if (t==0.0)  // Handle division by zero
						x = 1.0;
					else {
					 x = sin(M_PI*t)/(M_PI*t);    // sin(x)/x pulse
					 //double den = 1 - pow((2.0*alpha*t),2.0);
					 double den = 1 - (2.0*alpha*t)*(2.0*alpha*t);
					 if (den == 0)   // Handle division by zero.
						 x = 0;
					 else
						 x = x * cos(M_PI*alpha*t) / den;  // RC shaping applied
					}
					break;

				// If filter type is raised cosine (RC)
				case SRRC:
					if(t==0.0)
						x = 1.0 + alpha*(4.0/M_PI -1);
					else if (abs(t) == 1/(4.0*alpha))
						x = (alpha/sqrt(2.0)) * ( (1.0+2.0/M_PI ) * sin(M_PI/(4.0*alpha)) + (1-2.0/M_PI) * cos(M_PI/(4.0 * alpha)));
					else
						//x = (sin(M_PI * t * (1-alpha)) + 4.0 * alpha * t * cos(M_PI*t*(1+alpha))) / (M_PI*t * (1.0-pow((4.0*alpha*t),2.0)));
						x = (sin(M_PI * t * (1-alpha)) + 4.0 * alpha * t * cos(M_PI*t*(1+alpha))) / (M_PI*t * (1.0-(4.0*alpha*t)*(4.0*alpha*t)));
					break;

				// Unsupported pulse shape. Just make it a rectangle.
				default:
					x = 1.0;
			} // switch(FT_)
			return x;
		}

	public:
		CoeffTable () : Nwf(NS*OSR), Ts(OSR), Ns(NS)
		{
			for (int j = 0; j < NS; j++) {
				// Create coefficient, shifting by j*OSR modulo Nwf
				for(int k=0; k < Nwf; k++) {
				   coeff_a0[0][j][(k+j*OSR) % Nwf] = TC(genCoeff(0.5,k));     // Table 0, alpha = 0.5
				   coeff_a0[1][j][(k+j*OSR) % Nwf] = TC(genCoeff(0.35,k));    // Table 1, alpha = 0.35
				} // end for k

				// Create the slope coefficient, shifting by j*OSR modulo Nwf.
				for(int k=0; k < Nwf; k++) {
					coeff_a1[0][j][k]  = coeff_a0[0][j][(k+1) % Nwf] - coeff_a0[0][j][k];
					coeff_a1[1][j][k] = coeff_a0[1][j][(k+1) % Nwf] - coeff_a0[1][j][k];
				}

			} // end for j

		} // CoeffTable() constructor

		// Access methods non-synthesis version has asserts to check the address range.
#ifndef __SYNTHESIS__
		TC getA0(int i, int j, int k) { assert((j < Ns) && (k < Nwf) && ((i==0) || (i==1))); return coeff_a0[i][j][k];}
		TC getA1(int i, int j, int k) { assert((j < Ns) && (k < Nwf)); return coeff_a1[i][j][k];}
#else
		 TC getA0(int i, int j, int k) {return (coeff_a0[i][j][k]);}
		 TC getA1(int i, int j, int k) {return (coeff_a1[i][j][k]);}
#endif
	};

}; // namespace ppl
#endif //COEFF_TABLE_H_
