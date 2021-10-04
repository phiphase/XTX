/**************************************************************************************************
* Copyright (C) 2018 Phiphase Limited. All rights reserved
*
* File: tb_modulator.cpp
* Description:
*   top-level of high speed modulator testbench
*
*************************************************************************************************/
// Optional Command line argument: -oqpsk: enables OQPSK mode.

#include <iostream>
#include <iomanip>
#include <iostream>
#include <fstream>
#include <cmath>
#include <complex>
#include <cstdio>
#include <vector>
using namespace std;

#include <hls_stream.h>

#include "pulse_shaper.h"
using namespace ppl;

#ifdef _FIXED_POINT_
#define ABS_ERR_THRESH 0.01
#else
#define ABS_ERR_THRESH 0.0001
#endif

typedef vector< complex<double> > cplx_vector_t;

// Function prototype for DUT.
void pulse_shaper_top(hls::stream<din_t> din[FSZ_DIN],
                      hls::stream<dout2_t>& dout,
                      phi_t dphi,
                      gain_t gain,
                      bool oqpsk,
                      bool alpha);

/****************************************************************************************************
 * THE TEST-BENCH
 ***************************************************************************************************/
int main(int argc, char* argv[])
{

	ifstream din_ifs("din.dat");           // Input data stream source file (from MATLAB)
	ifstream dout_ifs("dout.golden.dat");  // Comparison output sample source file (from MATLAB)
	ofstream dout_ofs("hw_dout.dat");      // Output file which can be imported into MATLAB.
	hls::stream<din_t> din[FSZ_DIN];       // An array of input streams
	hls::stream<dout2_t> dout("dout");
	phi_t dphi = DPHI;                     // defined in modulator.h
	gain_t gain = gain_t(1.0);             // Gain control input
	cplx_vector_t sw_result;
	cplx_vector_t hw_result;
	unsigned int err_cnt = 0;
	bool oqpsk = false;
	bool alpha = ALPHA_0p35;

	// Flag indicating OQPSK modulation enabled. Only one argument so we don't
	// have to scan argv[].
	int i = 1;
	while(i < argc)
	{
		if(string(argv[i]) == "-oqpsk") {
			oqpsk = true;
		}
		else if(string(argv[i]) == "-gain") {
			gain = ::atof(argv[++i]);
		}
		else if(string(argv[i]) == "-alpha") {
			alpha = ::atoi(argv[++i]);
		}
		i++;
	}

	if (din_ifs.bad())
	{
	   cout << "input file read error" << endl;
	   din_ifs.close();
	   return(1);
	}
	if (dout_ifs.bad())
	{
   	   cout << "input file read error" << endl;
   	   dout_ifs.close();
   	   return(1);
	}
	if (dout_ofs.bad())
	{
   	   cout << "output file read error" << endl;
   	   dout_ofs.close();
   	   return(1);
	}

	// Read the input file of symbols into the din stream.
	// We read a double, then explicitly cast to din_t
	// because there appears to be an exception thrown when the last line of a file
	// that just as a CR/LF on it gets read.
	/*
   double ctmp;
   din2_t din2;
   while(din_ifs) {
	   for(int i=0; i < FSZ_DIN; i++) {
		   din_ifs >> ctmp;
		   din2.s[i] = din_t(ctmp);
	   }
	   din << din2;
   }
   din_ifs.close();
   */
   double ctmp;
   int nSym = 0;
   while(din_ifs) {
   	   din_ifs >> ctmp;
   	   if(nSym % 2)
   		   din[1] << din_t(ctmp);
   	   else
   		   din[0] << din_t(ctmp);
   	   nSym++;
  }
  din_ifs.close();


   // Read the golden output reference data from a file.
   while(dout_ifs)
   {
	   complex<double> x;
	   dout_ifs >> x;
	   sw_result.push_back(x);
   }
   sw_result.pop_back();  // For some reason, one extra invalid line gets read. Remove the last element.



   // ****************** Run the DUT *********************
   while (!din[0].empty() && !din[1].empty()) {

	   // Run the DUT
	   pulse_shaper_top(din, dout, dphi, gain, oqpsk, alpha);
   }


   // ****************** Do the checking ******************
   // Write the output samples to hw_result[], reading from the hls::stream object.
   // We also write the data to an output file to enable the output to be analysed in MATLAB or Excel.
   dout_ofs << right << fixed << setbase(10) << setprecision(16);
   while(!dout.empty()) {
	   dout2_t y = dout.read();
	//   cout << setw(4) << setfill('0') << hex << short(y.y[3].imag()*16384);
	//   cout << setw(4) << setfill('0') << hex << short(y.y[2].imag()*16384);
    //   cout << setw(4) << setfill('0') << hex << short(y.y[1].imag()*16384);
    //   cout << setw(4) << setfill('0') << hex << short(y.y[0].imag()*16384);
	//   cout << setw(4) << setfill('0') << hex << short(y.y[3].real()*16384);
	//   cout << setw(4) << setfill('0') << hex << short(y.y[2].real()*16384);
	//   cout << setw(4) << setfill('0') << hex << short(y.y[1].real()*16384);
	//   cout << setw(4) << setfill('0') << hex << short(y.y[0].real()*16384) << endl;

	   for(int n=0; n < FSZ_DOUT; n++) {
#ifdef _FIXED_POINT_
		   hw_result.push_back(complex<double>(y.y[n].real().to_double(), y.y[n].imag().to_double()));
#else
	 	   hw_result.push_back(complex<double>(y.y[n].real(), y.y[n].imag()));
#endif
	 	   dout_ofs << real(y.y[n]) << "," <<  imag(y.y[n]) << endl;
	   }

   }
   dout_ofs.close();

   // Check results
   cout << "Checking results against a tolerance of " << ABS_ERR_THRESH << endl;
   cout << fixed << setprecision(5);
   for (unsigned i = 0; i < sw_result.size(); i++) {
      complex<double> abs_err = hw_result[i] - sw_result[i];

      if (abs(abs_err.real()) > ABS_ERR_THRESH || abs(abs_err.imag()) > ABS_ERR_THRESH) {
         cout << "Error threshold exceeded: i = " << i;
         cout << "  Expected: " << sw_result[i];
         cout << "  Got: " << hw_result[i];
         cout << "  Delta: " << abs_err << endl;
         err_cnt++;
      }
   }
   cout << endl;

   // Print final status message
   if (err_cnt) {
      cout << "!!! TEST FAILED - " << err_cnt;
      cout << " results out of tolerance." << endl;
   } else
      cout << "Test Passed" << endl;

   // Only return 0 on success
   return err_cnt;
}
