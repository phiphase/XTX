/**************************************************************************************************
 * Copyright (C) 2018 Phiphase Limited. All rights reserved
 *
 * File: pulse_shaper_top.cpp
 * Description:
 *   top-level of high speed pulse shaper
 *
 * This version of the pulse shaper is 2 in, 4 out i.e. it accepts 2 input symbols per clock cycle
 * on separate input streams and outputs 4 samples packed into an AXIS stream as 128 bits (4 x 32)
 * FSZ_DIN (frame size data input) = 2
 * FSZ_DOUT (frame size data output) = 4
 * NOTE: AT PRESENT THESE VALUES ARE HARD-CODED. DO NOT CHANGE FSZ_DIN AND FSZ_DOUT.
 *
 * Input Arguments
 *     hls::stream<din_t> din[FSZ_DIN]
 *       two input symbol streams of type ap_fifo
 *       din[0] is even numbered symbols,
 *       din[1] is odd numbered symbols.
 *       din_t is 3 bit unsigned integer representing the following phase states:
 *          0: Phase state A (0 deg)
 *          1: Phase state B (45 deg)
 *          2: Phase state C (90 deg)
 *          3: Phase state D (135 deg)
 *          4: Phase state E (180 deg)
 *          5: Phase state F (225 deg)
 *          6: Phase state G (270 deg)
 *          7: Phase state H (315 deg)
 *     hls::stream<dout2_t>& reference to output sample stream which is 128 bits wide, ap_fifo.
 *         The DATA_PACK directive has been applied to this type dout2_t:
 *            typedef struct { dout_t y[FSZ_DOUT]; } dout2_t;
 *         in which each element of y is a 32 bit complex number (16 bit real, 16 bit imag)
 *         The physical mapping of samples into bits is as follows:
 *             y[0].real mapped into bits 0 to 15
 *             y[1].real mapped into bits 16 to 31
 *             y[2].real mapped into bits 32 to 47
 *             y[3].real mapped into bits 48 to 63
 *             y[0].imag mapped into bits 64 to 79
 *             y[1].imag mapped into bits 80 to 95
 *             y[2].imag mapped into bits 96 to 111
 *             y[3].imag mapped into bits 112 to 127
 *     phi_t  dphi: 32 bit delta-phi which controls the symbol rate
 *     gain_t gain: 8 bit gain control 0-0.996
 *     bool   oqpsk: 1: OQPSK modulation mode; the Q-channel is delayed by 1/2 symbol.
 *                   0: QPSK or 8PSK
 *     bool   alpha: 0: filter roll-off factor = 0.5
 *                   1: filter roll-off factor = 0.35
 *                   Note: these are the only two options permitted.
 *  Return value:
 *      none
 *************************************************************************************************/
#include <iostream>
using namespace std;

#include "pulse_shaper.h"
#include "coeff_table.h"
using namespace ppl;

#include <hls_stream.h>


void pulse_shaper_top(hls::stream<din_t> din[FSZ_DIN], hls::stream<dout2_t>& dout, phi_t dphi, gain_t gain, bool oqpsk, bool alpha)
{
#pragma HLS DATA_PACK variable=dout
#pragma HLS INTERFACE axis register both port=dout
#pragma HLS PIPELINE II=1

static CoeffTable<coeff_t, NS, OSR, FILTER_TYPE> myCoeffTable;

// Phase state mapping table. This table maps the phase states 0-7 (A-H) to complex symbols.
const double RSQRT2     = 0.707106781186547; // 1/sqrt(2)
const sym_t pst[] = {
	sym_t(1,0),                // Phase state A (0 deg)
	sym_t(RSQRT2, RSQRT2),     // Phase state B (45 deg)
	sym_t(0,1),                // Phase state C (90 deg)
	sym_t(-RSQRT2, RSQRT2),    // Phase state D (135 deg)
	sym_t(-1,0),               // Phase state E (180 deg)
	sym_t(-RSQRT2,-RSQRT2),    // Phase state F (225 deg)
	sym_t(0,-1),               // Phase state G (270 deg)
	sym_t(RSQRT2, -RSQRT2)};   // Phase state H (315 deg)

static sym_t symBuf[NS];
#pragma HLS ARRAY_PARTITION variable=symBuf cyclic factor=2 dim=1
static nco_t nco = {0,0,0,0,0,0,0};
static sym_t sym[FSZ_DIN];
acc_t acc[FSZ_DOUT];
dout2_t myDout;
static bool doRead[FSZ_DIN] = {true,true};  // Force read on first iteration.

	// Do the read if required to refresh the cached symbol.
	read_loop: for(int n=0; n < FSZ_DIN; n++)
	{
#pragma HLS UNROLL
		if(doRead[n])
		{
			sym[n] = pst[din[n].read()];  // Cache the symbol and translate from phase state to complex number.
		}
		doRead[n] = false;
	}


	// Frame processing loop: iterates FSZ_DOUT times to generate FSZ_DOUT output samples. Consumes up to 2 input symbols per
	// per transaction.
	frame_loop: for(int l=0; l < FSZ_DOUT; l++)
	{

#ifdef _FIXED_POINT_
		nco.k = k_t(nco.phi);
		nco.kq = nco.k + (oqpsk ? OSR/2 : 0);
		nco.f = nco.phi;
		nco.dsc = nco.sc;
		nco.sc = nco.k % OSR;
#else
		nco.k = k_t(nco.phi);
		nco.kq = nco.k + (oqpsk ? OSR/2 : 0);
		nco.f = nco.phi - phi_t(nco.k);
		nco.dsc = nco.sc;
		nco.sc = signed(nco.k) % OSR;
#endif
		// Accumulate the NS symbols to form an output sample in acc.
		acc[l] = acc_t(0.0, 0.0);
		filter_loop1: for(int j=0; j < NS; j++)
		{
#pragma HLS UNROLL
			r_t ri = myCoeffTable.getA0(alpha,j,nco.k)  + myCoeffTable.getA1(alpha,j,nco.k)  * nco.f;
			r_t rq = myCoeffTable.getA0(alpha,j,nco.kq) + myCoeffTable.getA1(alpha,j,nco.kq) * nco.f;
			acc[l] = acc[l] + acc_t(symBuf[j].real() * ri, symBuf[j].imag() * rq);
		}
		myDout.y[l] = dout_t(acc[l].real()*gain, acc[l].imag()*gain);

#ifdef _FIXED_POINT_
		nco.phi = nco.phi + dphi;
#else
		nco.phi = fmod(nco.phi + dphi, NWF);
#endif


		// If the NCO wrapped modulo over-sampling ratio, then read in another symbol into the
		// symbol buffer from the cached symbol "sym" We read from either an "odd" stream or an "even" stream.
		// This is required because we sometimes have to read from two streams in a single clock cycle.
		if (nco.dsc - nco.sc > 0)
		{
			if(nco.symIdx % 2) {
				symBuf[nco.symIdx] = sym[1];
				doRead[1] = true;
			}
			else {
				symBuf[nco.symIdx] = sym[0];
				doRead[0] = true;
			}
			nco.symIdx = (nco.symIdx+1) % NS;
		}
	}

	// Write the output array to the stream.
	dout.write(myDout);
}
