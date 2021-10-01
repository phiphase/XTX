/**************************************************************************************************
* Copyright (C) 2018 Phiphase Limited. All rights reserved
*
* File: pulse_shaper.h
* Description:
*   constant and type definitions file for pulse shaper (pulse_shaper_top.cpp)
*   Include this file where pulse_shaper_top is used.
*
*************************************************************************************************/
#pragma once

#include <complex>
#include <ap_fixed.h>

#define _FIXED_POINT_     // Define if we are using fixed point types

namespace ppl {

// Enumerated type for filter impulse response type
typedef enum {
   RC = 0, SRRC
} filter_t;

/*
typedef enum {
	ALPHA_0p5 = 0, ALPHA_0p35
} alpha_t;
*/

// Test parameters

const filter_t FILTER_TYPE = RC;           // Set the filter type (RC or SRRC)
const unsigned FSZ_DOUT   = 4;             // Frame size (number of samples processed per iteration)
const unsigned FSZ_DIN    = 2;             // data input (symbol) frame size needs to be FSZ_DOUT/2
const unsigned OSR        = 8;             // Over-sampling ratio
const unsigned NS         = 8;             // Number of symbol periods represented by the waveform
const unsigned NWF        = OSR*NS;        // Waveform length
const   double ALPHA      = 0.5;           // pulse shaping filter roll-off factor
const   double Rs         = 230;           // Symbol rate
const   double Fs         = 500.0;         // Output sample rate
const   double R0         = Fs/OSR;        // Fundamental waveform frequency
const   double DPHI       =  Rs/R0;        // symbol phase increment
const unsigned LOG_NWF    = 6;             // log2(NWF)
const unsigned NCO_W      = 32;            // symbol phase accumulator width
const unsigned NCO_I      = LOG_NWF;       // number of integer bits for symbol phase
const unsigned NCO_F      = NCO_W - NCO_I; // number of fractional bits for symbol phase
const unsigned SYM_W      = 12;            // phase-mapped complex symbol width
const unsigned DOUT_W     = 16;            // output sample width
const unsigned COEFF_W    = 12;            // coefficient width
const unsigned R_W        = COEFF_W+2;     // Intermediate, interpolated sample width
const unsigned ACC_W      = COEFF_W+2;     // symbol accumulator width
const unsigned GAIN_W     = 8;             // Gain control width
const bool     ALPHA_0p5  = 0;
const bool     ALPHA_0p35 = 1;


// Define types for input, output, NCO and coefficients

#ifdef _FIXED_POINT_

typedef ap_ufixed<GAIN_W,0, AP_RND,AP_SAT>            gain_t;     // gain control type 8 bit unsigned. 0 - 0.997
typedef ap_fixed<SYM_W,1,AP_RND,AP_SAT>               ap_sym_t;
typedef std::complex<ap_sym_t>                        sym_t;      // input symbols are complex
typedef ap_fixed<COEFF_W,1, AP_RND, AP_SAT>           coeff_t;    // Coefficient table
typedef ap_fixed<R_W,2, AP_RND>                       r_t;        // r is an intermediate register that holds the interpolated coefficients.
typedef ap_fixed<ACC_W,2, AP_RND>                     acc_ap_t;
typedef std::complex<acc_ap_t>                        acc_t;      // Accumulator
typedef ap_fixed<DOUT_W,2,AP_RND>                     ap_dout_t;
typedef std::complex<ap_dout_t>                       dout_t;     // output samples are complex
typedef ap_ufixed<NCO_W, NCO_I, AP_RND, AP_WRAP>      phi_t;      // NCO phase accumulator
typedef ap_uint<NCO_I>                                k_t;        // NCO integer type (table index)
typedef ap_ufixed<NCO_F,0>                            f_t;        // NCO fraction type (for interpolation)
typedef ap_uint<3>                                    din_t;      // Phase state input type


#else

typedef float                                         gain_t;
typedef float                                         ap_sym_t;
typedef std::complex<ap_sym_t>                        sym_t;     // input symbols are complex
typedef float                                         coeff_t;   // Coefficient table
typedef float                                         r_t;
typedef float                                         acc_ap_t;
typedef std::complex<acc_ap_t>                        acc_t;     // Accumulator
typedef float                                         ap_dout_t;
typedef std::complex<ap_dout_t>                       dout_t;    // output samples are complex
typedef float                                         phi_t;     // NCO phase accumulator
typedef float                                         f_t;       // NCO fraction type (for interpolation)
typedef ap_uint<NCO_I>                                k_t;       // NCO integer type (table index)
typedef unsigned                                      din_t;     // Phase state input type


#endif


// Structure for maintaining two NCOs for two sample streams
typedef struct {
	signed sc;
	signed dsc;
	phi_t phi;
	k_t k;
	k_t kq;
	f_t f;
	unsigned symIdx;
} nco_t;

// Input structure has FSZ_DIN symbols packed into each input word
//typedef struct { din_t s[FSZ_DIN]; } din2_t;

// Output structure has FSZ samples packed into each output word
typedef struct { dout_t y[FSZ_DOUT]; } dout2_t;
} // namespace ppl

