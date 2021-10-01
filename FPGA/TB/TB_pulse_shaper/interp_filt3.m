%---------------------------------------------------------------------------------------------------
% Copyright (C) 2018 Phiphase Limited
%
% Interpolating polyphase pulse shaping filter - linear interpolation only but with offset for the
% Q channel. This version accepts a 3-bit input symbol indicating the phase state A-H with the 
% following mapping:
%   phase state   Z (8PSK) b1b0 (QPSK)   phase (deg)
%        A (0)    000                     0
%        B (1)    001       00            45    
%        C (2)    010                     90
%        D (3)    011       10            135
%        E (4)    100                     180
%        F (5)    101       11            225
%        G (6)    110                     270
%        H (7)    111       01            315
%
% Phase states B, D, F and H are used for QPSK/OQPSK.
%
%
% File: interp_filt3.m
%
% Input arguments:
%   s       : array of input symbols indicating phase state A-H as 0-7.
%   [param] : optional parameter structure (overrides the defaults)
%      Rs = 200;                     : symbol rate
%      Fs = 500;                     : Sample rate
%      OSR = 8;                      : over-sampling ratio
%      Ns = 8;                       : Number of symbols in the impulse response
%      Nstr = 1;                     : Number of processing streams
%      ft = 'Raised Cosine';         : filter type: 'Raised Cosine' or 'Square Root Raised Cosine'
%      alpha = 0.5;                  : roll-off factor
%      qOffset = 0;                  : Q-channel offset (set to OSR/2 for OQPSK)
%      gain = 1.0;                   : output gain 0-1.0
%      getDatFile = false;           : generate output data files
%      dinFilename = 'din.dat';      : input symbol data
%      doutFilename = 'dout.dat';    : output sample data
%      dbgFilename = 'dbg.dat';      : debugging log
%
% Output arguments:
%   y   : Nstr x n array of filtered output samples at sample rate Fs
%   h   : the impulse response of the filter.
%
% Description:
%   This function implements a pulse-shaping filter that converts a series of symbols 
%   to filtered, over-sampled pulses with arbitrary symbol rate.
%   Instead of using a conventional FIR filter, the filter operates by storing the impulse response
%   of the filter in a set of tables.  The impulse response is an over-sampled pulse shape e.g. raised
%   cosine or other characteristic.  It is Ns symbols long, and has an over-sampling ratio of OSR.
%   The fundamental table frequency (FTF) represents a symbol rate R0 of Fs/OSR where Fs is the
%   desired output sample rate.
%
%   An Ns symbol buffer stores the previous Ns symbols. Upon each output sample iteration,
%   symbol s(n-j) is multiplied with h(k-jOSR) where j=0:Ns-1, k is the table index (derived from
%   the integer part of the NCO accumulator).  The Ns products are then accumulated to form the
%   output sample.
%
%   An NCO is used to step a set of Ns pointers, each OSR samples apart. The NCO phase increment
%   determines the transmitted symbol rate. The NCO generates a carry-out every Nwf samples. When a
%   carry-out is generated, the symbol circular buffer is updated with the next symbol.
%
%---------------------------------------------------------------------------------------------------
function [y h] = interp_filt3(s, p_)

p.Rs = 200;
p.Fs = 500;
p.OSR = 8;
p.Ns = 8;
p.Nstr = 1;
p.ft = 'Raised Cosine';
p.alpha = 0.5;
p.qOffset = 0;
p.gain = 1.0;
p.pst = exp(1i*pi/4*(0:7)');
p.genDatFile = false;
p.dinFilename = 'din.dat';
p.doutFilename = 'dout.dat';

if nargin < 1 || nargin > 2
    error('interp_filt2: usage: y = interp_filt2(s, [param])');
end

% Parameter validation
names = fieldnames(p_);
for n=1:length(names)
    if isfield(p,names{n})
        p.(names{n}) = p_.(names{n});
    else
        error(['Invalid field found: ' names{n}]);
    end
end
if rem(p.Ns,1) || p.Ns <= 0
    error('Ns must be an integer > 0');
end
if rem(p.OSR,1) || p.OSR <= 0
    error('OSR must be an integer > 0');
end
if p.Rs > p.Fs
    error('Rs must be less than Fs');
end
if p.qOffset < 0 || p.qOffset > p.OSR
    error('offset must be in the range 0-OSR');
end
if p.gain < 0 || p.gain > 1.0
    error('gain must be in the range 0 to 1.0');
end
if p.Nstr < 1
    error('Frame size, Nstr must be >= 1');
end

% Design the filter.
% Create the raised cosine pulse in time domain - i.e. "manually".
% Proakis, "Digital Communications" equation 9.2-27
%Ts = OSR;
Nwf = p.OSR * p.Ns;
t=(-Nwf/2:Nwf/2-1)/p.OSR;  % t is normalised wrt OSR (Ts)

switch(p.ft)
    case 'Raised Cosine'
    h = sin(pi*t)./(pi*t);
    h(isnan(h)) = 1.0;
    h = h .* (cos(pi*p.alpha*t)./(1-(2*p.alpha.*t).^2));
    h(isinf(h))=0;
% Square Root Raised Cosine
    case 'Square Root Raised Cosine'    
    h = (sin(pi*t*(1-p.alpha)) + 4*p.alpha*t .* cos(pi*t*(1+p.alpha))) ./ ...
                          (pi*t .* (1-(4*p.alpha*t).^2));
     h(t == 0) = 1 + p.alpha*(4/pi - 1);
     h(abs(t) == 1/(4*p.alpha)) = (p.alpha/sqrt(2)) * ( (1+2/pi) * sin(pi/(4*p.alpha)) + ...
                                           (1-2/pi)*cos(pi/(4*p.alpha)));          
    otherwise
      error('Invalid pulse shape specified. Must be ''Raised Cosine'' or ''Square Root Raised Cosine'' ');
end

% MATLAB design
%d = fdesign.pulseshaping(p.OSR, 'Raised Cosine', 'Nsym,Beta', p.Ns, p.alpha);
%hf = design(d);
%h2 = p.OSR*hf.Numerator(1:end-1);

% Make the filter length even.
if mod(length(h),2) ~= 0
    Nwf = length(h)-1;
end

R0 = p.Fs/p.OSR;   % Fundamental Table Frequency (FTF), Symbol rate.
dphi = p.Rs/R0;    % NCO phase increment to synthesise symbol rate of Rs.
Nsym = length(s);
Nsamp = floor(Nsym*(p.Fs/p.Rs)/p.Nstr);  % Nsamp is the number of samples per stream.

% Create the coefficient tables for linear interpolation
a0 = zeros(p.Ns, Nwf);
a1 = zeros(p.Ns, Nwf);
for j=1:p.Ns
    a0(j,:) = circshift(h, [0 (j-1)*p.OSR]);
    for i=0:Nwf-1
        a1(j,i+1) = -a0(j,mod(i,Nwf)+1) + a0(j,mod(i+1,Nwf)+1);
    end
end
            
symIdx  = 1;                      % symIdx is the index into the symbol array.
symBuf  = zeros(p.Ns,1);          % The symbol buffer, length Ns, where Ns is the number of symbols in the response.
y       = zeros(p.Nstr,Nsamp);    % Filter output buffer.
phi     = 0;                      % NCO phase accumulator
k       = 0;                      % waveform index, (NCO integer part)
kq      = 0;                      % waveform index for Q-channel if offset (OQPSK)
sc      = 0;                      % storage element for detecting a wrap transition in k.
f       = 0;                      % interpolation slope (NCO fractional part)
i       = 1;                      % output sample index.
din_fp  = 0;                      % file pointer for the din data file
dout_fp = 0;                      % filt pointer for the dout data file

% If genDatFile == true, open a file to log key variables to.
if p.genDatFile
    din_fp = fopen(p.dinFilename, 'w');    
    dout_fp = fopen(p.doutFilename, 'w');
    if (~din_fp || ~dout_fp)
        error('Error opening files for writing');
    end
end

%----------------------- Main processing loop. Process Nsym symbols --------------------------------
while(symIdx <= Nsym)   
        
    % for each sample in the frame
    for l=1:p.Nstr

        k = floor(phi);
        kq = mod(k+p.qOffset,Nwf);
        f = phi - k;
        
        % Generate a symbol timing pulse by detecting a wrap in k, modulo OSR
        dsc = sc;
        sc = mod(k, p.OSR);
        sf = (dsc - sc > 0);
        
        % Do the multiply/sum
        acc = 0;
        for j=1:p.Ns
            %acc = acc + symBuf(j) * (a0(j,k+1) + a1(j,k+1)*f);
            ri = a0(j,k+1)  + a1(j,k+1)*f;
            rq = a0(j,kq+1) + a1(j,kq+1)*f;
            acc = acc + complex(real(symBuf(j))* ri, imag(symBuf(j)) * rq);     
            %fprintf('%d\t%d\t%f\t%f\t%f\t%f\t%f\t%f\n', j,k,f,ri, a0(j,k+1), a1(j,k+1), ...
            %        real(acc), imag(acc));
        end

        % NCO that controls the effective symbol rate. phi wraps modulo the filter length Nwf.
        phi = mod(phi + dphi, Nwf);

        % Write the next symbol into the symbol buffer, every OSR samples, 
        if sf
            sym = p.pst(s(symIdx)+1);
            symBuf(mod(symIdx-1,p.Ns)+1) = sym;
            if(din_fp)
                fprintf(din_fp,'%d\n', s(symIdx));
            end
            symIdx = symIdx + 1;
        end  

        % Write the sample to the output array. Apply gain control.    
        y(l,i) = p.gain .* acc;
    end % for(l=1:Nstr)
    i = i + 1;
end % while

% If an output file is to be written, write the output samples in series (i.e. vector)
if(dout_fp)
    ys = y(:);
    for i=1:length(ys)
        fprintf(dout_fp,'%1.16f,%1.16f\n', real(ys(i)), imag(ys(i)));        
    end    
end
        
fclose('all');