%---------------------------------------------------------------------------------------------------
% Copyright (C) 2018 Phiphase Limited
%
% Interpolating polyphase pulse shaping filter
% File: interp_filt.m
%
% Input arguments:
%   s       : array of input symbols which may be complex.
%   Rs      : The symbol rate (Hz)
%   Fs      : The output sample rate (Hz)
%   h       : An array of OSR*Ns filter coefficients
%   Ns      : Number of symbols represented by the filter impulse response h
%   OSR     : over-sampling ratio of the filter (i.e. number of samples per symbol)
%   intMode : 0 = no interp, 1 = linear, 2 = quadratic, 3 = cubic
%   [genDatFile] : 1 = generate a test bench data file, interp_filt.dat to enable verification.
%
% Output arguments:
%   y   : Thearray of filtered output samples at sample rate Fs.
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
function y = interp_filt(s, Rs, Fs, h, Ns, OSR, intMode, gain_, genDatFile_)

if nargin < 7 || nargin > 9
    error('interp_filt: usage: y = interp_filt(s, Rs, Fs, h, Ns, OSR, intMode, [genDatFile])');
end

if rem(Ns,1) || Ns <= 0
    error('Ns must be an integer > 0');
end
if rem(OSR,1) || OSR <= 0
    error('OSR must be an integer > 0');
end
if Rs > Fs
    error('Rs must be less than Fs');
end
if intMode > 3 || intMode < 0
    error('intMode must be in the range 0-3');
end

gain = 1.0;
if nargin ==9
    gain = gain_;
end

genDatFile = false;
if nargin == 9
    genDatFile = genDatFile_;
end

Nwf = length(h);

% Make the filter length even.
if mod(length(h),2) ~= 0
    Nwf = length(h)-1;
end
if Nwf ~= Ns*OSR
    error('Filter length must be Ns*OSR');
end

R0 = Fs/OSR;   % Fundamental Table Frequency (FTF), Symbol rate.
dphi = Rs/R0;  % NCO phase increment to synthesise symbol rate of Rs.
Nsym = length(s);

% If symbols are complex, make the coefficients complex with equal real and imaginary parts.
if ~isreal(s)
    if isreal(h)
        h = complex(h,0);
    end
end


% Create the coefficient tables, depending upon the interpolation mode that has been selected.
switch(intMode)
    % Create a set of Ns tables, offset by jxOSR - linear interpolation
    case 1
        a0 = zeros(Ns, Nwf);
        a1 = zeros(Ns, Nwf);
        for j=1:Ns
             a0(j,:) = circshift(h, [0 (j-1)*OSR]);
             %a1(j,:) = [diff(a0(j,:)) a0(j,1)-a0(j,end)];
            for i=0:Nwf-1
                a1(j,i+1) = -a0(j,mod(i,Nwf)+1) + a0(j,mod(i+1,Nwf)+1);
            end
        end
        
    % Create a set of tables for quadratic interpolation
    case 2    
        a0 = zeros(Ns, Nwf);
        a1 = zeros(Ns, Nwf);
        a2 = zeros(Ns, Nwf);
        for j=1:Ns
            a0(j,:) = circshift(h, [0 (j-1)*OSR]);    % s(n-2)        
            for i=0:Nwf-1
                %                         s(n-2)             s(n-1)                      s(n)
                a1(j,i+1) = -1.5*a0(j,mod(i,Nwf)+1) + 2.0*a0(j,mod(i+1,Nwf)+1) - 0.5*a0(j,mod(i+2,Nwf)+1);
                a2(j,i+1) =  0.5*a0(j,mod(i,Nwf)+1) - 1.0*a0(j,mod(i+1,Nwf)+1) + 0.5*a0(j,mod(i+2,Nwf)+1);    
            end
        end

    % Create a set of tables for cubic interpolation.
    case 3
        a0 = zeros(Ns, Nwf);
        a1 = zeros(Ns, Nwf);
        a2 = zeros(Ns, Nwf);
        a3 = zeros(Ns, Nwf);
        for j=1:Ns
            a0(j,:) = circshift(h, [0 (j-1)*OSR]);    % s(n-1)        
            for i=0:Nwf-1
                n         = mod(i+1,Nwf)+1;
                n_minus_1 = mod(i,Nwf)+1;
                n_minus_2 = mod(i-1,Nwf)+1;
                n_minus_3 = mod(i-2,Nwf)+1;

                a1(j,i+1) = 1/6*a0(j,n_minus_3) - a0(j,n_minus_2) + 1/2*a0(j,n_minus_1) + 1/3 * a0(j,n);
                a2(j,i+1) = 1/2*a0(j,n_minus_2) - a0(j,n_minus_1) + 1/2*a0(j,n);
                a3(j,i+1) = -1/6*a0(j,n_minus_3) + 1/2*a0(j,n_minus_2) - 1/2*a0(j,n_minus_1) + 1/6*a0(j,n);
            end
        end
        
    otherwise
        a0 = zeros(Ns, Nwf);        
        for j=1:Ns
             a0(j,:) = circshift(h, [0 (j-1)*OSR]);             
        end
end

    
symIdx  = 1;                 % symIdx is the index into the symbol array.
symBuf  = zeros(Ns,1);       % The symbol buffer, length Ns, where Ns is the number of symbols in the response.
phi     = 0;                 % NCO phase accumulator
k       = 0;                 % waveform index, (NCO integer part)
sc      = 0;                 % storage element for detecting a wrap transition in k.
f       = 0;                 % interpolation slope (NCO fractional part)
sf      = 1;                 % initialise the symbol flag to 1 to ensure the first symbol is loaded.
y       = zeros(Nsym*OSR,1); % Filter output buffer.
i       = 1;                 % output sample index.
acc     = 0;                 % output sample accumulator.
din_fp  = 0;                 % file pointer for the din data file
dbg_fp  = 0;                 % file pointer for the debug data file
dout_fp = 0;                % filt pointer for the dout data file

% If genDatFile == true, open a file to log key variables to.
if genDatFile
    din_fp = fopen('din.dat', 'w');    
    dbg_fp = fopen('dbg.dat', 'w');
    dout_fp = fopen('dout.golden.dat', 'w');
end

while(symIdx <= Nsym)   
    
    % Do the multiply/sum
    acc = 0;    
    for j=1:Ns    
        % Apply the interpolation.
        switch intMode
            case 1                
                acc = acc + symBuf(j) * (a0(j,k+1) + a1(j,k+1)*f);
            case 2
                acc = acc + symBuf(j) * (a0(j,k+1) + a1(j,k+1)*f + a2(j,k+1)*f.^2);
            case 3
                acc = acc + symBuf(j) * (a0(j,k+1) + a1(j,k+1)*f + a2(j,k+1)*f.^2 + a3(j,k+1)*f.^3);
            otherwise
                acc = acc + symBuf(j) * a0(j,k+1);
        end
    end
        
    % NCO that controls the effective symbol rate. phi wraps modulo the filter length Nwf.
    phi = mod(phi + dphi, Nwf);
    k = floor(phi);
    f = phi - k;    
        
    % Generate a symbol timing pulse by detecting a wrap in k, modulo OSR
    dsc = sc;
    sc = mod(k, OSR);
    sf = (dsc - sc > 0);
    
    % Write the sample to the output array.
    % Apply gain control.
    y(i) = gain * acc;  
    
    if(dout_fp)
        fprintf(dout_fp,'(%f,%f)\n', real(y(i)), imag(y(i)));
    end
    if(dbg_fp)
       % i, re(symbol), im(symbol), acc, phi, k, f, sf, re(y), im(y)
       fprintf(dbg_fp,'%d\t%f\t%f\t%f\t%f\t%d\t%f\t%d\t%f\t%f\n', ...
           i, real(s(symIdx)), imag(s(symIdx)), acc, phi, k, f, sf, real(y(i)), imag(y(i)));
    end
    
    % Write the next symbol into the symbol buffer, every OSR samples, 
    if sf
        symBuf(mod(symIdx-1,Ns)+1) = s(symIdx);
        if(din_fp)
            fprintf(din_fp,'(%f,%f)\n', real(s(symIdx)), imag(s(symIdx)));
        end
        symIdx = symIdx + 1;
    end  

    i = i + 1;
end

fclose('all');