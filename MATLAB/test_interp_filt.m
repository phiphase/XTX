%---------------------------------------------------------------------------------------------------
% Copyright (C) 2018 Phiphase Limited
%
% Interpolating polyphase pulse shaping filter
% File: test_interp_filt.m
%
% Description:
%   This MATLAB script implements a pulse-shaping filter that converts a series of symbols (+1/-1)
%   to filtered, over-sampled pulses with arbitrary symbol rate.
%   Instead of using a conventional FIR filter, the filter operates by storing the impulse response
%   of the filter in a table.  The impulse response is an over-sampled pulse shape e.g. raised
%   cosine or other characteristic.  It is Ns symbols long, and has an over-sampling ratio of OSR.
%   The fundamental table frequency (FTF) represents a symbol rate R0 of Fs/OSR where Fs is the
%   desired output sample rate.
%
%   An Ns symbol circular buffer stores the previous Ns symbols. Upon each output sample iteration,
%   symbol s(n-j) is multiplied with h(k-jOSR) where j=0:Ns-1, k is the table index (derived from
%   the integer part of the NCO accumulator), and n is the input symbol stream index.  The Ns
%   products are then accumulated to form the output sample.
%
%   An NCO is used to step a set of Ns pointers, each OSR samples apart. The NCO phase increment
%   determines the transmitted symbol rate. The NCO generates a carry-out every Nwf samples. When a
%   carry-out is generated, the symbol circular buffer is updated with the next symbol.
%       Ns  : Number of symbols represented by the impulse response
%       Nwf : Number of samples in the waveform
%       Fs  : Output sample rate (Hz)
%       OSR : Over-sampling ratio
%       R0  : = Fs/OSR, the fundamental symbol rate.
%
%---------------------------------------------------------------------------------------------------
clc;
close all;
clear;
RandStream.setDefaultStream(RandStream('mt19937ar','seed', 12345));

% Symbols
Nsym = 1024;
sym = 2*randi([0 1],Nsym,1)-1;

Fs = 500;      % output sample rate
OSR = 8;      % Over-sampling ratio of the stored waveform.
R0 = Fs/OSR;   % Fundamental Table Frequency (FTF), Symbol rate.

Rs = 200;      % Required symbol rate.
dphi = Rs/R0;  % NCO phase increment to synthesise symbol rate of Rs.

% Design the filter response.
Ns = 8;        % Number of symbols in the response (determines sharpness of filter transition bands)
alpha = 0.35;
d = fdesign.pulseshaping(OSR, 'Raised Cosine', 'Nsym,Beta', Ns, alpha);
hf = design(d);
Nwf = length(hf.Numerator)-1;
h = hf.Numerator(1:Nwf);
h(abs(h) < eps) = 0;

% Create a set of Ns tables, offset by jxOSR
H = zeros(Ns, Nwf);
HD = zeros(Ns, Nwf);
for j=1:Ns
    H(j,:) = circshift(h, [0 (j-1)*OSR]);
    HD(j,:) = [diff(H(j,:)) H(j,1)-H(j,end)];
end

symIdx = 1;             % symIdx is the index into the symbol array.
symBuf = zeros(1,Ns);   % The symbol buffer, length Ns, where Ns is the number of symbols in the response.
phi = 0;                % NCO phase accumulator
k = 0;                  % waveform index, (NCO integer part)
sc = 0;                 % storage element for detecting a wrap transition in k.
f = 0;                  % interpolation slope (NCO fractional part)
sf = 1;                 % initialise the symbol flag to 1 to ensure the first symbol is loaded.
y = zeros(Nsym*OSR,1);  % Filter output buffer.
i = 1;                  % output sample index.


while(symIdx <= Nsym)
       
    % Write the next symbol into the symbol buffer, every OSR samples, 
    if sf
        symBuf(mod(symIdx-1,Ns)+1) = sym(symIdx);
        symIdx = symIdx + 1;
    end  
       
    % Do the multiply/sum
    acc = 0;    
    for j=1:Ns    
        % Uses a table for the difference
        acc = acc + symBuf(j) * (H(j,k+1) + HD(j,k+1)*f);        
    end

    fprintf('%d:\t%2.4f\t%d\t%1.4f\t%d\t%d : %1.6f\t%1.6f\n',i,phi, k, f, sc, sf, H(1,k+1), HD(1,k+1));
    
    y(i) = acc;
    i = i + 1;
        
    % NCO that controls the effective symbol rate. phi wraps modulo the filter length Nwf.
    phi = mod(phi + dphi, Nwf);
    k = floor(phi);
    f = phi - k;
        
    % Generate a symbol timing pulse by detecting a wrap in k, modulo OSR
    dsc = sc;
    sc = mod(k, OSR);
    sf = (dsc - sc > 0);
end

Nop = i-1;   % Number of valid output samples
y = y(1:Nop);

% Comparison using conventional pulse-shaping and linear interpolation.
errTol = 1e-4;
t0 = (0:Nsym*OSR-1)';
ts = t0*(Rs/R0);
x = upsample(sym, OSR, 0);
yref = interp1q(t0,filter(h,1,x),ts);
if length(y) < length(yref)
    yref=yref(1:length(y));
else
    y=y(1:length(yref));
end
plot([y yref], '-o')
grid on;
diffIdx = find(abs(yref - y) > errTol);
hold
plot(diffIdx, y(diffIdx), 'or');
legend('y','yref', 'difference > errTol');
hold off;
figure;

% Decimate to recover original symbols and measure error
Ny = length(y);
t0 = (0:Ny-1)'/Fs;
ts = t0*(Fs/Rs);
yd = interp1q(t0,y,ts);
yd = yd(~isnan(yd));
yd = yd(abs(yd) > 0.5);
%plot(ts(1:length(yd)),yd);
sd = sym(1:length(yd));
plot([yd sd]);
mse = mean((yd - sd).^2);
fprintf('Mean Squared Error: %f (%2.2fdB)\n',mse, 10*log10(mse));
fprintf('Standard deviation: %f (%2.2fdB)\n',std(yd-sd), 20*log10(std(yd-sd)));

figure;
hs = spectrum.welch('Hann', 256);
%msspectrum(hs, y, 'Fs', Fs, 'CenterDC',true);
psd(hs, y, 'Fs', Fs, 'CenterDC',true);

