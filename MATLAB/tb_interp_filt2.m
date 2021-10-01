%---------------------------------------------------------------------------------------------------
% Copyright (C) 2018 Phiphase Limited
%
% Testbench for function: interp_filt()
% File: tb_interp_filt.m
%
% Description:
%---------------------------------------------------------------------------------------------------
clc;
close all;
clear;
RandStream.setDefaultStream(RandStream('mt19937ar','seed', 12345));

Nsym = 100;

%p.ft = 'Square Root Raised Cosine';
p.ft = 'Raised Cosine';
p.Rs = 200;
p.Fs = 500;     % output sample rate
p.OSR = 8;      % Over-sampling ratio of the stored waveform.
p.Ns =  8       % Number of symbols in the response
p.alpha = 0.5;
p.qOffset = 0;
p.doutFilename = 'dout.golden.dat';
p.dinFilename = 'din.dat';
p.genDatFile = true;
analyseFile = true;
gain = 0.5;
Qgain = 8;  % Number of bits for gain quantisation
p.gain = round((2^Qgain)*gain)/(2^Qgain);  % Quantise the gain.

% Read the hardware output data for comparison with the MATLAB generated data.
if analyseFile
 hw_dout_filename = 'Z:/XTX/FPGA/Modulator/modulator/solution2/csim/build/hw_dout.dat';
 hw_dout = dlmread(hw_dout_filename,'\t');
 hw_y = complex(hw_dout(:,1), hw_dout(:,2));
end
   
% Symbols: impulse
% symI = [1; zeros(Nsym-1,1)];
% symQ = [1; zeros(Nsym-1,1)];
% sym = complex(symI, symQ);

% Symbols: QPSK
% symI = 2*randi([0 1],Nsym,1)-1;
% symQ = 2*randi([0 1],Nsym,1)-1;
% sym = 0.99999* complex(symI, symQ);
% p.qOffset = 0;

% Offset QPSK
symI = 2*randi([0 1],Nsym,1)-1;
symQ = 2*randi([0 1],Nsym,1)-1;
sym = 0.99999* complex(symI, symQ);
p.qOffset = p.OSR/2;

% Symbols: 8PSK
% s = randi([0 7],Nsym,1);
% sym = 0.99999 * exp(1i*s*pi/4);
% p.qOffset = 0;


fprintf('Testbench for interp_filt\n');
fprintf('   Rs = %f, Fs = %f, Ns = %d, OSR = %d, alpha = %0.2f\n', ...
    p.Rs, p.Fs, p.Ns, p.OSR, p.alpha);
fprintf('Filter has %d coefficients\n', p.OSR*p.Ns);

% Run the function under test.
[y h] = interp_filt2(sym, p);


% Comparison using conventional pulse-shaping and linear interpolation.
% xf is the filtered symbols at the FTF symbol rate, R0 i.e. prior to interpolation up to Rs.
errTol = 1e-4;
t0 = (0:Nsym*p.OSR-1)';
ts = t0*(p.Rs*p.OSR/p.Fs);
x = upsample(sym, p.OSR, 0);
xf = filter(h,1,x);

% Linear interpolation
yref = interp1(t0,xf,ts, 'linear');
yref = yref(~isnan(yref));
yref = yref(abs(yref) < p.OSR) * gain;

% Ensure vectors y and yref are the same length.
if length(y) < length(yref)
    yref=yref(1:length(y));
else
    y=y(1:length(yref));
end

% Plot the filter and reference outputs and indicate any deviations with a red circle.
plot([real(y) real(yref)]);
grid on;
diffIdx = find(abs(yref - y) > errTol);
hold on;
plot(real(diffIdx), real(y(diffIdx)), 'or');
legend('y','yref', 'difference > errTol');
hold off;

if analyseFile
    y = hw_y(1:length(y));   
end

% Re-sample to extract the symbols from the raised cosine pulses.
OSR_rx = 10;
FsRx = OSR_rx * p.Fs;

% Up-sample by a factor of OSR_rx to gain more timing resolution.
yrx = interp(y, OSR_rx);

% Sample at the symbol rate, applying a 1/2 symbol offset for OQPSK.
Ny = length(yrx);
t0 = (0:Ny-1)'/FsRx;
tsi = (0:(Nsym*p.OSR)-1)'/p.Rs;
tsq = ((0:(Nsym*p.OSR)-1)' + (p.qOffset/p.OSR))/p.Rs;
ydi = interp1(t0,real(yrx),tsi, 'linear');
ydq = interp1(t0,imag(yrx),tsq, 'linear');

% Remove leading Nan
ydi = ydi(~isnan(ydi));
ydq = ydq(~isnan(ydq));

% Perform cross-correlation to align received and reference data. This is done for I and Q
% individually so that OQPSK can be handled.
sd = p.gain * sym;
len = length(ydi);
[s, i] =max(abs(xcorr(ydi,real(sd))));
ydi = ydi((i-len+1):end);
[s, i] =max(abs(xcorr(ydq,imag(sd))));
ydq = ydq((i-len+1):end);

lmin = min(length(ydi),length(ydq));
yd = complex(ydi(1:lmin),ydq(1:lmin));   % Remove Ns/2 initial zeros
sd = sd(1:lmin);

figure;
plot([yd sd], 'o-');
grid on;
if isreal(sd)    
    mse = mean((yd - sd).^2);
else
    mse = mean((real(sd) - real(yd)).^2 + (imag(sd) - imag(yd)).^2)/mean(abs(sd).^2);
end
evm = mse * 100;

fprintf('Mean Squared Error: %f (%2.2fdB)\n',mse, 10*log10(mse));
fprintf('RMS EVM: %f%%\n',evm);


