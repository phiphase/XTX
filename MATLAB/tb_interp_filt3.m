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
% For MATLAB 2010a
%RandStream.setDefaultStream(RandStream('mt19937ar','seed', 12345));

% For MATLAB 2021a
%RandStream('mt19937ar','Seed', 12345);
rng(12345);


% p is the structure that holds the pulse_shaper parameters
%p.ft = 'Square Root Raised Cosine';
p.ft = 'Raised Cosine';
p.Rs = 230;                          % Default output Symbol Rate
p.Fs = 500;                          % Default output sample rate
p.OSR = 8;                           % OSR of the stored waveform.
p.Ns =  8;                           % Number of symbols in the response
p.Nstr = 2;                          % Number of processing streams
p.alpha = 0.35;
p.qOffset = 0;
p.pst = exp(1i*pi/4*(0:7)');         % Input phase references
p.doutFilename = 'dout.golden.dat';  % Golden reference output file
p.dinFilename = 'din.dat';           %  Input samples
p.genDatFile = true;

analyseHWFile = true;                % Enables analysis of H/W output file.
Nsym = 200;                         % Number of symbols
gain = 1.0;
Qgain = 8;                          % Number of bits for gain quantisation
p.gain = round((2^Qgain)*gain)/(2^Qgain);  % Quantise the gain.

% Read the hardware output data for comparison with the MATLAB generated data.
if analyseHWFile
 disp('Analysing H/W output file');
 hw_dout_filename = ['../FPGA/PRJ/pulse_shaper/solution1/csim/build/' ...
                     'hw_dout.dat'];
 hw_dout = dlmread(hw_dout_filename);
 hw_y = complex(hw_dout(:,1), hw_dout(:,2));
end
   
% Symbols: impulse
% symI = [1; zeros(Nsym-1,1)];
% symQ = [1; zeros(Nsym-1,1)];
% sym = complex(symI, symQ);


psQPSK = [1; 7; 3; 5];          % Phase state mapping table for QPSK/OQPSK
%p.qOffset = p.OSR/2;           % For QPSK/8PSK

% Symbols: QPSK/OQPSK
 %s = randi([0 3], Nsym, 1);
 %sym = psQPSK(s+1);

%p.qOffset = p.OSR/2;    % For OQPSK

% Symbols: 8PSK
sym = randi([0 7],Nsym,1);


fprintf('Testbench for interp_filt\n');
fprintf('   Nstr=%d, Rs = %f, Fs = %f, Ns = %d, OSR = %d, alpha = %0.2f\n', ...
    p. Nstr, p.Rs, p.Fs, p.Ns, p.OSR, p.alpha);
fprintf('Filter has %d coefficients\n', p.OSR*p.Ns);

% Run the function under test.
[y h] = interp_filt3(sym, p);

% y is arranged in Nf columns where Nf is the number of frames. Re-arrange to be a single-dimension.
y = y(:);

% Comparison using conventional pulse-shaping and linear interpolation.
% xf is the filtered symbols at the FTF symbol rate, R0 i.e. prior to
% interpolation up to Rs.
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
% plot([real(y) real(yref)]);
% grid on;
% diffIdx = find(abs(yref - y) > errTol);
% hold on;
% plot(real(diffIdx), real(y(diffIdx)), 'or');
% legend('y','yref', 'difference > errTol');
% hold off;

if analyseHWFile
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
ydi(ydi > 1.0) = 1;
ydq(ydi > 1.0) = 1;
ydi(ydi < -1.0) = -1;
ydq(ydi < -1.0) = -1;

% Perform cross-correlation to align received and reference data.
% This is done for I and Q individually so that OQPSK can be handled.
sd = p.gain * p.pst(sym+1);
len = length(ydi);
[~, i] =max(abs(xcorr(ydi,real(sd(1:len)))));
ydi = ydi((i-len+1):end);
[~, i] =max(abs(xcorr(ydq,imag(sd(1:len)))));
ydq = ydq((i-len+1):end);

lmin = min(length(ydi),length(ydq));
yd = complex(ydi(1:lmin),ydq(1:lmin));   % Remove Ns/2 initial zeros
sd = sd(1:lmin);

figure;
plot([yd sd], 'o-');
axis([-1 1 -1 1]);
grid on;
if isreal(sd)    
    mse = mean((yd - sd).^2);
else
    mse = mean((real(sd) - real(yd)).^2 + (imag(sd) - imag(yd)).^2)/mean(abs(sd).^2);
end
evm = mse * 100;

fprintf('Mean Squared Error: %f (%2.2fdB)\n',mse, 10*log10(mse));
fprintf('RMS EVM: %f%%\n',evm);


