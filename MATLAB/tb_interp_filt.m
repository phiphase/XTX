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
FT = 'Raised Cosine';
Rs = 200;
Fs = 500;     % output sample rate
OSR = 8;      % Over-sampling ratio of the stored waveform.
Ns = 8;       % Number of symbols in the response
Nwf = Ns*OSR;
intMode = 1;  % Interpolation mode
genDatFile = true;
analyseFile = false;
gain = 0.5;
Ngain = 8;  % Number of bits for gain quantisation
gain = round((2^Ngain)*gain)/(2^Ngain);  % Quantise the gain.

% Read the hardware output data for comparison with the MATLAB generated data.
 hw_dout_filename = 'Z:/XTX/FPGA/Modulator/modulator/solution2/csim/build/hw_dout.dat';
 hw_dout = dlmread(hw_dout_filename,'\t');
 hw_y = complex(hw_dout(:,1), hw_dout(:,2));
   

% Symbols: QPSK
%  Nsym = 1024;
%  symI = 2*randi([0 1],Nsym,1)-1;
%  symQ = 2*randi([0 1],Nsym,1)-1;
%  sym = complex(symI, symQ);

% Offset QPSK
% Nsym = 512;
% symI = 2*randi([0 1],Nsym,1)-1;
% symQ = 2*randi([0 1],Nsym,1)-1;
% symI = reshape(repmat(symI',2,1),[],1);
% symQ = reshape(repmat(symQ',2,1),[],1);
% symQ = [0; symQ(1:end-1)];
% sym = complex(symI, symQ);


% Symbols: 8PSK
Nsym = 100;
s = randi([0 7],Nsym,1);
sym = 0.99999 * exp(1i*s*pi/4);

alpha = 0.50;
d = fdesign.pulseshaping(OSR, FT, 'Nsym,Beta', Ns, alpha);
hf = design(d);
% Since we are not doing FIR filtering and summing, we can make the
% peak value = 1 by multiplying coefficients by OSR.
h = OSR*hf.Numerator(1:Nwf);

% Create the raised cosine pulse in time domain - i.e. "manually".
% Proakis, "Digital Communications" equation 9.2-27
%Ts = OSR;
t=(-Nwf/2:Nwf/2-1)/OSR;  % t is normalised wrt OSR (Ts)
if(strcmp(FT,'Raised Cosine'))
    ht = sin(pi*t)./(pi*t);
    ht(isnan(ht)) = 1.0;
    ht = ht .* (cos(pi*alpha*t)./(1-(2*alpha.*t).^2));
    ht(isinf(ht))=0;
% Square Root Raised Cosine
else    
    ht = (sin(pi*t*(1-alpha)) + 4*alpha*t .* cos(pi*t*(1+alpha))) ./ ...
                          (pi*t .* (1-(4*alpha*t).^2));
     ht(t == 0) = 1 + alpha*(4/pi - 1);
     ht(abs(t) == 1/(4*alpha)) = (alpha/sqrt(2)) * ( (1+2/pi) * sin(pi/(4*alpha)) + ...
                                           (1-2/pi)*cos(pi/(4*alpha)));          
end

% Select ht if we want to use the manually generated coefficients (matches 'C' model).
h = ht;

fprintf('Testbench for interp_filt\n');
fprintf('   Rs = %f, Fs = %f, Ns = %d, OSR = %d, alpha = %0.2f, interp. order=%d\n', ...
    Rs, Fs, Ns, OSR, alpha, intMode);
fprintf('Filter has Nwf = %d coefficients\n', length(h));

% Run the function under test.
y = interp_filt(sym, Rs, Fs, h, Ns, OSR, intMode, gain, genDatFile);

% Comparison using conventional pulse-shaping and linear interpolation.
% xf is the filtered symbols at the FTF symbol rate, R0 i.e. prior to interpolation up to Rs.
errTol = 1e-4;
t0 = (0:Nsym*OSR-1)';
ts = t0*(Rs*OSR/Fs);
x = upsample(sym, OSR, 0);
xf = filter(h,1,x);

% Linear interpolation
yref = interp1(t0,xf,ts, 'linear');
yref = yref(~isnan(yref));
yref = yref(abs(yref) < OSR);

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
    y = hw_y;
end

% Re-sample to extract the symbols from the raised cosine pulses.
OSR_rx = 10;
FsRx = OSR_rx * Fs;
yrx = interp(y, OSR_rx);
Ny = length(yrx);
t0 = (0:Ny-1)'/FsRx;
ts = (0:Nsym-1)'/Rs;
yd = interp1(t0,yrx,ts, 'linear');

% Remove leading zeros etc.
yd = yd(abs(yd) > 0.01);
sd = sym(1:length(yd));

% Apply quantised gain to the reference symbols.
sd = gain * sd;

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
%fprintf('Standard deviation: %f (%2.2fdB)\n',std(yd-sd), 20*log10(std(yd-sd)));

%figure;
%hs = spectrum.welch('Hann', 256);
%psd(hs, y, 'Fs', Fs, 'CenterDC',true);

%fvtool(hf,'Fs',Fs);

