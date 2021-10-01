%---------------------------------------------------------------------------------------------------
% Copyright (C) 2018 Phiphase Limited
%
% Testbench for CCSDS Channel coder
% ---------------------------------
%
% Dependencies: uses the MATLAB 2010 Communications toolbox
%
% References: CCSDS 401.0-B-4 Annex 1 (4D-TCM-8PSK)
%             CCSDS 131.0-B-3 Section 3 (Convolutional Coding)
%---------------------------------------------------------------------------------------------------
clear;
clc;
RandStream.setDefaultStream(RandStream('mt19937ar','seed', 12345));

% Encoding
p.mode   = 1;
p.diff   = true;      % Differential encoding of the input sequence
p.bicm   = 1;         % 8PSK BICM case 1 or case 2

% Define the convolutional encoder
G1 = 171;
G2 = 133;
K = 7;
trel = poly2trellis(K, [G1 G2]);

% Puncture patterns These are ordered in time assuming that the C1 and C2 output bits (from poly G1
% and G2 respectively) are transmitted in time alternately  i.e. in the order C1(1), C2(1), C1(2),
% C2(2),...A '0' denotes puncture.
R_1_2 = [1 1];                          % Rate 1/2: c1(1)c2(1)...
R_2_3 = [1 1 0 1];                      % Rate 2/3: c1(1)c2(1)c2(2)...
R_3_4 = [1 1 0 1 1 0];                  % Rate 3/4: c1(1)c2(1)c2(2)c1(3)...
R_5_6 = [1 1 0 1 1 0 0 1 1 0];          % Rate 5/6: c1(1)c2(1)c2(2)c1(3)c2(4)c1(5)...
R_7_8 = [1 1 0 1 0 1 0 1 1 0 0 1 1 0];  % Rate 7/8: c1(1)c2(1)c2(2)c2(3)c2(4)c1(5)c2(6)c1(7)...

% QPSK mapping table for gray coding phase states
map_QPSK = [1 7 3 5];
% BICM 8PSK (from CCSDS 401.0 Annex 2) phase states
map_8PSK_BICM_case1 = [1 2 4 3 0 7 5 6];
map_8PSK_BICM_case2 = [1 0 4 5 2 7 3 6];

% Input data
Nb = 100;
din = randint(Nb,1);


din_diff = zeros(size(din));
if(p.diff)
    m = din(1);
    for i=1:length(din)
        if din(i) == 0
            din_diff(i) = m;
        else
            m = ~m;
            din_diff(i) = m;
        end
    end
else
    din_diff = din;
end

switch p.mode
    case 1  % Rate 1/2 CC
        dout = convenc(din_diff, trel);
        dout = xor(dout,repmat([0 1]', length(dout)/2,1));  % Invert C2 bits
        npad = mod(length(dout),2);
        dout = [dout; zeros(npad,1)];
        z = reshape(dout,[],2) * [1;2];
        z = map_QPSK(z+1);
    case 2 % Rate 2/3 CC
        dout = convenc(din_diff, trel, R_2_3);
        npad = mod(length(dout),2);
        dout = [dout; zeros(npad,1)];
        z = reshape(dout,[],2) * [1;2];
        z = map_QPSK(z+1);
    case 3 % Rate 3/4 CC
        dout = convenc(din_diff, trel, R_3_4);
        npad = mod(length(dout),2);
        zdout = [dout; zeros(npad,1)];
        z = reshape(dout,[],2) * [1;2];
        z = map_QPSK(z+1);
    case 4 % Rate 5/6 CC
        dout = convenc(din_diff, trel, R_5_6);
        npad = mod(length(dout),2);
        dout = [dout; zeros(npad,1)];
        z = reshape(dout,[],2) * [1;2];
        z = map_QPSK(z+1);
    case 5 % Rate 7/8 CC
        dout = convenc(din_diff, trel, R_7_8);
        npad = mod(length(dout),2);
        dout = [dout; zeros(npad,1)];
        z = reshape(dout,[],2) * [1;2];
        z = map_QPSK(z+1);
    case 6  % 8-PSK, no coding
        npad = mod(length(dout),3);
        din_diff=[din_diff; zeros(npad,1)];
        z = reshape(dout,[],2) * [1;2;4];
        % Perform BICM mapping
        if p.bicm == 1
            z = map_8PSK_BICM_case1(z+1);
        else
            z = map_8PSK_BICM_case2(z+1);
        end
    case 7  % 4D-TCM, m=8,  Rm=8/9,   Reff = 2 bits per symbol        
        z = tcm_encode(din_diff,8);
    case 8  % 4D-TCM, m=9,  Rm=9/10,  Reff = 2.25 bits per symbol
        z = tcm_encode(din_diff,9);
    case 9  % 4D-TCM, m=10, Rm=10/11, Reff = 2.5 bits per symbol 
        z = tcm_encode(din_diff,10);
    case 10 % 4D-TCM, m=11, Rm=11/12, Reff = 2.75 bits per symbol
        z = tcm_encode(din_diff,11);
    otherwise % No coding
        dout = din_diff;
        npad = ceil(mod(dout,2));
        dout = [dout; zeros(npad,1)];
        z = reshape(dout,[],2) * [1;2];
        z = map_QPSK(z+1);
end

% Make z a column vector.
z = z(:);


% map phase states to complex numbers
zc = exp(1i*z*pi/4);
plot(zc,'-o');