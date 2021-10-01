%---------------------------------------------------------------------------------------------------
% Copyright (C) 2018 Phiphase Limited
%
% Ungerboek Trellis coded modulation (TCM) encoder
% Reference: CCSDS 401.0-B-4 Annex 1
%
% This function takes an N x 1 vector of binary input symbols (value 0 or 1), applies differential
% coding, R=3/4 convolutional coding and constellation mapping
%
% Input arguments
%     d : N x 1 vector of binary input symbols, value 0 or 1
%     m : Coding rate, 8, 9, 10 or 11 for Rates of Rm=m/m+1.
%
% Output arguments
%     z : an array of 4 symbols representing signal set constituents, L=4, each with value 0-7
%---------------------------------------------------------------------------------------------------
function z = tcm_encode(d,m)

% Set the indexes of the differential encoder (dcIdx) and the constellation mapper according to m.
% We need to add +1 because MATLAB arrays are 1'based index.
nought = m+1;  % Index in x which maps to a '0'
switch m
    case 8
        dcIdx = [1 5 8] + 1;
        z1map = [1 5 8] + 1;
        z2map = [nought 3 7; 1 5 8] + 1;
        z3map = [nought 2 5; 1 5 8] + 1;
        z4map = [nought 1 4; nought 2 6] + 1;        
    case 9
        dcIdx = [2 6 9] + 1;
        z1map = [2 6 9] + 1;
        z2map = [0 4 8; 2 6 9] + 1;
        z3map = [nought 3 7; 2 6 9] + 1;
        z4map = [nought 3 7; nought 1 5] + 1;
    case 10
        dcIdx = [3 7 10] + 1;
        z1map = [3 7 10] + 1;
        z2map = [1 5 9; 3 7 10] + 1;
        z3map = [0 4 8; 3 7 10] + 1;
        z4map = [nought 2 6; 0 4 8] + 1;
    case 11
        dcIdx = [4 8 11] + 1;
        z1map = [4 8 11] + 1;
        z2map = [2 6 10; 4 8 11] + 1;
        z3map = [1 5 9; 4 8 11] + 1;
        z4map = [0 3 7; 1 5 9] + 1;
    otherwise
        error('tcm_encode: m must be 8, 9, 10 or 11');
end

% Pad the input vector to be a multiple of m and reshape into n x m bit matrix, wi
npad = mod(length(d), m);
d = [din; zeros(npad,1)];
wi = reshape(d, [], m);
N = size(wi,1);   % Number of words in wi
c = zeros(1,3);   % Differential encoder delay element
x = zeros(1,m+2); % differentially encoded vector. Note: extra zero on the end for when the 
                  % constellation mapper needs to sum a zero.
d = zeros(1,6);   % Delay line for the convolutional encoder
for i=1:N
    % Differential encoding, modulo-8
    a = w(i,dcIdx);
    b = c(dcIdx);
    c = add_mod_8(a,b);
    x(dcIdx) = c;
    
    % Convolutional encoding, generates x0
    d(1) = x(1);
    d(2) = xor(d(1), x(4));
    d(3) = xor(d(2), x(3));
    d(4) = xor(d(3), x(4));
    d(5) = xor(d(4), x(2)); d(5) = xor(d(5), x(3));
    d(6) = xor(d(5), x(2)); d(6) = xor(d(6),x(1));
    x(1) = d(6);
    
    % Constellation mapping
    zbits(1,:) = x(z1map);
    zbits(2,:) = add_mod_8(z2map(1,:), z2map(2,:));
    zbits(3,:) = add_mod_8(z3map(1,:), z3map(2,:));
    zbits(4,:) = add_mod_8(add_mod_8(z4map(1,:), z4map(2,:)), zbits(2,:));
    % Convert the bit patterns to unsigned values 0-7
    z = zbits * [1;2;4];
end
