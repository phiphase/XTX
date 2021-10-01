%---------------------------------------------------------------------------------------------------
% Copyright (C) 2018 Phiphase Limited
%
% modulo-8 bit-wise add
% This function is used by tcm_encode().
%
% Input arguments:
%    a:  1 x 3 vector. a(1) is bit 0 of a, a(2) is bit 1 of a, a(3) is bit 2 of a
%    b:  1 x 3 vector. b(1) is bit 0 of b, b(2) is bit 1 of b, b(3) is bit 2 of b
%
% Output arguments:
%    c(1) = a(1) ^ b(1)
%    c(2) = a(2) ^ b(2) ^ r(1)
%    c(3) = a(3) ^ b(3) ^ r(2)
%
%    c(1) is bit 0 of c, c(2) is bit 1 of c, c(3) is bit 2 of c.
%---------------------------------------------------------------------------------------------------
function c = add_mod_8(a,b)
% r1 is the carry from a1 ^ b1
% r2 is the carry from a2 ^ b2 ^ r1
r(1) = and(a(1),b(1));
r(2) = or(and(a(2),b(2)), and(a(2),r(1))); r(2) = or(r(2), and(b(2), r(1)));
c(1) = xor(a(1), b(1));
c(2) = xor(a(2),b(2)); c(2) = xor(c(2), r(1));
c(3) = xor(a(3),b(3)); c(3) = xor(c(3), r(2));
