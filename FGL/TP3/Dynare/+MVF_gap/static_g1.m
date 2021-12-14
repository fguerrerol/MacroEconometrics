function g1 = static_g1(T, y, x, params, T_flag)
% function g1 = static_g1(T, y, x, params, T_flag)
%
% File created by Dynare Preprocessor from .mod file
%
% Inputs:
%   T         [#temp variables by 1]  double   vector of temporary terms to be filled by function
%   y         [M_.endo_nbr by 1]      double   vector of endogenous variables in declaration order
%   x         [M_.exo_nbr by 1]       double   vector of exogenous variables in declaration order
%   params    [M_.param_nbr by 1]     double   vector of parameter values in declaration order
%                                              to evaluate the model
%   T_flag    boolean                 boolean  flag saying whether or not to calculate temporary terms
%
% Output:
%   g1
%

if T_flag
    T = MVF_gap.static_g1_tt(T, y, x, params);
end
g1 = zeros(12, 12);
g1(1,1)=1;
g1(1,2)=(-1);
g1(2,2)=1;
g1(2,3)=(-1);
g1(3,3)=1-(1-params(1));
g1(4,4)=1-params(3);
g1(5,4)=(-params(9));
g1(5,5)=1-(params(8)/(1+params(8)*params(7))+params(7)/(1+params(8)*params(7)));
g1(5,7)=(-params(10));
g1(6,7)=1-params(13);
g1(7,9)=1;
g1(7,10)=(-1);
g1(7,11)=(-1);
g1(8,4)=params(15);
g1(8,10)=1-params(16);
g1(9,11)=1-(1-params(18));
g1(9,12)=(-1);
g1(10,12)=1-(1-params(17));
g1(11,5)=(-1);
g1(11,6)=1;
g1(12,7)=(-1);
g1(12,8)=1;
if ~isreal(g1)
    g1 = real(g1)+2*imag(g1);
end
end
