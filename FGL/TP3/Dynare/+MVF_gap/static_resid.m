function residual = static_resid(T, y, x, params, T_flag)
% function residual = static_resid(T, y, x, params, T_flag)
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
%   residual
%

if T_flag
    T = MVF_gap.static_resid_tt(T, y, x, params);
end
residual = zeros(12, 1);
lhs = y(1);
rhs = y(2);
residual(1) = lhs - rhs;
lhs = y(2);
rhs = y(3)+params(4)*x(1);
residual(2) = lhs - rhs;
lhs = y(3);
rhs = params(1)*params(2)/4+y(3)*(1-params(1))+params(5)*x(2);
residual(3) = lhs - rhs;
lhs = y(4);
rhs = y(4)*params(3)+params(6)*x(3);
residual(4) = lhs - rhs;
lhs = y(5);
rhs = y(5)*params(8)/(1+params(8)*params(7))+y(5)*params(7)/(1+params(8)*params(7))+y(4)*params(9)+params(10)*y(7)+params(11)*x(4);
residual(5) = lhs - rhs;
lhs = y(7);
rhs = y(7)*params(13)+params(14)*x(5);
residual(6) = lhs - rhs;
lhs = y(9);
rhs = y(10)+y(11);
residual(7) = lhs - rhs;
lhs = y(10);
rhs = y(4)*(-params(15))+y(10)*params(16)+params(22)*x(6);
residual(8) = lhs - rhs;
lhs = y(11);
rhs = params(18)*params(19)+y(11)*(1-params(18))+y(12)+params(20)*x(7);
residual(9) = lhs - rhs;
lhs = y(12);
rhs = y(12)*(1-params(17))+params(21)*x(8);
residual(10) = lhs - rhs;
lhs = y(6);
rhs = y(5)+params(12);
residual(11) = lhs - rhs;
lhs = y(8);
rhs = y(7);
residual(12) = lhs - rhs;
if ~isreal(residual)
  residual = real(residual)+imag(residual).^2;
end
end
