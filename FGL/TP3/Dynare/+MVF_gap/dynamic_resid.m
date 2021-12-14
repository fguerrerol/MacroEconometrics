function residual = dynamic_resid(T, y, x, params, steady_state, it_, T_flag)
% function residual = dynamic_resid(T, y, x, params, steady_state, it_, T_flag)
%
% File created by Dynare Preprocessor from .mod file
%
% Inputs:
%   T             [#temp variables by 1]     double   vector of temporary terms to be filled by function
%   y             [#dynamic variables by 1]  double   vector of endogenous variables in the order stored
%                                                     in M_.lead_lag_incidence; see the Manual
%   x             [nperiods by M_.exo_nbr]   double   matrix of exogenous variables (in declaration order)
%                                                     for all simulation periods
%   steady_state  [M_.endo_nbr by 1]         double   vector of steady state values
%   params        [M_.param_nbr by 1]        double   vector of parameter values in declaration order
%   it_           scalar                     double   time period for exogenous variables for which
%                                                     to evaluate the model
%   T_flag        boolean                    boolean  flag saying whether or not to calculate temporary terms
%
% Output:
%   residual
%

if T_flag
    T = MVF_gap.dynamic_resid_tt(T, y, x, params, steady_state, it_);
end
residual = zeros(12, 1);
lhs = y(8);
rhs = y(9)+y(11)-y(2);
residual(1) = lhs - rhs;
lhs = y(9);
rhs = y(10)+params(4)*x(it_, 1);
residual(2) = lhs - rhs;
lhs = y(10);
rhs = params(1)*params(2)/4+(1-params(1))*y(1)+params(5)*x(it_, 2);
residual(3) = lhs - rhs;
lhs = y(11);
rhs = y(2)*params(3)+params(6)*x(it_, 3);
residual(4) = lhs - rhs;
lhs = y(12);
rhs = params(8)/(1+params(8)*params(7))*y(3)+params(7)/(1+params(8)*params(7))*y(20)+y(11)*params(9)+params(10)*y(14)+params(11)*x(it_, 4);
residual(5) = lhs - rhs;
lhs = y(14);
rhs = params(13)*y(4)+params(14)*x(it_, 5);
residual(6) = lhs - rhs;
lhs = y(16);
rhs = y(17)+y(18);
residual(7) = lhs - rhs;
lhs = y(17);
rhs = y(11)*(-params(15))+params(16)*y(5)+params(22)*x(it_, 6);
residual(8) = lhs - rhs;
lhs = y(18);
rhs = params(18)*params(19)+(1-params(18))*y(6)+y(19)+params(20)*x(it_, 7);
residual(9) = lhs - rhs;
lhs = y(19);
rhs = (1-params(17))*y(7)+params(21)*x(it_, 8);
residual(10) = lhs - rhs;
lhs = y(13);
rhs = y(12)+params(12);
residual(11) = lhs - rhs;
lhs = y(15);
rhs = y(14);
residual(12) = lhs - rhs;

end
