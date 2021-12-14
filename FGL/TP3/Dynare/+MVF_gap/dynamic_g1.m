function g1 = dynamic_g1(T, y, x, params, steady_state, it_, T_flag)
% function g1 = dynamic_g1(T, y, x, params, steady_state, it_, T_flag)
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
%   g1
%

if T_flag
    T = MVF_gap.dynamic_g1_tt(T, y, x, params, steady_state, it_);
end
g1 = zeros(12, 28);
g1(1,8)=1;
g1(1,9)=(-1);
g1(1,2)=1;
g1(1,11)=(-1);
g1(2,9)=1;
g1(2,10)=(-1);
g1(2,21)=(-params(4));
g1(3,1)=(-(1-params(1)));
g1(3,10)=1;
g1(3,22)=(-params(5));
g1(4,2)=(-params(3));
g1(4,11)=1;
g1(4,23)=(-params(6));
g1(5,11)=(-params(9));
g1(5,3)=(-(params(8)/(1+params(8)*params(7))));
g1(5,12)=1;
g1(5,20)=(-(params(7)/(1+params(8)*params(7))));
g1(5,14)=(-params(10));
g1(5,24)=(-params(11));
g1(6,4)=(-params(13));
g1(6,14)=1;
g1(6,25)=(-params(14));
g1(7,16)=1;
g1(7,17)=(-1);
g1(7,18)=(-1);
g1(8,11)=params(15);
g1(8,5)=(-params(16));
g1(8,17)=1;
g1(8,26)=(-params(22));
g1(9,6)=(-(1-params(18)));
g1(9,18)=1;
g1(9,19)=(-1);
g1(9,27)=(-params(20));
g1(10,7)=(-(1-params(17)));
g1(10,19)=1;
g1(10,28)=(-params(21));
g1(11,12)=(-1);
g1(11,13)=1;
g1(12,14)=(-1);
g1(12,15)=1;

end
