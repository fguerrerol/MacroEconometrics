function [ys_, params, info] = steadystate(ys_, exo_, params)
% Steady state generated by Dynare preprocessor
    info = 0;
    ys_(3)=params(2)/4;
    ys_(1)=ys_(3);
    ys_(2)=ys_(3);
    ys_(4)=0;
    ys_(5)=0;
    ys_(6)=params(12);
    ys_(7)=0;
    ys_(8)=0;
    ys_(11)=params(19);
    ys_(9)=ys_(11);
    ys_(10)=0;
    ys_(12)=0;
    % Auxiliary equations
end