%
% Status : main Dynare file
%
% Warning : this file is generated automatically by Dynare
%           from model file (.mod)

if isoctave || matlab_ver_less_than('8.6')
    clear all
else
    clearvars -global
    clear_persistent_variables(fileparts(which('dynare')), false)
end
tic0 = tic;
% Define global variables.
global M_ options_ oo_ estim_params_ bayestopt_ dataset_ dataset_info estimation_info ys0_ ex0_
options_ = [];
M_.fname = 'MVF_gap';
M_.dynare_version = '4.6.0';
oo_.dynare_version = '4.6.0';
options_.dynare_version = '4.6.0';
%
% Some global variables initialization
%
global_initialization;
diary off;
diary('MVF_gap.log');
M_.exo_names = cell(8,1);
M_.exo_names_tex = cell(8,1);
M_.exo_names_long = cell(8,1);
M_.exo_names(1) = {'eps_YBar'};
M_.exo_names_tex(1) = {'eps\_YBar'};
M_.exo_names_long(1) = {'eps_YBar'};
M_.exo_names(2) = {'eps_G'};
M_.exo_names_tex(2) = {'eps\_G'};
M_.exo_names_long(2) = {'eps_G'};
M_.exo_names(3) = {'eps_y'};
M_.exo_names_tex(3) = {'eps\_y'};
M_.exo_names_long(3) = {'eps_y'};
M_.exo_names(4) = {'eps_ppi'};
M_.exo_names_tex(4) = {'eps\_ppi'};
M_.exo_names_long(4) = {'eps_ppi'};
M_.exo_names(5) = {'eps_tcr'};
M_.exo_names_tex(5) = {'eps\_tcr'};
M_.exo_names_long(5) = {'eps_tcr'};
M_.exo_names(6) = {'eps_u'};
M_.exo_names_tex(6) = {'eps\_u'};
M_.exo_names_long(6) = {'eps_u'};
M_.exo_names(7) = {'eps_UBar'};
M_.exo_names_tex(7) = {'eps\_UBar'};
M_.exo_names_long(7) = {'eps_UBar'};
M_.exo_names(8) = {'eps_gUBar'};
M_.exo_names_tex(8) = {'eps\_gUBar'};
M_.exo_names_long(8) = {'eps_gUBar'};
M_.endo_names = cell(12,1);
M_.endo_names_tex = cell(12,1);
M_.endo_names_long = cell(12,1);
M_.endo_names(1) = {'gY_obs'};
M_.endo_names_tex(1) = {'gY\_obs'};
M_.endo_names_long(1) = {'gY_obs'};
M_.endo_names(2) = {'gYBar'};
M_.endo_names_tex(2) = {'gYBar'};
M_.endo_names_long(2) = {'gYBar'};
M_.endo_names(3) = {'G'};
M_.endo_names_tex(3) = {'G'};
M_.endo_names_long(3) = {'G'};
M_.endo_names(4) = {'y'};
M_.endo_names_tex(4) = {'y'};
M_.endo_names_long(4) = {'y'};
M_.endo_names(5) = {'ppi'};
M_.endo_names_tex(5) = {'ppi'};
M_.endo_names_long(5) = {'ppi'};
M_.endo_names(6) = {'ppi_obs'};
M_.endo_names_tex(6) = {'ppi\_obs'};
M_.endo_names_long(6) = {'ppi_obs'};
M_.endo_names(7) = {'tcr'};
M_.endo_names_tex(7) = {'tcr'};
M_.endo_names_long(7) = {'tcr'};
M_.endo_names(8) = {'tcr_obs'};
M_.endo_names_tex(8) = {'tcr\_obs'};
M_.endo_names_long(8) = {'tcr_obs'};
M_.endo_names(9) = {'U_obs'};
M_.endo_names_tex(9) = {'U\_obs'};
M_.endo_names_long(9) = {'U_obs'};
M_.endo_names(10) = {'u'};
M_.endo_names_tex(10) = {'u'};
M_.endo_names_long(10) = {'u'};
M_.endo_names(11) = {'UBar'};
M_.endo_names_tex(11) = {'UBar'};
M_.endo_names_long(11) = {'UBar'};
M_.endo_names(12) = {'gUBar'};
M_.endo_names_tex(12) = {'gUBar'};
M_.endo_names_long(12) = {'gUBar'};
M_.endo_partitions = struct();
M_.param_names = cell(22,1);
M_.param_names_tex = cell(22,1);
M_.param_names_long = cell(22,1);
M_.param_names(1) = {'THETA'};
M_.param_names_tex(1) = {'THETA'};
M_.param_names_long(1) = {'THETA'};
M_.param_names(2) = {'Gss'};
M_.param_names_tex(2) = {'Gss'};
M_.param_names_long(2) = {'Gss'};
M_.param_names(3) = {'PHI'};
M_.param_names_tex(3) = {'PHI'};
M_.param_names_long(3) = {'PHI'};
M_.param_names(4) = {'SIG_eps_YBar'};
M_.param_names_tex(4) = {'SIG\_eps\_YBar'};
M_.param_names_long(4) = {'SIG_eps_YBar'};
M_.param_names(5) = {'SIG_eps_G'};
M_.param_names_tex(5) = {'SIG\_eps\_G'};
M_.param_names_long(5) = {'SIG_eps_G'};
M_.param_names(6) = {'SIG_eps_y'};
M_.param_names_tex(6) = {'SIG\_eps\_y'};
M_.param_names_long(6) = {'SIG_eps_y'};
M_.param_names(7) = {'BETTA'};
M_.param_names_tex(7) = {'BETTA'};
M_.param_names_long(7) = {'BETTA'};
M_.param_names(8) = {'LAMBDA'};
M_.param_names_tex(8) = {'LAMBDA'};
M_.param_names_long(8) = {'LAMBDA'};
M_.param_names(9) = {'KAPPA'};
M_.param_names_tex(9) = {'KAPPA'};
M_.param_names_long(9) = {'KAPPA'};
M_.param_names(10) = {'GAMMA'};
M_.param_names_tex(10) = {'GAMMA'};
M_.param_names_long(10) = {'GAMMA'};
M_.param_names(11) = {'SIG_eps_ppi'};
M_.param_names_tex(11) = {'SIG\_eps\_ppi'};
M_.param_names_long(11) = {'SIG_eps_ppi'};
M_.param_names(12) = {'ppiss'};
M_.param_names_tex(12) = {'ppiss'};
M_.param_names_long(12) = {'ppiss'};
M_.param_names(13) = {'TAU_5'};
M_.param_names_tex(13) = {'TAU\_5'};
M_.param_names_long(13) = {'TAU_5'};
M_.param_names(14) = {'SIG_eps_tcr'};
M_.param_names_tex(14) = {'SIG\_eps\_tcr'};
M_.param_names_long(14) = {'SIG_eps_tcr'};
M_.param_names(15) = {'TAU_1'};
M_.param_names_tex(15) = {'TAU\_1'};
M_.param_names_long(15) = {'TAU_1'};
M_.param_names(16) = {'TAU_2'};
M_.param_names_tex(16) = {'TAU\_2'};
M_.param_names_long(16) = {'TAU_2'};
M_.param_names(17) = {'TAU_3'};
M_.param_names_tex(17) = {'TAU\_3'};
M_.param_names_long(17) = {'TAU_3'};
M_.param_names(18) = {'TAU_4'};
M_.param_names_tex(18) = {'TAU\_4'};
M_.param_names_long(18) = {'TAU_4'};
M_.param_names(19) = {'Uss'};
M_.param_names_tex(19) = {'Uss'};
M_.param_names_long(19) = {'Uss'};
M_.param_names(20) = {'SIG_eps_UBar'};
M_.param_names_tex(20) = {'SIG\_eps\_UBar'};
M_.param_names_long(20) = {'SIG_eps_UBar'};
M_.param_names(21) = {'SIG_eps_gUBar'};
M_.param_names_tex(21) = {'SIG\_eps\_gUBar'};
M_.param_names_long(21) = {'SIG_eps_gUBar'};
M_.param_names(22) = {'SIG_eps_u'};
M_.param_names_tex(22) = {'SIG\_eps\_u'};
M_.param_names_long(22) = {'SIG_eps_u'};
M_.param_partitions = struct();
M_.exo_det_nbr = 0;
M_.exo_nbr = 8;
M_.endo_nbr = 12;
M_.param_nbr = 22;
M_.orig_endo_nbr = 12;
M_.aux_vars = [];
options_.varobs = cell(4, 1);
options_.varobs(1)  = {'ppi_obs'};
options_.varobs(2)  = {'gY_obs'};
options_.varobs(3)  = {'tcr_obs'};
options_.varobs(4)  = {'U_obs'};
options_.varobs_id = [ 6 1 8 9  ];
M_.Sigma_e = zeros(8, 8);
M_.Correlation_matrix = eye(8, 8);
M_.H = 0;
M_.Correlation_matrix_ME = 1;
M_.sigma_e_is_diagonal = true;
M_.det_shocks = [];
options_.linear = true;
options_.block = false;
options_.bytecode = false;
options_.use_dll = false;
options_.linear_decomposition = false;
M_.nonzero_hessian_eqs = [];
M_.hessian_eq_zero = isempty(M_.nonzero_hessian_eqs);
M_.orig_eq_nbr = 12;
M_.eq_nbr = 12;
M_.ramsey_eq_nbr = 0;
M_.set_auxiliary_variables = exist(['./+' M_.fname '/set_auxiliary_variables.m'], 'file') == 2;
M_.epilogue_names = {};
M_.epilogue_var_list_ = {};
M_.orig_maximum_endo_lag = 1;
M_.orig_maximum_endo_lead = 1;
M_.orig_maximum_exo_lag = 0;
M_.orig_maximum_exo_lead = 0;
M_.orig_maximum_exo_det_lag = 0;
M_.orig_maximum_exo_det_lead = 0;
M_.orig_maximum_lag = 1;
M_.orig_maximum_lead = 1;
M_.orig_maximum_lag_with_diffs_expanded = 1;
M_.lead_lag_incidence = [
 0 8 0;
 0 9 0;
 1 10 0;
 2 11 0;
 3 12 20;
 0 13 0;
 4 14 0;
 0 15 0;
 0 16 0;
 5 17 0;
 6 18 0;
 7 19 0;]';
M_.nstatic = 5;
M_.nfwrd   = 0;
M_.npred   = 6;
M_.nboth   = 1;
M_.nsfwrd   = 1;
M_.nspred   = 7;
M_.ndynamic   = 7;
M_.dynamic_tmp_nbr = [0; 0; 0; 0; ];
M_.equations_tags = {
  1 , 'name' , 'gY_obs' ;
  2 , 'name' , 'gYBar' ;
  3 , 'name' , 'G' ;
  4 , 'name' , 'y' ;
  5 , 'name' , 'ppi' ;
  6 , 'name' , 'tcr' ;
  7 , 'name' , 'U_obs' ;
  8 , 'name' , 'u' ;
  9 , 'name' , 'UBar' ;
  10 , 'name' , 'gUBar' ;
  11 , 'name' , 'ppi_obs' ;
  12 , 'name' , 'tcr_obs' ;
};
M_.mapping.gY_obs.eqidx = [1 ];
M_.mapping.gYBar.eqidx = [1 2 ];
M_.mapping.G.eqidx = [2 3 ];
M_.mapping.y.eqidx = [1 4 5 8 ];
M_.mapping.ppi.eqidx = [5 11 ];
M_.mapping.ppi_obs.eqidx = [11 ];
M_.mapping.tcr.eqidx = [5 6 12 ];
M_.mapping.tcr_obs.eqidx = [12 ];
M_.mapping.U_obs.eqidx = [7 ];
M_.mapping.u.eqidx = [7 8 ];
M_.mapping.UBar.eqidx = [7 9 ];
M_.mapping.gUBar.eqidx = [9 10 ];
M_.mapping.eps_YBar.eqidx = [2 ];
M_.mapping.eps_G.eqidx = [3 ];
M_.mapping.eps_y.eqidx = [4 ];
M_.mapping.eps_ppi.eqidx = [5 ];
M_.mapping.eps_tcr.eqidx = [6 ];
M_.mapping.eps_u.eqidx = [8 ];
M_.mapping.eps_UBar.eqidx = [9 ];
M_.mapping.eps_gUBar.eqidx = [10 ];
M_.static_and_dynamic_models_differ = false;
M_.has_external_function = false;
M_.state_var = [3 4 5 7 10 11 12 ];
M_.exo_names_orig_ord = [1:8];
M_.maximum_lag = 1;
M_.maximum_lead = 1;
M_.maximum_endo_lag = 1;
M_.maximum_endo_lead = 1;
oo_.steady_state = zeros(12, 1);
M_.maximum_exo_lag = 0;
M_.maximum_exo_lead = 0;
oo_.exo_steady_state = zeros(8, 1);
M_.params = NaN(22, 1);
M_.endo_trends = struct('deflator', cell(12, 1), 'log_deflator', cell(12, 1), 'growth_factor', cell(12, 1), 'log_growth_factor', cell(12, 1));
M_.NNZDerivatives = [40; 0; -1; ];
M_.static_tmp_nbr = [0; 0; 0; 0; ];
close all;
M_.params(1) = 0.2;
THETA = M_.params(1);
M_.params(2) = 4;
Gss = M_.params(2);
M_.params(3) = 0.76;
PHI = M_.params(3);
M_.params(4) = 0.01;
SIG_eps_YBar = M_.params(4);
M_.params(5) = 0.01;
SIG_eps_G = M_.params(5);
M_.params(6) = 0.01;
SIG_eps_y = M_.params(6);
M_.params(7) = 0.9999;
BETTA = M_.params(7);
M_.params(8) = 0.13;
LAMBDA = M_.params(8);
M_.params(9) = 0.044;
KAPPA = M_.params(9);
M_.params(10) = 0.017;
GAMMA = M_.params(10);
M_.params(11) = 0.01;
SIG_eps_ppi = M_.params(11);
M_.params(12) = 0.75;
ppiss = M_.params(12);
M_.params(13) = 0.645;
TAU_5 = M_.params(13);
M_.params(14) = 0.01;
SIG_eps_tcr = M_.params(14);
M_.params(15) = 0.5;
TAU_1 = M_.params(15);
M_.params(16) = 0.027;
TAU_2 = M_.params(16);
M_.params(17) = 1;
TAU_3 = M_.params(17);
M_.params(18) = 1;
TAU_4 = M_.params(18);
M_.params(19) = 8;
Uss = M_.params(19);
M_.params(22) = 0.01;
SIG_eps_u = M_.params(22);
M_.params(20) = 0.01;
SIG_eps_UBar = M_.params(20);
M_.params(21) = 0.01;
SIG_eps_gUBar = M_.params(21);
steady;
oo_.dr.eigval = check(M_,options_,oo_);
%
% SHOCKS instructions
%
M_.exo_det_length = 0;
M_.Sigma_e(1, 1) = 1;
M_.Sigma_e(2, 2) = 1;
M_.Sigma_e(3, 3) = 1;
M_.Sigma_e(4, 4) = 1;
M_.Sigma_e(5, 5) = 1;
M_.Sigma_e(6, 6) = 1;
M_.Sigma_e(7, 7) = 1;
M_.Sigma_e(8, 8) = 1;
estim_params_.var_exo = zeros(0, 10);
estim_params_.var_endo = zeros(0, 10);
estim_params_.corrx = zeros(0, 11);
estim_params_.corrn = zeros(0, 11);
estim_params_.param_vals = zeros(0, 10);
estim_params_.param_vals = [estim_params_.param_vals; 1, NaN, NaN, NaN, 1, 0.15, 0.1, NaN, NaN, NaN ];
estim_params_.param_vals = [estim_params_.param_vals; 2, 4, NaN, NaN, 5, NaN, NaN, 0, 10, NaN ];
estim_params_.param_vals = [estim_params_.param_vals; 3, NaN, NaN, NaN, 1, 0.6, 0.1, NaN, NaN, NaN ];
estim_params_.param_vals = [estim_params_.param_vals; 4, NaN, 0, NaN, 3, 0.3, 0.3, NaN, NaN, NaN ];
estim_params_.param_vals = [estim_params_.param_vals; 5, NaN, 0, NaN, 3, 0.3, 0.3, NaN, NaN, NaN ];
estim_params_.param_vals = [estim_params_.param_vals; 6, NaN, 0, NaN, 3, 0.3, 0.3, NaN, NaN, NaN ];
estim_params_.param_vals = [estim_params_.param_vals; 8, NaN, NaN, NaN, 1, 0.25, 0.1, NaN, NaN, NaN ];
estim_params_.param_vals = [estim_params_.param_vals; 9, NaN, 0, NaN, 3, 0.15, 0.1, NaN, NaN, NaN ];
estim_params_.param_vals = [estim_params_.param_vals; 10, NaN, 0, NaN, 3, 0.075, 0.07, NaN, NaN, NaN ];
estim_params_.param_vals = [estim_params_.param_vals; 11, NaN, 0, NaN, 3, 0.3, 0.3, NaN, NaN, NaN ];
estim_params_.param_vals = [estim_params_.param_vals; 13, 0.6, NaN, NaN, 5, NaN, NaN, 0, 1, NaN ];
estim_params_.param_vals = [estim_params_.param_vals; 14, NaN, 0, NaN, 3, 3, 1, NaN, NaN, NaN ];
estim_params_.param_vals = [estim_params_.param_vals; 15, NaN, 0, NaN, 3, 0.2, 0.2, NaN, NaN, NaN ];
estim_params_.param_vals = [estim_params_.param_vals; 16, NaN, NaN, NaN, 1, 0.5, 0.25, NaN, NaN, NaN ];
estim_params_.param_vals = [estim_params_.param_vals; 17, NaN, NaN, NaN, 1, 0.5, 0.25, NaN, NaN, NaN ];
estim_params_.param_vals = [estim_params_.param_vals; 18, NaN, NaN, NaN, 1, 0.5, 0.25, NaN, NaN, NaN ];
estim_params_.param_vals = [estim_params_.param_vals; 19, NaN, NaN, NaN, 5, NaN, NaN, 0, 15, NaN ];
estim_params_.param_vals = [estim_params_.param_vals; 22, NaN, 0, NaN, 3, 0.1, 0.2, NaN, NaN, NaN ];
estim_params_.param_vals = [estim_params_.param_vals; 20, NaN, 0, NaN, 3, 0.1, 0.2, NaN, NaN, NaN ];
estim_params_.param_vals = [estim_params_.param_vals; 21, NaN, 0, NaN, 3, 0.1, 0.2, NaN, NaN, NaN ];
set(0,'DefaultFigureWindowStyle' , 'docked')
options_.mh_nblck = 0;
options_.mh_replic = 0;
options_.mode_compute = 0;
options_.nograph = true;
options_.plot_priors = 0;
options_.smoothed_state_uncertainty = true;
options_.smoother = true;
options_.datafile = 'Datos';
options_.mode_file = 'MVF_gap_mode';
options_.xls_range = 'B1:AA2000';
options_.xls_sheet = 'data';
options_.first_obs = 5;
options_.nobs = 74;
options_.order = 1;
var_list_ = {};
oo_recursive_=dynare_estimation(var_list_);
options_.no_graph.shock_decomposition = true;
options_.parameter_set = 'posterior_mode';
var_list_ = {};
oo_ = shock_decomposition(M_,oo_,options_,var_list_,bayestopt_,estim_params_);
global initial_date_graph;
initial_date_graph='2001Q1';
options_ = set_default_plot_shock_decomposition_options(options_);
options_.plot_shock_decomp.steadystate = true;
options_.plot_shock_decomp.type = 'qoq';
var_list_ = {'ppi_obs';'gY_obs';'gYBar'};
oo_ = plot_shock_decomposition(M_, oo_, options_, var_list_);
options_.irf = 20;
options_.nograph = true;
options_.order = 1;
options_.periods = 0;
var_list_ = {};
[info, oo_, options_] = stoch_simul(M_, options_, oo_, var_list_);
save MVF_gap.mat oo_ M_ options_;
save('MVF_gap_results.mat', 'oo_', 'M_', 'options_');
if exist('estim_params_', 'var') == 1
  save('MVF_gap_results.mat', 'estim_params_', '-append');
end
if exist('bayestopt_', 'var') == 1
  save('MVF_gap_results.mat', 'bayestopt_', '-append');
end
if exist('dataset_', 'var') == 1
  save('MVF_gap_results.mat', 'dataset_', '-append');
end
if exist('estimation_info', 'var') == 1
  save('MVF_gap_results.mat', 'estimation_info', '-append');
end
if exist('dataset_info', 'var') == 1
  save('MVF_gap_results.mat', 'dataset_info', '-append');
end
if exist('oo_recursive_', 'var') == 1
  save('MVF_gap_results.mat', 'oo_recursive_', '-append');
end


disp(['Total computing time : ' dynsec2hms(toc(tic0)) ]);
if ~isempty(lastwarn)
  disp('Note: warning(s) encountered in MATLAB/Octave code')
end
diary off
