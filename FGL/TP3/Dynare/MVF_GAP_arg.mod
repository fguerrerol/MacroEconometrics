close all;

// ==================================================
// VARIABLES, ENDOGENOUS
// ==================================================
var gY_obs // _observed GDP growth
    gYBar // growth of potential GDP level
    G // Potential growth
    y // output gap
    ppi // inflation
    ppi_obs // _observed inflation
    tcr // real exchange rate
    tcr_obs // _observed tcr
    U_obs // _observed unemployment
    u // unemployment gap
    UBar // Natural unemployment level
    gUBar // Natural unemployment growth

;

// ==================================================
// VARIABLES, EXOGENOUS
// ==================================================
varexo eps_YBar // shock potential gdp level
       eps_G    // shock potential gdp growth
       eps_y    // shock to output gap
       eps_ppi  // shock to inflation
       eps_tcr  // shock to tcr
       eps_u    // shock to unemployment gap
       eps_UBar // shock to natural unemployment level
       eps_gUBar// shock to natural unemployment growth
;
// ==================================================
// PARAMETERS
// ==================================================

parameters THETA Gss PHI SIG_eps_YBar SIG_eps_G SIG_eps_y // Related to output
           BETTA LAMBDA KAPPA GAMMA SIG_eps_ppi ppiss// Related to inflation 
           TAU_5 SIG_eps_tcr// Related to TCR
           TAU_1 TAU_2 TAU_3 TAU_4 Uss SIG_eps_UBar SIG_eps_gUBar SIG_eps_u //Related to unemployment
;

// ==================================================
// CALIBRATION: Posterior mode in BCCh paper 2015
// ==================================================
// Related to output
THETA=0.2; 
Gss=4; 
PHI=0.76;
SIG_eps_YBar =0.01;
SIG_eps_G=0.01; 
SIG_eps_y=0.01;  

// Related to inflation 
BETTA=0.9999; 
LAMBDA=0.13; 
KAPPA=0.044; 
GAMMA=0.017; 
SIG_eps_ppi=0.01;  
ppiss=3/4;

// Related to TCR           
TAU_5=0.645 ;
SIG_eps_tcr=0.01; 

//Related to unemployment
TAU_1=0.5; 
TAU_2=0.027; 
TAU_3=1; 
TAU_4=1; 
Uss=8; 
SIG_eps_u=0.01;  
SIG_eps_UBar=0.01;  
SIG_eps_gUBar=0.01;  

// ==================================================
// MODEL
// ==================================================

model(linear);

gY_obs = gYBar +y - y(-1);// _observed GDP growth
gYBar = G + SIG_eps_YBar*eps_YBar; // growth of potential GDP level
G = THETA*Gss/4 + (1-THETA)*G(-1) + SIG_eps_G*eps_G; // potential GDP growth 
y = PHI*y(-1) + SIG_eps_y*eps_y; // output gap

ppi=LAMBDA/(1+BETTA*LAMBDA)*ppi(-1) + BETTA/(1+BETTA*LAMBDA)*ppi(+1) + KAPPA*y + GAMMA*tcr + SIG_eps_ppi*eps_ppi; // Inflation

tcr=TAU_5*tcr(-1) + SIG_eps_tcr*eps_tcr; //TCR

U_obs = u + UBar; // _observed unemployment
u = -TAU_1*y + TAU_2*u(-1) + SIG_eps_u*eps_u; // unemployment gap
UBar = TAU_4*Uss + (1-TAU_4)*UBar(-1) + gUBar + SIG_eps_UBar*eps_UBar; // Natural unemployment level
gUBar = (1-TAU_3)*gUBar(-1) + SIG_eps_gUBar*eps_gUBar ;   // Natural unemployment growth

ppi_obs = ppi + ppiss;
tcr_obs = tcr;

end;

// ==================================================
// STEADY STATE
// ==================================================
steady_state_model;

G = Gss/4;
gY_obs = G;
gYBar = G;
y = 0;

ppi=0;
ppi_obs =ppiss;

tcr=0;
tcr_obs=0;

UBar = Uss ;
U_obs = UBar; 
u = 0;
gUBar = 0;

end;

steady;
check;


// ==================================================
// CALIBRATED SHOCK SCALES
// ==================================================
shocks;
var eps_YBar =1;
var eps_G =1;
var eps_y =1;
var eps_ppi =1;
var eps_tcr =1;
var eps_u =1;
var eps_UBar =1;
var eps_gUBar =1;
end;



// ==================================================
// DECLARATION OF OBSERVABLE VARIABLES
// ==================================================
varobs ppi_obs	gY_obs	tcr_obs	U_obs;

// ==================================================
// PRIORS
// ==================================================
estimated_params;

// Related to GDP
THETA        ,  ,  ,  , beta_pdf, 0.15 , 0.1 ,  ,  ;
Gss          ,  , 0,  , normal_pdf, 3.2 , 3 ,  0 , 10 ;
PHI          ,  ,  ,  , beta_pdf, 0.5 , 0.25 ,  ,  ;
SIG_eps_YBar ,  , 0,  , normal_pdf, 0.5 , 0.6 ,  ,  ;
SIG_eps_G    ,  , 0,  , normal_pdf, 0.5 , 0.6 ,  ,  ;
SIG_eps_y    ,  , 0,  , normal_pdf, 0.5 , 0.6 ,  ,  ;

// Related to inflation  
// BETTA is calibrated
LAMBDA       ,  ,  ,  , uniform_pdf, 0.25 , 0.1 ,  ,  ;
KAPPA        ,  , 0,  , normal_pdf, 0.15  , 0.1 ,  ,  ;
GAMMA        ,  , 0,  , normal_pdf, 0.075 , 0.07 ,  ,  ;
SIG_eps_ppi  ,  , 0,  , normal_pdf, 0.3 , 0.3 ,  ,  ;
// ppiss is calibrated

// Related to TCR 
TAU_5       ,0.6 ,  ,  , uniform_pdf,  ,  ,  0 , 1 ; //beta_pdf, 0.75 , 0.1 ,  ,  ;
SIG_eps_tcr ,  , 0,  , normal_pdf, 3 , 1 ,  ,  ;

//Related to unemployment 
TAU_1         ,  , 0,  , normal_pdf, 0.2 , 0.2 ,  ,  ;
TAU_2         ,  ,  ,  , beta_pdf, 0.5 , 0.25 ,  ,  ;
TAU_3         ,  ,  ,  , beta_pdf, 0.5 , 0.25 ,  ,  ;
TAU_4         ,  ,  ,  , beta_pdf, 0.5 , 0.25 ,  ,  ;
Uss           ,  ,  ,  , uniform_pdf,  ,  ,  0, 15  ;
SIG_eps_u     ,  , 0,  , normal_pdf, 0.1 , 0.2 ,  ,  ; // ,  ,  ,  , inv_gamma_pdf, 0.01 , 0.2 ,  ,  ;//
SIG_eps_UBar  ,  , 0,  , normal_pdf, 0.1 , 0.2 ,  ,  ;
SIG_eps_gUBar ,  , 0,  , normal_pdf, 0.1 , 0.2 ,  ,  ;

end;	

// Option to dock all graph produced by Dynare in the same window
set(0,'DefaultFigureWindowStyle' , 'docked')

/*
		
// ==================================================
// BAYESIAN ESTIMATION: Maximizing the Posterior, sample 2001.Q1-2019.Q2.
// ==================================================
estimation(datafile = Datos, xls_sheet = data, xls_range = B1:AA2000, first_obs = 5, nobs = 74, mode_compute = 5, plot_priors = 1, mode_check, mh_replic = 0, mh_nblocks = 0);

*/


// ==================================================
// Reading previously estimated mode; computing smoother; ploting historical decomposition; computing moments and irf at the posterior model
// ==================================================
estimation(datafile = Datos, xls_sheet = data, xls_range = B1:AA2000, first_obs = 5, nobs = 74, mode_compute = 0, mode_file = MVF_gap_mode, plot_priors = 0, mh_replic = 0,  mh_nblocks = 0, smoother, smoothed_state_uncertainty, nograph);

shock_decomposition(parameter_set=posterior_mode, nograph);

global initial_date_graph;
initial_date_graph='2001Q1';
plot_shock_decomposition(steadystate, type = qoq) ppi_obs gY_obs gYBar;

stoch_simul(periods = 0, irf = 20, order = 1, nograph); 

save MVF_gap.mat oo_ M_ options_;


/*
// ==================================================
// BAYESIAN ESTIMATION: Running M-H. mh_jscale already tunned in
   mode_compute = 0, mode_file = MVF_gap_mode,  mh_replic = 10000, mh_nblocks = 1, mh_jscale = 0.32);

// ==================================================
// CHAINS & RECURSIVE MEANS
// ==================================================
model_name  = 'MVF_gap';    // name of the mod-file
blocks_mh   = 1;        // blocks that you want to read (it can be a vector if you have more than one block)
nrows       = 3;        // number of rows per figure
ncols       = 3;        // number of columns per figure
plots_for_dynare_chains(model_name,blocks_mh,nrows,ncols,oo_);
*/

/*
// ==================================================
// Reading Previosly generated MH, Save Bayesian IRF
// ==================================================
estimation(datafile = Datos, xls_sheet = data, xls_range = B1:AA2000, first_obs = 5, nobs = 74, mode_compute = 0, mode_file = MVF_gap_mode, plot_priors = 0, diffuse_filter, mh_replic = 0, mh_nblocks = 1, load_mh_file, load_results_after_load_mh, bayesian_irf, posterior_nograph);
stoch_simul(periods = 0, irf = 20, order = 1, nograph); 
save MVF_gap_mhresults.mat oo_ M_ options_;

*/
