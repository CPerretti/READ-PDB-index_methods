#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(log_obs_DFO);        // log scale values of expanded surveys
  DATA_VECTOR(log_obs_spring);
  DATA_VECTOR(log_obs_fall);
  DATA_VECTOR(DFO_sd);             // log scale standard deviations of surveys (computed from CVs in R code)
  DATA_VECTOR(spring_sd);
  DATA_VECTOR(fall_sd);
  
  PARAMETER_VECTOR(logB);          // population biomass on log scale
  PARAMETER(log_process_error);    // process error for population
  PARAMETER(log_obs_error_DFO);    // additional observation error for each survey (survey CV doesn't account for all variability)
  PARAMETER(log_obs_error_spring);
  PARAMETER(log_obs_error_fall);
  
  Type process_error = exp(log_process_error);
  Type obs_error_DFO = exp(log_obs_error_DFO);
  Type obs_error_spring = exp(log_obs_error_spring);
  Type obs_error_fall = exp(log_obs_error_fall);
  
  int n_obs = log_obs_DFO.size();  // number of observations
  
  Type nll=0;                      // negative log likelihood
  
  for(int y = 1; y < n_obs; y++){      // likelihood for state transitions
    //Type m = logB[y-1];
    nll -= dnorm(logB[y], logB[y-1], process_error, true);
  }
  
  for(int y = 0; y < n_obs; y++){      // likelihood for observations, note addition of obs_error and year-specific sd
    nll -= dnorm(log_obs_DFO[y],    logB[y], obs_error_DFO + DFO_sd[y],       true);
    nll -= dnorm(log_obs_spring[y], logB[y], obs_error_spring + spring_sd[y], true);
    nll -= dnorm(log_obs_fall[y],   logB[y], obs_error_fall + fall_sd[y],     true);
  }
  
  ADREPORT(process_error);
  ADREPORT(obs_error_DFO);
  ADREPORT(obs_error_spring);
  ADREPORT(obs_error_fall);
  
  return nll;
}
