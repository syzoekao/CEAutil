library(CEAutil)
library(dampack)

treetxt <- parse_amua_tree("AmuaExample/DraculaParty_Export/main.R")
param_ls <- treetxt[["param_ls"]]
treefunc <- treetxt[["treefunc"]]

tree_output <- dectree_wrapper(params_basecase = param_ls, treefunc = treefunc, popsize = 200)


params_range <- data.frame(pars = c("p_bite", "C_drug", "p_inf_bitten"), 
                           min = c(0.05, 2, 0.2), 
                           max = c(0.8, 20, 0.9))

owsa_out <- run_owsa_det(params_range, param_ls, nsamp = 100, 
                         dectree_wrapper, 
                         # outcomes = c("expectedEff1", "expectedCost"), 
                         treefunc = treefunc, popsize = 200)
plot(owsa_out$owsa_expectedEff1)
plot(owsa_out$owsa_expectedEff2)
plot(owsa_out$owsa_expectedCost)

params_range <- data.frame(pars = c("p_bite", "C_drug"), 
                           min = c(0.05, 2), 
                           max = c(0.8, 20))
twsa_out <- run_twsa_det(params_range, param_ls, nsamp = 40, 
                         dectree_convert, 
                         # outcomes = c("expectedEff1", "expectedCost"), 
                         treefunc = treefunc, popsize = 200)
plot(twsa_out$twsa_expectedEff1)
plot(twsa_out$twsa_expectedEff2)
plot(twsa_out$twsa_expectedCost)


my_params <- c("p_bite", "C_hospital")
my_dists <- c("beta", "gamma")
my_parameterization_types <- c("mean, sd", "mean, sd")
my_dists_params <- list(c(0.25, 0.1), c(500, 250))

l_params_vary <- gen_psa_samp(params = my_params,
                              dists = my_dists,
                              parameterization_types = my_parameterization_types,
                              dists_params = my_dists_params)

res <- dectree_wrapper(params_basecase = param_ls, 
                       treefunc = treefunc, 
                       popsize = 1, 
                       vary_param_samp = l_params_vary)


psa_out <- run_psa(psa_samp = l_params_vary,
                   params_basecase = param_ls, 
                   FUN = dectree_convert,
                   outcomes = c("Eff1", "Cost"), 
                   treefunc = treefunc, popsize = 200)


## Markov model

library(CEAutil)
library(data.table)
library(dampack)
data(ONtan)
ltable <- ONtan$lifetable
behavior <- ONtan$behavior

vary_param_ls <- list(p_nontan_to_cancer = 0.005,
                      p_regtan_to_cancer = 0.04,
                      p_cancer_to_dead = 0.07)

other_input_ls <- list(ltable = ltable, 
                       behavior = behavior, 
                       state_names = c("nontan", "regtan", "cancer", "deadnature", "deadcancer"),
                       n_t = 100,
                       v_init = c(1, 0, 0, 0, 0))

res <- markov_decision_wrapper(vary_param_ls = vary_param_ls, 
                        other_input_ls = other_input_ls, 
                        userfun = markov_model, 
                        strategy_set = c("null", "targeted ban", "universal ban"))

params_range <- data.frame(pars = c("p_regtan_to_cancer", "p_cancer_to_dead"), 
                           min = c(0.01, 0.01), 
                           max = c(0.1, 0.1))

owsa_out <- run_owsa_det(params_range, vary_param_ls, nsamp = 100, 
                         FUN = markov_decision_wrapper, 
                         # outcomes = c("expectedEff1", "expectedCost"), 
                         userfun = markov_model, 
                         other_input_ls = other_input_ls, 
                         strategy_set = c("null", "targeted ban", "universal ban"))
plot(owsa_out)

twsa_out <- run_twsa_det(params_range, vary_param_ls, nsamp = 40, 
                         FUN = markov_decision_wrapper, 
                         # outcomes = c("expectedEff1", "expectedCost"), 
                         userfun = markov_model, 
                         other_input_ls = other_input_ls, 
                         strategy_set = c("null", "targeted ban", "universal ban"))
plot(twsa_out)


#### DAMPACK intro

library(CEAutil)
library(dampack)

treetxt <- parse_amua_tree("AmuaExample/DraculaParty_Export/main.R")
param_ls <- treetxt[["param_ls"]]
treefunc <- treetxt[["treefunc"]]

tree_output <- dectree_wrapper(params_basecase = param_ls, treefunc = treefunc, popsize = 200)


params_range <- data.frame(pars = c("p_bite", "C_drug", "p_inf_bitten"), 
                           min = c(0.05, 2, 0.2), 
                           max = c(0.8, 20, 0.9))

owsa_out <- run_owsa_det(params_range, param_ls, nsamp = 100, 
                         dectree_wrapper, 
                         # outcomes = c("expectedEff1", "expectedCost"), 
                         treefunc = treefunc, popsize = 200)
plot(owsa_out$owsa_expectedEff1)
plot(owsa_out$owsa_expectedEff2)
plot(owsa_out$owsa_Cost)


params_range <- data.frame(pars = c("p_bite", "C_drug"), 
                           min = c(0.05, 2), 
                           max = c(0.8, 20))
twsa_out <- run_twsa_det(params_range, param_ls, nsamp = 40, 
                         dectree_convert, 
                         # outcomes = c("expectedEff1", "expectedCost"), 
                         treefunc = treefunc, popsize = 200)
plot(twsa_out$twsa_expectedEff1)
plot(twsa_out$twsa_expectedEff2)
plot(twsa_out$twsa_Cost)

