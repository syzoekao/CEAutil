library(CEAutil)

treetxt <- parse_amua_tree("/Users/szu-yukao/Documents/CEA/CEAcode/AmuaExample/DraculaParty_Export/main.R")
param_ls <- treetxt[["param_ls"]]
treefunc <- treetxt[["treefunc"]]

tree_output <- dectree_wrapper(params_basecase = param_ls, treefunc = treefunc, popsize = 200)

print(tree_output)



treetxt <- parse_amua_tree("/Users/szu-yukao/Documents/CEA/CEAcode/AmuaExample/IPS_Export/main.R")
param_ls <- treetxt[["param_ls"]]
treefunc <- treetxt[["treefunc"]]

tree_output <- dectree_wrapper(params_basecase = param_ls, treefunc = treefunc, popsize = 200)

print(tree_output)

library(CEAutil)
library(data.table)
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
                               FUN = markov_model,
                               strategy_set = c("null", "targeted ban", "universal ban"))




