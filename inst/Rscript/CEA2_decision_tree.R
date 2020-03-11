#############################################
# Decision tree
#############################################

#### download requred packages and attach CEAutil package
if(!require("remotes")) install.packages("remotes")
if(!require("dplyr")) install.packages("dplyr")
if(!require("CEAutil")) remotes::install_github("syzoekao/CEAutil", dependencies = TRUE)
if(!require(dampack)) remotes::install_github("DARTH-git/dampack", dependencies = TRUE)

library(CEAutil)

#### Use function in CEAutil to extract exported R code from AMUA
treetxt <- parse_amua_tree("inst/rmd/AmuaExample/DraculaParty_Export/main.R")

print(treetxt$param_ls)

param_ls <- treetxt[["param_ls"]]
treefunc <- treetxt[["treefunc"]]

#### Calculate cost and effectiveness
tree_output <- dectree_wrapper(params_basecase = param_ls, treefunc = treefunc, popsize = 1)
print(tree_output)

tree_output <- dectree_wrapper(params_basecase = param_ls, treefunc = treefunc, popsize = 200)
print(tree_output)

#### Calculate ICERs
library(dampack)
dracula_icer <- calculate_icers(tree_output$Cost,
                                tree_output$expectedBlood,
                                tree_output$strategy)
print(dracula_icer)
plot(dracula_icer, effect_units = "mL")

#### Change parameter values
### Example 1: p_bite increased to 0.5
param_ls$p_bite <- 0.5
tree_output <- dectree_wrapper(params_basecase = param_ls, treefunc = treefunc, popsize = 200)
print(tree_output)

dracula_icer <- calculate_icers(tree_output$Cost,
                                tree_output$expectedBlood,
                                tree_output$strategy)
print(dracula_icer)
plot(dracula_icer, effect_units = "mL")


### Example 2: allowing the cost of hospitalization to follow a distribution
## Generate random samples for the cost of hospitalization
C_hospital <- gen_psa_samp(params = c("C_hospital"),
                           dists = c("gamma"),
                           parameterization_types = c("mean, sd"),
                           dists_params = list(c(500, 250)),
                           nsamp = 200)

cat("mean and sd are", c(mean(C_hospital$C_hospital), sd(C_hospital$C_hospital)), ", respectively.")

hist(C_hospital$C_hospital, main = "histogram", xlab = "values", ylab = "frequency", col = "gray", border = F)
abline(v = mean(C_hospital$C_hospital), col = "firebrick", lwd = 3)
text(mean(C_hospital$C_hospital) + 50, 30, "mean\ncost", col = "firebrick", font = 2)

## Calculate the cost and effectiveness for each sample of cost
tree_vary <- dectree_wrapper(params_basecase = param_ls,
                             treefunc = treefunc,
                             vary_param_samp = C_hospital)


## Tree outputs
print(names(tree_vary))
print(head(tree_vary$param_samp))
print(head(tree_vary$expectedBlood))
print(head(tree_vary$Cost))
print(summary(tree_vary$Cost))

hist(tree_vary$Cost$Donothing, col = rgb(0.5, 0.5, 0.5, 0.6), border = F,
     main = c("Cost of hospitalization"),
     xlab = "Cost")
hist(tree_vary$Cost$Targetedantibiotics, col = rgb(0.2, 0.2, 0.8, 0.5), border = F, add = T)
hist(tree_vary$Cost$Universalantibiotics, col = rgb(0.3, 0.8, 0.2, 0.5), border = F, add = T)
legend("topright", c("do nothing", "targeted", "universal"),
       col = c(rgb(0.5, 0.5, 0.5, 0.6), rgb(0.2, 0.2, 0.8, 0.5), rgb(0.3, 0.8, 0.2, 0.5)),
       pch = 15, pt.cex = 2)

