#############################################
# Markov model
#############################################
if(!require(dampack)) remotes::install_github("DARTH-git/dampack", dependencies = TRUE)
if(!require(CEAutil)) remotes::install_github("syzoekao/CEAutil", dependencies = TRUE)

library(CEAutil)
library(dampack)

#### 1. Read in, set, or transform parameters if needed

p_nontan_to_cancer <- 0.005
p_regtan_to_cancer <- 0.04
p_cancer_to_dead <- 0.07
n_worker <- 1000
salary <- 28000
target_red <- 0.1
universal_red <- 0.8

data(ONtan)
ltable <- ONtan$lifetable
behavior <- ONtan$behavior

print(head(ltable))
print(head(behavior))

n_t <- 100
state_names <- c("nontan", "regtan", "cancer", "deadnature", "deadcancer")
v_init <- c(1, 0, 0, 0, 0)


# We need the age index for matching values of the lifetable and behavior data.
ages <- c(10 : (10 + n_t - 1))

# We extract the probability of natural death age 10-110 from the lifetable.
p_mort <- ltable$qx[match(ages, ltable$age)]

# Strategy
## We modify the values of tanning behavior based on strategy of interest.
strategy <- "null"
if (strategy == "targeted_ban") {
  behavior$p_init_tanning[behavior$age <= 18] <- 0
}
if (strategy == "universal_ban") {
  behavior$p_init_tanning <- 0
}

# We extract the tanning behavior at age 10-110 from the behavior data
p_init_tan <- behavior$p_init_tanning[match(ages, behavior$age)]
p_halt_tan <- behavior$p_halt_tanning[match(ages, behavior$age)]

# We get the number of health states based on the length of the string vector, state_names
state_names <- state_names
n_states <- length(state_names)

# Get the time horizon
n_t <- n_t

#### 2. Create transition matrix
tr_mat <- array(0, dim = c(n_states, n_states, n_t),
                dimnames = list(state_names, state_names, ages))

# 1. Fill out the transition probabilities from non-tanner to other states
tr_mat["nontan", "regtan", ] <- p_init_tan
tr_mat["nontan", "cancer", ] <- p_nontan_to_cancer
tr_mat["nontan", "deadnature", ] <- p_mort
tr_mat["nontan", "nontan", ] <- 1 - p_init_tan - p_nontan_to_cancer - p_mort

# 2. Fill out the transition probabilities from regular tanner to other states
tr_mat["regtan", "nontan", ] <- p_halt_tan
tr_mat["regtan", "cancer", ] <- p_regtan_to_cancer
tr_mat["regtan", "deadnature", ] <- p_mort
tr_mat["regtan", "regtan", ] <- 1 - p_halt_tan - p_regtan_to_cancer - p_mort

# 3. Fill out the transition probabilities from skin cancer to other states.
#    Be careful that this is a tunnel state. Therefore, there is no self loop.
tr_mat["cancer", "deadcancer", ] <- p_cancer_to_dead
tr_mat["cancer", "deadnature", ] <- p_mort
tr_mat["cancer", "nontan", ] <- 1 - p_cancer_to_dead - p_mort

# 4. Fill out the transition probabilities for cancer specific death (this is an absorbing state!!)
tr_mat["deadcancer", "deadcancer", ] <- 1

# 5. Fill out the transition probabilities for natural death (this is an absorbing state!!)
tr_mat["deadnature", "deadnature", ] <- 1

print(tr_mat[, , "20"])
print(tr_mat[, , "50"])

## Check if transition matrix is valid
# Check whether the transition matrices have any negative values or values > 1!!!
if (sum(tr_mat > 1) | sum(tr_mat < 0)) stop("there are invalid transition probabilities")

# Check whether each row of a transition matrix sum up to 1!!!
if (any(rowSums(t(apply(tr_mat, 3, rowSums))) != n_states)) stop("transition probabilities do not sum up to one")


#### 3. Create the trace matrix to track the changes in the population distribution through time
#### You could also create other matrix to track different outcomes,
#### e.g., costs, incidence, etc.
trace_mat <- matrix(0, ncol = n_states, nrow = n_t + 1,
                    dimnames = list(c(10 : (10 + n_t)), state_names))
# Modify the first row of the trace_mat using the v_init
trace_mat[1, ] <- v_init

# Suppose that we want to track the cost of having skin cancer for a year
trace_cost <- rep(0, n_t)

for(t in 1 : n_t){
  trace_mat[t + 1, ] <- trace_mat[t, ] %*% tr_mat[, , t]
}

#### 5. Organize outputs
# Cost
tot_cost <- 0 # if there is no tanning ban
if (strategy == "targeted_ban") {
  tot_cost <- n_worker * target_red * salary
}
if (strategy == "universal_ban") {
  tot_cost <- n_worker * universal_red * salary
}

# Life expectancy
LE <- sum(rowSums(trace_mat[, !grepl("dead", state_names)])) - 1

# Output table
output <- data.frame("strategy" = strategy,
                     "LE" = LE,
                     "Cost" = tot_cost)
print(output)

#### Put everything in a function
vary_param_ls <- list(p_nontan_to_cancer = 0.005,
                      p_regtan_to_cancer = 0.04,
                      p_cancer_to_dead = 0.07,
                      n_worker = 1000,
                      salary = 28000,
                      target_red = 0.1,
                      universal_red = 0.8)

other_input_ls <- list(ltable = ltable,
                       behavior = behavior,
                       state_names = c("nontan", "regtan", "cancer", "deadnature", "deadcancer"),
                       n_t = 100,
                       v_init = c(1, 0, 0, 0, 0))

res <- markov_decision_wrapper(vary_param_ls = vary_param_ls,
                               other_input_ls = other_input_ls,
                               userfun = markov_model,
                               strategy_set = c("null", "targeted_ban", "universal_ban"))
print(res)


#### Cost effectiveness analysis
tan_icer <- calculate_icers(res$Cost,
                            res$LE,
                            res$strategy)
print(tan_icer)
plot(tan_icer)
