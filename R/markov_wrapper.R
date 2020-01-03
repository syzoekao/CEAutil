#' @title Example of Markov model function
#'
#' @description This Markov model function using Ontario tanning ban as an example
#'
#' @param l_param_all All parameter inputs. This is a named list organized as
#'        `list(par1 = 0.3, par2 = 0.5, df1 = data.frame(t = c(1:3), param = c(0.2, 0.2, 0.3)))`
#' @param strategy The strategy of interest. The default is `NULL`, which is the do nothing strategy.
#'
#' @return The function returns a `data.frame` with first column as the strategy of interest,
#'         and the other columns are the outcomes of interest.
#'
#' @export
markov_model <- function(l_param_all,
                         strategy = NULL) {
  with(as.list(l_param_all), {
    #### Step 1. Read in, set, or transform parameters if needed
    ages <- c(10 : (10 + n_t - 1))
    p_mort <- ltable$qx[match(ages, ltable$age)]

    if (strategy == "targeted ban") {
      behavior$p_init_tanning[behavior$age <= 18] <- 0
    }
    if (strategy == "universal ban") {
      behavior$p_init_tanning <- 0
    }

    p_init_tan <- behavior$p_init_tanning[match(ages, behavior$age)]
    p_halt_tan <- behavior$p_halt_tanning[match(ages, behavior$age)]

    n_states <- length(state_names)

    #### Step 2. Create the transition probability matrices using array
    tr_mat <- array(0, dim = c(n_states, n_states, n_t),
                    dimnames = list(state_names, state_names, ages))

    ## Start filling out transition probabilities in the array
    ## When you fill out the transition probabilities, always remember to deal with
    ## the self-loop the LAST!!!!!
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

    # Check whether the transition matrices have any negative values or values > 1!!!
    if (sum(tr_mat > 1) | sum(tr_mat < 0)) stop("there are invalid transition probabilities")

    # Check whether each row of a transition matrix sum up to 1!!!
    if (any(rowSums(t(apply(tr_mat, 3, rowSums))) != n_states)) stop("transition probabilities do not sum up to one")

    #### Step 3. Create the trace matrix to track the changes in the population distribution through time
    #### You could also create other matrix to track different outcomes,
    #### e.g., costs, incidence, etc.
    trace_mat <- matrix(0, ncol = n_states, nrow = n_t + 1,
                        dimnames = list(c(10 : (10 + n_t)), state_names))
    # Modify the first row of the trace_mat using the v_init
    trace_mat[1, ] <- v_init

    # Suppose that we want to track the cost of having skin cancer for a year
    trace_cost <- rep(0, n_t)

    #### Step 4. Compute the Markov model over time by iterating through time steps
    for(t in 1 : n_t){
      trace_mat[t + 1, ] <- trace_mat[t, ] %*% tr_mat[, , t]
    }

    #### Step 5. Organize outputs
    LE <- sum(rowSums(trace_mat[, !grepl("dead", state_names)])) - 1
    output <- data.frame("strategy" = strategy,
                         "LE" = LE)

    #### Step 6. Return the relevant results
    return(output)
  })
}


#' @title Wrapper function running a Markov model for each strategy.
#'
#' @description This is a wrapper function to calculate the outcome for every strategy.
#'
#' @param vary_param_ls This argument takes a list of parameters that are varied in deterministic
#'        or probabilistic sensitivity analyses. The list should be named and the length of each element
#'        should be 1. For example, `list(par1 = 0.2, par2 = 0.3, par3 = 0.2)`.
#' @param other_input_ls The argument is a named list. This argument takes parameters or data that do not
#'        change in deterministic or probablistic sensitivity analysis.
#' @param userfun This is a user defined markov model function. See the example function `markov_model()`.
#' @param strategy_set The set of strategies that are embedded in the `markov_model()`.
#'
#' @return The function returns a `data.frame` with first column as the strategies, the other columns are
#'         the outcomes of interest.
#'
#' @export
markov_decision_wrapper <- function(vary_param_ls = NULL, other_input_ls = NULL,
                                    userfun, strategy_set) {
  if (is.null(vary_param_ls) & is.null(other_input_ls)) {
    stop("users should specify at least one of vary_param_ls and other_input_ls")
  }

  if (!is.null(vary_param_ls) & is.null(names(vary_param_ls))) {
    stop("vary_param_ls requires names")
  }

  if (!is.null(other_input_ls) & is.null(names(other_input_ls))) {
    stop("other_input_ls requires names")
  }

  if (is.null(strategy_set)) stop("please provide the list of strategies")

  intersect_param <- intersect(names(vary_param_ls), names(other_input_ls))
  if (length(intersect_param) > 0) {
    stop(paste0(intersect_param, " cannot be in both vary_param_ls or other_input_ls"))
  }

  l_param_all <- c(vary_param_ls, other_input_ls)

  outdf <- lapply(strategy_set, function(x, tmp_param = l_param_all) {
    userfun(tmp_param, strategy = x)
  })

  outdf <- do.call(rbind, outdf)
  return(outdf)
}
