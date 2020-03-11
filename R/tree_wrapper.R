#' @title Parsing text file of the decision tree model exported from AMUA
#'
#' @description This function is to parse the decision tree model exported from AMUA. The function returns
#'              a list of three elements: `param_names`, `param_val`, and `treefunc`.
#'
#' @param path_to_txt A string of the path to the decision tree model exported from AMUA.
#'
#' @return The function returns a list of three elements: `param_names`, `param_val`, and `treefunc`.
#'     \item{param_ls}{A list of parameters with parameter names.}
#'     \item{treefunc}{Text of the decision tree exported from AMUA.}
#'
#' @export
parse_amua_tree <- function(path_to_txt) {
  if (!is.character(path_to_txt)) stop("path_to_txt should be a string")


  txt <- readLines(path_to_txt)
  ix_palm <- which(grepl("^###", txt))

  ix_param <- which(txt %in% c("### Define parameters"))
  ix_param <- c(ix_param, ix_palm[which(ix_palm == ix_param) + 1])
  ix_param <- c(ix_param[1] : ix_param[2])
  param_chunk <- txt[ix_param[-c(1, length(ix_param))]]
  tmp_nchar <- unlist(lapply(c(1:length(param_chunk)), function(x) nchar(param_chunk[[x]])))
  param_chunk <- param_chunk[which(tmp_nchar > 0)]

  param_chunk <- gsub(" ", "", param_chunk)
  param_chunk <- gsub("<-", " ", param_chunk)
  param_chunk <- gsub("#.*", "", param_chunk)
  param_ls <- strsplit(param_chunk, " ")
  param_names <- unlist(lapply(param_ls, function(x) x[1]))
  param_val <- lapply(param_ls, function(x) as.numeric(x[2]))
  names(param_val) <- param_names

  ix_tree <- which(txt %in% c("### Define Node Class", "### Display output for each strategy"))
  ix_tree <- c(ix_tree[1] : (ix_tree[2] + 1))

  txt[ix_param[2:length(ix_param)]] <- ""
  txt[ix_param[2]] <- " for(i in c(1:length(params_basecase))) {"
  txt[ix_param[3]] <- "  assign(names(params_basecase)[i], params_basecase[[i]])"
  txt[ix_param[4]] <- " }"

  ix_class <- which(grepl("setRefClass", txt))
  ix_tmp <- which(grepl(")", txt))
  ix_tmp <- ix_tmp[min(which(ix_tmp > ix_class)) + 1]
  txt[ix_tmp] <- ", where = parent.frame())"

  tree_chunk <- txt[ix_tree]
  return(list("param_ls" = param_val, "treefunc" = tree_chunk))
}


#' @title The function converting the decision tree model exported from AMUA
#'
#' @description This is a function converting the decision tree model exported from AMUA.
#'              It is required to run `parse_amua_tree()` first before running this function.
#'
#' @param params_basecase A list of parameters with the basecase values. The list should be organized
#'        as `list(par1 = 0.5, par2 = 0.2)` with the parameter names and values. For the AMUA
#'        decision tree model, this argument is the 1st output of `parse_amua_tree()`.
#' @param treefunc Text data of the AMUA decision model. This argument is the 2nd output of `parse_amua_tree()`.
#' @param popsize The population size of the decision problem. The default is 1.
#'
#' @return The function returns a `data.frame` with the strategy name as the first column and the
#'         expected effectiveness and cost are in the following columns.
#'
#' @import dplyr
#'
#' @export
dectree_convert <- function(params_basecase, treefunc, popsize = 1) {
  suppressMessages(require(dplyr))
  # wrapper for decision tree exported from AMUA
  eval(parse(text = treefunc))

  ### Display output for each strategy
  numStrategies <- length(Root$children)
  if (numStrategies < 2) stop("There should be at least 2 strategies.")

  output <- do.call(rbind, lapply(c(1:numStrategies), function(x) {
    curNode <- tree[[Root$children[x]]]
    tmp_names <- c("name", names(curNode)[which(grepl("^expected*", names(curNode)))])
    out <- unlist(lapply(tmp_names, function(x) curNode[[x]]))
    names(out) <- tmp_names
    return(out)
  }))

  output <- data.frame(output)
  output <- output %>%
    mutate_at(vars(starts_with("expected")), ~as.numeric(as.character(.)) * popsize) %>%
    rename_at(vars(contains("Cost")), funs(gsub("expected", "", .))) %>%
    rename(strategy = name)
  return(output)
}


#' @title The decision tree wrapper
#'
#' @description This is a wrapper function handling only one set of input parameters or multiple sets
#'              of input parameters.
#'
#' @param params_basecase A list of parameters with the basecase values. The list should be organized
#'        as `list(par1 = 0.5, par2 = 0.2, par3 = 0.1, par4 = 0.5)` with the parameter names and values. For the AMUA
#'        decision tree model, this argument is the 1st output of `parse_amua_tree()`.
#' @param treefunc Text data of the AMUA decision model. This argument is the 2nd output of `parse_amua_tree()`.
#' @param popsize The population size of the decision problem. The default is 1.
#' @param vary_param_samp The default is set to `NULL` if there is no varying parameters.
#'        If some parameters are varying, this argument takes a `data.frame` of different
#'        parameter values. The column names should be a subset of the parameter names in `params_basecase`.
#'
#' @return The function returns either a `data.frame` or a list of `data.frame`s. If there is no varying
#'         parameters, the function will return only one `data.frame` with all the cost and effectiveness
#'         of each strategy. If there are varying parameters (when `vary_param_samp` is not `NULL`), the
#'         function will return a list of `data.frame`s. If there are multiple outcomes of the
#'         decision tree model, the number of the elements in the list is the number of outcomes plus 1.
#'         Each outcome has a `data.frame` of outcomes calculated based on the different
#'         parameter values provided to the model for each strategy. In addition, the function will return
#'         the parameter set for the varying parameters.
#'
#' @import dplyr
#'
#' @export
dectree_wrapper <- function(params_basecase, treefunc, popsize = 1, vary_param_samp = NULL) {
  if (!is.list(params_basecase)) stop("params_basecase should be a list")
  if (is.null(names(params_basecase)) | any(is.na(names(params_basecase)))) stop("please provide parameter names to the params_basecase")
  if(!is.character(treefunc)) stop("treefunc should be text")

  if (is.null(vary_param_samp)) {
    tree_out <- dectree_convert(params_basecase = params_basecase,
                                treefunc = treefunc, popsize = popsize)
  } else {
    # Checking input arguments
    if (!is.data.frame(vary_param_samp)) stop("vary_param_samp should be in a data.frame")
    if (any(!(colnames(vary_param_samp) %in% names(params_basecase)))) {
      rm_names <- colnames(vary_param_samp)[!(colnames(vary_param_samp) %in% names(params_basecase))]
      warning(paste0(rm_names, " are not the parameters included in params_basecase. The function will ingore these parameters."))
    }

    n_samp <- nrow(vary_param_samp)
    tmp_samp <- data.frame(lapply(params_basecase, function(x) rep(x, n_samp)))
    colnames(tmp_samp) <- names(params_basecase)
    replace_col <- colnames(vary_param_samp)[colnames(vary_param_samp) %in% names(params_basecase)]
    tmp_samp[, replace_col] <- vary_param_samp[, replace_col]
    sim_out <- lapply(c(1:n_samp), function(x) {
      rep_param <- tmp_samp[x, ]
      dectree_convert(params_basecase = rep_param,
                      treefunc = treefunc,
                      popsize = popsize)
    })
    n_outcomes <- ncol(sim_out[[1]]) - 1
    outcome_name <- colnames(sim_out[[1]])[-1]
    strategy_name <- as.character(tree_output$strategy)
    tree_out <- vector(mode = "list", n_outcomes)
    names(tree_out) <- outcome_name
    for (i in c(1:n_outcomes)) {
      tmp_out <- do.call(rbind, lapply(sim_out, function(x) {
        x[, outcome_name[i]]
      }))
      tmp_out <- data.frame(tmp_out)
      colnames(tmp_out) <- strategy_name
      tree_out[[outcome_name[i]]] <- tmp_out
    }
    tree_out$param_samp <- vary_param_samp
  }
  return(tree_out)
}
