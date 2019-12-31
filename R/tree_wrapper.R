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
txt_parse <- function(path_to_txt) {
  if (!is.character(path_to_txt)) stop("path_to_txt should be a string")


  txt <- readLines(path_to_txt)
  ix_palm <- which(grepl("^###", txt))

  ix_param <- which(txt %in% c("### Define parameters"))
  ix_param <- c(ix_param, ix_palm[which(ix_palm == ix_param) + 1])
  ix_param <- c(ix_param[1] : (ix_param[2] - 1))
  param_chunk <- txt[ix_param[-c(1, length(ix_param))]]

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
  txt[ix_param[2]] <- " for(i in c(1:length(params))) {"
  txt[ix_param[3]] <- "  assign(names(params)[i], params[[i]])"
  txt[ix_param[4]] <- " }"

  ix_class <- which(grepl("setRefClass", txt))
  ix_tmp <- which(grepl(")", txt))
  ix_tmp <- ix_tmp[min(which(ix_tmp > ix_class)) + 1]
  txt[ix_tmp] <- ", where = parent.frame())"

  tree_chunk <- txt[ix_tree]
  return(list("param_ls" = param_val, "treefunc" = tree_chunk))
}


#' @title The wrapper function of the decision tree model exported from AMUA
#'
#' @description This is the wrapper function of the decision tree movel exported from AMUA.
#'              It is required to run `txt_parse()` first before running this function.
#'
#' @param params A list of parameters with the basecase values. The list should be organized
#'        as `list(par1 = 0.5, par2 = 0.2)` with the parameter names and values. For the AMUA
#'        decision tree model, this argument is the 1st output of `txt_parse()`.
#' @param treefunc Text data of the AMUA decision model. This argument is the 2nd output of `txt_parse()`.
#' @param popsize The population size of the decision problem. The default is 1.
#'
#' @return The function returns a `data.frame` with the strategy name as the first column and the
#'         expected effectiveness and cost are in the following columns.
#'
#' @export
dectree_wrapper <- function(params, treefunc, popsize = 1) {
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
    mutate_at(vars(starts_with("expected")), ~as.numeric(as.character(.)) * popsize)
  return(output)
}

