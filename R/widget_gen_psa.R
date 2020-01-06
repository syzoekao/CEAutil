#'
#' @export
gen_psa_samp_code <- function(param_table, n_samp = 100) {
  param_ls <- c()
  dist_ls <- c()
  par_type_ls <- c()
  par_val_ls <- c()

  for (i in c(1 : nrow(param_table))) {
    tmp_row <- param_table[i, ]

    tmp_out <- match_param_col_val(tmp_row)

    param_ls <- c(param_ls, tmp_out$par_i)
    dist_ls <- c(dist_ls, tmp_out$dist_i)
    par_type_ls <- c(par_type_ls, tmp_out$type_i)
    par_val_ls <- c(par_val_ls, tmp_out$val_i)

    chr_param <- paste0("\"", paste(param_ls, collapse = "\", \""), "\"")
    chr_dist <- paste0("\"", paste(dist_ls, collapse = "\", \""), "\"")
    chr_par_type <- paste0("\"", paste(par_type_ls, collapse = "\", \""), "\"")
    chr_par_val <- paste(par_val_ls, collapse = ", ")
  }

  gen_code <- paste0("l_params_all <- gen_psa_samp(params = c(", chr_param,
                     "),\ndists = c(", chr_dist,
                     "),\nparameterization_types = c(", chr_par_type,
                     "),\ndists_params = list(", chr_par_val,
                     "),\nnsamp = ", n_samp, ")")
  cat(gen_code)
}

#'
#' @export
match_param_col_val <- function(row_i) {
  dist_i <- row_i$dist

  all_dist_type <- c("normal", "log-normal", "truncated-normal",
                     "beta", "gamma", "dirichlet", "bootstrap", "constant")

  if (!(dist_i %in% all_dist_type)) {
    stop("the distribution is not one of the distribution types provided in dampack")
  }

  if (dist_i == "normal") {
    match_col <- list(c("mean", "sd"))
  }

  if (dist_i == "log-normal") {
    match_col <- list(c("mean", "sd"), c("meanlog", "sdlog"))
  }

  if (dist_i == "truncated-normal") {
    match_col <- list(c("mean", "sd", "ll", "ul"))
  }

  if (dist_i == "beta") {
    match_col <- list(c("mean", "sd"), c("a", "b"))
  }

  if (dist_i == "gamma") {
    match_col <- list(c("mean", "sd"), c("shape", "scale"))
  }

  if (dist_i == "dirichlet") {
    match_col <- list(c("value", "mean_prop", "sd"), c("value", "n"), c("value", "alpha"))
  }

  if (dist_i == "bootstrap") {
    match_col <- NA
    stop("this function does not incorporate bootstrap yet")
  }

  if (dist_i == "constant") {
    match_col <- list(c("val"))
  }

  if (length(match_col) > 1) {
    extract_val <- extract_value(match_col, row_ii = row_i, dist_i = dist_i)
    keep_ix <- which(unlist(lapply(extract_val, function(x) x[[1]])))
    if (length(keep_ix) > 1) stop("more than one set of parameter definition is used")
    out_col <- match_col[[keep_ix]]
    out_val <- extract_val[[keep_ix]]$v
  } else {
    out_col <- unlist(match_col)
    out_val <- row_i[out_col]
  }

  out_col <- paste(out_col, collapse = ", ")
  out_val <- paste0("c(", paste(out_val, collapse = ", "), ")")
  par_i <- row_i$param

  return(list(par_i = par_i, dist_i = dist_i, type_i = out_col, val_i = out_val))
}

#'
#' @export
extract_value <- function(match_col, row_ii, dist_i) {
  if (dist_i != "dirichlet") {
    lapply(match_col,
           function(x) {
             v <- row_ii[x]
             keep <- any(!is.na(row_ii[x]))
             return(list(keep = keep, v = v))
           })
  } else {
    lapply(match_col,
           function(x) {
             v <- row_ii[x]
             keep <- all(!is.na(row_ii[x]))
             return(list(keep = keep, v = v))
           })
  }
}
