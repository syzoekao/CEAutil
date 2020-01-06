library(dampack)


dist_all <- c("normal", "log-normal", "truncated-normal",
              "beta", "gamma", "dirichlet", "bootstrap", "constant")

all_param_types <- c("mean, sd", "a, b", "shape, scale",
                     "value, mean_prop, sd", "value, n",
                     "value, alpha", "mean, sd, ll, ul", "val", "meanlog, sdlog")
all_param_types <- unique(strsplit(paste(all_param_types, collapse = ', '), ", ")[[1]])


df <- matrix(NA, ncol = length(all_param_types) + 2, nrow = 1)
df <- as.data.frame(df)
colnames(df) <- c("param", "dist", all_param_types)
df$param <- "par1"
df$dist <- "dirichlet"
df$alpha <- 0.2
df$value <- 4


gen_psa_samp_code(df, n_samp = 100)


library(dampack)
l_params_all <- gen_psa_samp(params = c("par1", "par2", "par3"),
                             dists = c("log-normal", "truncated-normal", "gamma"),
                             parameterization_types = c("mean, sd", "mean, sd, ll, ul", "shape, scale"),
                             dists_params = list(c(4, 2), c(2, 1, 0, NA), c(2, 1)),
                             nsamp = 100)





