library(CEAutil)

treetxt <- txt_parse("/Users/szu-yukao/Documents/CEA/CEAcode/AmuaExample/DraculaParty_Export/main.R")
param_ls <- treetxt[["param_ls"]]
treefunc <- treetxt[["treefunc"]]

tree_output <- dectree_wrapper(params = param_ls, treefunc = treefunc, popsize = 200)

print(tree_output)

