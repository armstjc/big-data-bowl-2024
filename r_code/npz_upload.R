#install.packages("reticulate")
library(reticulate)
np <- import("numpy")

npz1 <- np$load("C:/Users/Owner/Downloads/play_ex.npz")
npz1$files

npz1$f[["6"]]
npz1$f[["21"]]