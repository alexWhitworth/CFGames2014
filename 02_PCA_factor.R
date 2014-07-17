# 01. Load data
#-------------------------------------------------------------
setwd("C:/Users/crossfit_al1985/Documents/UCLA/_2014 summer projects/2014_CFGames_pred/R")
source("./00_functions.R")
load("./data01.Rdata")

# 02. PCA - determine number of factors
#-------------------------------------------------------------
# check correlations of events
c_men <- cor(cbind(-men_g[,12:13], men_g[, 14:18]))
c_fem <- cor(cbind(-fem_g[,12:13], fem_g[, 14:18]))

pca_m <- eigen(c_men) # 48.5% - 2; 64.3% - 3  
pca_f <- eigen(c_fem) # 62.6% - 2; 76% - 3 

# 03. factor analysis
#-------------------------------------------------------------
library(GPArotation)
# rotation options: "none", "varimax", "oblimin"
f_men <- factanal(covmat= c_men, factors= 3, rotation= "oblimin")
f_fem <- factanal(covmat= c_fem, factors= 3, rotation= "oblimin")
