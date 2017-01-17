# script for calibrating the leaf lv spectr PLSR models and ncertainty estimates.
#---------------- Load required libraries ---------------------------------------------------------#

rm(list=ls(all=TRUE))   # clear workspace
library(pls)
library(plotrix)
library(prospectr)
library (leaps)
library (glmnet)
library (boot)

source(paste(getwd(), 'scripts/src.R', sep=""))
source(paste(getwd(), 'scripts/functions.R', sep=""))

closeAll()

set.seed(1)
# Script options
pls.options(plsralg = "oscorespls")
Create.fig <- FALSE

# Set ouput directory
out.dir = paste(getwd(), "/Outputs/", sep="") 
in.dir = paste(getwd(), "/data/", sep="") 
aug.spectra <- imp.spectra('ASD_Chem.csv', in.dir)
X <- aug.spectra[,11:length(aug.spectra[1,])]
Y <- aug.spectra[,6:10]
Y$dry_wgt <- log(Y$dry_wg)
#diag.spectra()
X_corr = cor(as.matrix(X), Y)
sp.corr(X, Y, out.dir)

# Build full PLSR dataset: from wawelength 500 to 2048
aug.X <- data.frame(aug.spectra$Site,Y,X[,51:1050])

#--------------------------------------------------------------------------------------------------#
# Subset data into cal/val by site
eval.set <- cut.set(aug.X,out.dir)
train.data <- eval.set$train
test.data <- eval.set$test

#--------------------------------------------------------------------------------------------------#
# Run calibration PLSR analysis to select optimal number of components
pls.mod.train <- pls.cal(train.data, rep(15,5))
#--------------------------------------------------------------------------------------------------#
test.comp <- opt.comps(pls.mod.train, test.data,use.press = TRUE)
#calculate number of components given min test PRESS or RMSEP
optim.ncomps <- opt.comps(pls.mod.train, train.data)
#--------------------------------------------------------------------------------------------------#
pred.val.data <- predict.pls(pls.mod.train, test.data, optim.ncomps)
out.data <- res.out(pred.val.data, test.data)
train.val.data <- predict.pls(pls.mod.train, train.data, optim.ncomps)
train.out <- res.out(train.val.data, train.data)

#-- run a generalized linear model with all the features: clearly not a realistic option, ---------#
#-- since there are more features than observations, we are in front of the " p > n issue ---------#
#--   Hence, this model is not an option, and will not be used for the actual analyses    ---------#

full.reg<- full.asd(test.data,train.data)
#-- run lasso -------------------------------------------------------------------------------------#
lasso.reg <- lasso.asd(test.data,train.data)
#-- run step forward selection -------------------------------------------------------------------------------------#
fwd.reg <- step.fwd(test.data,train.data)
