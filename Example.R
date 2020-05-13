###############################################################
### Sample code for generating and estimating               ###
### simulated datasets.                                     ###
###############################################################
### The list of variables used in this analysis.
### Beta.i: Function of beta 1. Set the corresponding beta function, i.e. Beta.i=Beta8a.
### Beta.ii: Function of beta 2. Set the corresponding beta function, i.e. Beta.ii=Beta8c.
### Beta1Tru.text: Indicator variable. Set the name of beta function for this variable, i.e. Beta1Tru.text="Beta8a" for beta function Beta.i=Beta8a.
### Beta2Tru.text: Beta2Tru.text: Indicator variable. 
### estX: Indicator variable. In data generation step, estX=T means to derive xHat(t) from x(t). In estimation step, estX=T means to use the derived xHat(t) for the analysis.
### xcoef.mod: Optional. This variable is "T" if the modified variables xHatBar(t) and xHatSD(t) are used. Default setting is "F" because using xHat(t) is good enough.
### n: Number of sujbects in training set.
### nTest: Number of sujbects in testing set.
### rangeval: domain of X(t).
### max.iter: Maximum number of iteration steps.
### tol: Tolerance of two values, this will be used to set the changeThres.
### changeThres: Threshold of the change of values between two iterative steps.
### a: Tuning parameter used for functional SCAD penalty, i.e. in the DSCAD function.
### cutoff: Cutoff of the estimated coefficient of beta, i.e. values smaller than cutoff will be set to zeros.
### int1: "T" if the intercept of beta 1 is existed. Default setting is "F".
### int2: "T" if the intercept of beta 2 is existed. Default setting is "F".
### LLrate: Optional. This is an indicator variable in the tuneCV function. "T" if researchers want to control the False Positive (FP) rate in a reasonable range. Default setting is "F".
### nknots: Number of knots when setting up basis functions.
### norder: Number of order when setting up basis functions.


###############################################################
### Call packages and functions:
library(fda)
library(pracma)
library(mvtnorm)
library(MASS)
library(matrixcalc)

source("Functions.R")


###############################################################
### Generate simulated datasets:            

# Set values to input variables.
rangeval <- c(0,1)
sigma <- matrix(c(1,0.9,0.9,1), ncol=2)

# Generate simulated datasets under the setting of Case I, beta1=Beta8a and beta2=Beta8c, aMat=normal(0,1), sigma=matrix(c(1,0.9,0.9,1), ncol=2).
data <- generate.datasets(rangeval, nruns=3, n=150, nTest=150,  Beta8a, Beta8c, intercept1=F, intercept2=F, estX=T)
#saveRDS(data, file="sample_code_data_nruns3_cor90_norm01")


###############################################################
### Estimate the simulated dataset generated above: 

# Set values to input variables.
Beta.i=Beta8a
Beta.ii=Beta8c
Beta1Tru.text="Beta8a"
Beta2Tru.text="Beta8c"
estX=T
xcoef.mod=F
int1=F  
int2=F  
LLrate=F
n=150
max.iter=500
tol=1e-4
a=3.7
cutoff=1e-2  

nknots=70
norder=5
nbasis <- nknots + norder - 2
basisfd <- create.bspline.basis(rangeval= rangeval, nbasis=nbasis, 	norder=norder)
basisfd.beta <- basisfd

# Suggested tuning candidates. Note, candidates vary in different settings.
lambda1=c(0.14,0.16,0.18,0.2)
lambda2=c(0.4,0.5,0.6)
gamma=c(1.1e-10,1.2e-10,1.3e-10) 

# Estimate simulated dataset
# data <- readRDS(file="sample_code_data_nruns3_cor90_norm01")
results <- estimate.datasets(nruns=3, data, lambda1, lambda2, gamma, max.iter, tol, a, cutoff, int1, int2, estX, xcoef.mod, LLrate, Beta.i, Beta.ii, Beta1Tru.text, Beta2Tru.text)








