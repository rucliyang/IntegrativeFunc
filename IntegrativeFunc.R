#############################################################
#####                                                   #####
#####                                                   #####
#####                    Data Generation                #####
#####                                                   #####
#####                                                   #####
#############################################################



####################### Beta Functions #######################
### Set distributions of betas in all three Cases.         ###
### There are four beta distributions in each Case.        ###

####### Case I - 20% nonzero length #######
# Named as Beta8a/Beta8b/Beta8c/Beta8d for beta1/beta21/beta22/beta23.

Beta8a <- function(t) {
# Case I, beta1.
	beta8a.t <- rep(0,length(t))
  	for(i in 1:length(t))
  	{
    	if(0.2 < t[i] && t[i] <= 0.3) {beta8a.t[i] <- 3*(1-t[i])*sin(10*pi*(t[i]+0.2))}
    	else if(0.7 < t[i] && t[i] <= 0.8) {beta8a.t[i] <- 6*t[i]*sin(10*pi*(t[i]+0.2))}
    	else{beta8a.t[i] <- 0}}
  	return(beta8a.t)
}

Beta8b <- function(t) {
# Case I, beta21.
  	beta8b.t <- rep(0,length(t))
  	for(i in 1:length(t))
  	{
    	if(0.2 < t[i] && t[i] <= 0.3) {beta8b.t[i] <- 2*(1-t[i])*sin(10*pi*(t[i]+0.2))}
    	else if(0.7 < t[i] && t[i] <= 0.8) {beta8b.t[i] <- 4*t[i]*sin(10*pi*(t[i]+0.2))}
    	else{beta8b.t[i] <- 0}}
  	return(beta8b.t)
}

Beta8c <- function(t) {
# Case I, beta22.
	beta8c.t <- rep(0,length(t))
	for(i in 1:length(t))
	{
		if(0.2 < t[i] && t[i] <= 0.3) {beta8c.t[i] <- 2 + 0.6*exp(t[i])*cos(48*pi*t[i])}
		else if(0.7 < t[i] && t[i] <= 0.8) {beta8c.t[i] <- -3.6 - 0.3*exp(t[i])*cos(48*pi*t[i])}
		else{beta8c.t[i] <- 0}}
	return(beta8c.t)
}

Beta8d <- function(t) {
# Case I, beta23.
	beta8d.t <- rep(0,length(t))
for(i in 1:length(t))
	{
	if(0.2 < t[i] && t[i] <= 0.3) {beta8d.t[i] <- 3*(1-t[i])*sin(10*pi*(t[i]+0.2))}
	else if(0.6 < t[i] && t[i] < 0.8) {beta8d.t[i] <- 15*(t[i]-0.5)*sin(10*pi*(t[i]+0.2))}
	else{beta8d.t[i] <- 0}}
	return(beta8d.t)
}



####### Case II - 10% nonzero length #######
# Named as Beta6a/Beta6b/Beta6c/Beta6d for beta1/beta21/beta22/beta23.
 
Beta6a <- function(t) {
# Case II, beta1.
	beta6a.t <- rep(0,length(t))
	for(i in 1:length(t))
	{
		if(0.195 < t[i] && t[i] <= 0.25) {beta6a.t[i] <- -3*exp(t[i])*cos(18*pi*t[i])}
		else if(0.86 < t[i] && t[i] <= 0.915) {beta6a.t[i] <- 4*exp(t[i]-0.9)*cos(18*pi*t[i])}
		else{beta6a.t[i] <- 0}}
	return(beta6a.t)
}


Beta6b <- function(t) {
# Case II, beta21.
	beta6b.t <- rep(0,length(t))
	for(i in 1:length(t))
	{
		if(0.195 < t[i] && t[i] <= 0.25) {beta6b.t[i] <- -2*exp(t[i])*cos(18*pi*t[i])}
		else if(0.86 < t[i] && t[i] <= 0.915) {beta6b.t[i] <- 3*exp(t[i]-0.9)*cos(18*pi*t[i])}
		else{beta6b.t[i] <- 0}}
	return(beta6b.t)
}


Beta6c <- function(t) {
# Case II, beta22.
	beta6c.t <- rep(0,length(t))
	for(i in 1:length(t))
	{
		if(0.195 < t[i] && t[i] <= 0.25) {beta6c.t[i] <- -2-0.3*exp(t[i])*cos(67*pi*t[i])}
		else if(0.86 < t[i] && t[i] <= 0.915) {beta6c.t[i] <- 3-0.6*cos(63*pi*t[i])}
		else{beta6c.t[i] <- 0}}
	return(beta6c.t)
}


Beta6d <- function(t) {
# Case II, beta23.
	beta6d.t <- rep(0,length(t))
	for(i in 1:length(t))
	{
		if(0.195 < t[i] && t[i] <= 0.25) {beta6d.t[i] <- -2-0.3*exp(t[i])*cos(67*pi*t[i])}
		else if(0.825 < t[i] && t[i] <= 0.92) {beta6d.t[i] <- 3-0.6*cos(63*pi*t[i])} 
		else{beta6d.t[i] <- 0}}
	return(beta6d.t)
}


####### Case I - 30% nonzero length #######
# Named as Beta3a/Beta3b/Beta3c/Beta3d for beta1/beta21/beta22/beta23.

Beta3a <- function(t) {
# Case III, beta1.
	beta3a.t <- rep(0,length(t))
	for(i in 1:length(t))
	{
		if(0.15 < t[i] && t[i] <= 0.25) {beta3a.t[i] <- -3*exp(t[i])*cos(10*pi*t[i])}
		else if(0.65 < t[i] && t[i] <= 0.85) {beta3a.t[i] <- 4*exp(t[i]-0.9)*cos(10*pi*t[i])}
		else{beta3a.t[i] <- 0}}
	return(beta3a.t)
}


Beta3b <- function(t) {
# Case III, beta21.
	beta3b.t <- rep(0,length(t))
	for(i in 1:length(t))
	{
		if(0.15 < t[i] && t[i] <= 0.25) {beta3b.t[i] <- -2*exp(t[i])*cos(10*pi*t[i])}
		else if(0.65 < t[i] && t[i] <= 0.85) {beta3b.t[i] <- 3*exp(t[i]-0.9)*cos(10*pi*t[i])}
		else{beta3b.t[i] <- 0}}
	return(beta3b.t)
}


Beta3c <- function(t) {
# Case III, beta22.
	beta3c.t <- rep(0,length(t))
	for(i in 1:length(t))
	{
		if(0.15 < t[i] && t[i] <= 0.25) {beta3c.t[i] <- -2-0.5*cos(60*pi*t[i])}
		else if(0.65 < t[i] && t[i] <= 0.75) {beta3c.t[i] <- -3+0.5*cos(60*pi*t[i])}
		else if(0.75 < t[i] && t[i] <= 0.85) {beta3c.t[i] <- 3-0.5*cos(60*pi*t[i])}
		else{beta3c.t[i] <- 0}}
	return(beta3c.t)
}


Beta3d <- function(t) {
# Case III, beta23.
	beta3d.t <- rep(0,length(t))
	for(i in 1:length(t))
	{
		if(0.15 < t[i] && t[i] <= 0.25) {beta3d.t[i] <- -2-0.5*cos(60*pi*t[i])}
		else if(0.61 < t[i] && t[i] <= 0.75) {beta3d.t[i] <- -3+0.5*cos(60*pi*t[i])}
		else if(0.75 < t[i] && t[i] <= 0.85) {beta3d.t[i] <- 3-0.5*cos(60*pi*t[i])}
    	else{beta3d.t[i] <- 0}}
	return(beta3d.t)
}

###########################################################





##################### Calculate InnerProd #####################
InnerProd <- function(Betaf,basisfd,j) {
# 	Compute the <beta_j, B_j>, integral of beta_j and B_j. 
# 	Betaf: Beta function.
# 	basis: Basis function, jth column of basismatrix (eval.basis) (t rows and nbasis columns).
	
	rng <- getbasisrange(basisfd)
	knots <- basisfd$param
	knots <- c(rng[1], knots, rng[2])
	nbasis <- basisfd$nbasis
	norder <- nbasis - length(knots) + 2
	
	a <-rng[1]
	if(j-norder > 0) {a <- knots[j-norder+1]}
	
	b <- rng[2]
	if(j <= (nbasis-norder)) {b <- knots[j+1]}
	
	BFun <- function(t) {
		basismatrix <- eval.basis(t,basisfd) # 71 by 74 matrix, t rows and nbasis column
		basismatrix.j <- t(basismatrix[,j]) #get jth column of basismatrix
	return(basismatrix.j)
	}
	
	flog <- function(t) {Betaf(t)*BFun(t)}
	in.prod <- integrate(flog,a,b) 
	return(in.prod$value)
}
############################################################




################ Generate Data (single data) ###############
DataYBeta <- function(n, rangeval, intercept1=F, intercept2=F, Beta.i, Beta.ii, sigma, estX=T){
# 	Input:
#	intercept: Intercept of the model.
# 	rangeval: Domain of t.
# 	n: Size of one dataset.
#	Beta.i/Beta.ii: Beta functions.
#	estX: "T" if derive xHat(t) from x(t).
#
# 	Output: 
# 	This function will generate a single dataset with size n. It contains: (1) intercept, original x(t), derived xHat(t), modified xHatBar(t) and xHatSD(t), sigma, signal to noise simga (sigmaSTN), various variables of response (y1, y2, yStar, y, y1Tru, y2Tru). 
# 	Note 1: The distribution of the "a Matrix" need to be specified in this function as aij.  For the use of the sample code, we set it to normal(0,1).
# 	Note 2: intercept1 and intercept2 in data generation are always set to "F" (i.e. =0), but in estimation stage, different notations (int1 and int2) are used and they could be T or F. 
# 	Note 3: y1 and y2 are n-dimensional vectors for the first and second phenotypes; yStar is a n-by-2 matrix (=cbind(y1,y2)); y is a 2n-dimensional vector (=rbind(y1,y2)); y1Tru and y2Tru are y1 and y2 without error terms, respectively.
	
	nnknots <- 100
	nnbasis <- nnknots + 3 - 2
	nnorder <- 3
	basisfdData <- create.bspline.basis(rangeval=rangeval, nbasis=nnbasis, norder=nnorder)
	
	#inner product of <beta#_j, B_j>
	G1 <- matrix(0,nrow=nnbasis, ncol=1)
	for (j in 1:nnbasis){
		G1[j,1] <- InnerProd(Beta.i,basisfdData,j)
	} 
	G2 <- matrix(0,nrow=nnbasis, ncol=1)
	for (j in 1:nnbasis){
		G2[j,1] <- InnerProd(Beta.ii,basisfdData,j)
	} 
	
	
	# Setting "aMat" aij, which is the coefficient of x(t).
	# Two kinds of aMat: normal(0,1) or uniform(-2,2).
	aMat <- matrix(rnorm(n*nnbasis,mean=0,sd=1),n,nnbasis)
	#aMat <- matrix(runif(n*nnbasis,min=-2,max=2),n,nnbasis)
	
	
	y1Tru <- intercept1 + aMat%*%G1
	y2Tru <- intercept1 + aMat%*%G2
	
	ep <- rmvnorm(n=n, mean=c(0,0), sigma=sigma)
	
	y1Sig <- sd(y1Tru)
	y2Sig <- sd(y2Tru)
	
	y1e <- ep[,1]*y1Sig/0.8
	y2e <- ep[,2]*y2Sig/0.8
	y1 <- y1Tru + y1e
	y2 <- y2Tru + y2e
	
	#Signal to noise sigma.
	sigmaSTN <- cov(cbind(y1e, y2e))
	

	yStar <- as.data.frame(cbind(y1, y2)) # n by 2 matrix
	y <- NULL
	for(i in 1:n)
	{
		y <- rbind(y,rbind(y1[i,],y2[i,]))
	}
	

	x<- fd(coef=t(aMat), basisobj=basisfdData)
	
	xHat <- NULL
	xGeno <- NULL
	# For data with SNP as predictors, derive xHat(t) from G=0,1,2.
	if (estX)
	{
		XX <- EstX(x, mode="additive", basisfdData)
		xHat <- XX$xHat
		xHatBar <- XX$xHatBar
		xHatSD <- XX$xHatSD
		xGeno <- XX$geno
	}	
		
	#return(data)	
	data <- list(x=x, xHat=xHat, xHatBar=xHatBar, xHatSD=xHatSD, xGeno=xGeno, y1=y1, y2=y2, yStar=yStar, y=y, y1Tru=y1Tru, y2Tru= y2Tru, intercept1=intercept1, intercept2=intercept2, sigma=sigma, sigmaSTN=sigmaSTN, nnbasis=nnbasis)
	
	return(data)		
}
############################################################




################### Generate Data with EstX ##################
### Derive xHat(t).                                        ###
### This function first convert x(t) to discrete G=0,1,2,  ###
### then derive xHat(t) from G.                            ###
EstX <- function(x, mode="Additive", basisfd){
# 	Note 1: In this analysis, we use the additive method only, so mode="Additive" is the default setting.
#	Note 2: xHatBar(t) and xHatSD(t) are modified xHat(t). xHat(t) is used in this analysis. xHatBar(t) and xHatSD(t) are optional variables.  The indicator variable xcoef.mod=T are applied if these two variables are used.
	nbasis <- basisfd$nbasis
	rng <- getbasisrange(basisfd)
	rng.a <- rng[1]
	rng.b <- rng[2]
	m <- 5000
	t <- seq.int(rng.a, rng.b, rng.b/(m-1)) 
	evalX <- t(eval.fd(t,x)) # n by nbasis matrix

	
	# Set evalX to 0,1,2 in "geno"
	geno<- evalX*0
	for (i in 1:nrow(evalX))
		for (j in 1:ncol(evalX))
		{
			if (evalX[i,j] <= quantile(evalX[i,], 1/3)) {geno[i,j]=0} 
			else if (evalX[i,j] > quantile(evalX[i,], 2/3)) {geno[i,j]=2}
			else {geno[i,j]=1}
		}
	
	# (Optional) Set geno to genoX if Dominant or Recessive effect is of interest.
	genoX <- evalX*0
	for (i in 1:nrow(evalX))
		for (j in 1:ncol(evalX))
		{
			if (mode == "Dom")
			{
				if (geno[i, j] == 1 || geno[i, j] == 2)
            	genoX[i,j] = 1
			}
			else if ( mode == "Rec")
			{
            	if (geno[i, j] == 2)
                genoX[i,j] = 1
            }
		}
	
	if  ( mode == "Rec" || mode == "Dom")
      geno = genoX 	
	
	
	nsample <- nrow(geno)
   	nsnp <- ncol(geno)
   
	betabasis <- basisfd
    genobasis <- basisfd
    
    Phi <- eval.basis(t, genobasis)
    
    U <- geno %*% Phi %*% ginv(t(Phi) %*% Phi)
    Ubar <- U-mean(U)
    Usd <- (U-mean(U))/sd(U)
	
	
	results <- list(xHat=fd(coef=t(U), basisobj=basisfd), xHatBar=fd(coef=t(Ubar), basisobj=basisfd), xHatSD=fd(coef=t(Usd), basisobj=basisfd), geno=geno)
	return(results)		
}

###############################################################




########################## Replicates #########################
### Generate Traing and Testing datasets with replicates.   ###
generate.datasets <- function(rangeval, nruns, n, nTest, Beta.i, Beta.ii, intercept1=F, intercept2=F, estX)
{
	simuData <- list()	
	train <- list()
	test <- list()
	
	i <- 1
	while (i <= nruns)
	{
		print(sprintf('running=%d',i))

		train[[i]] <- DataYBeta(n, rangeval, intercept1=F, intercept2=F, Beta.i, Beta.ii, sigma, estX) 
        if(nTest > 0) test[[i]] <- DataYBeta(nTest, rangeval, intercept1=F, intercept2=F, Beta.i, Beta.ii, sigma, estX) 

		i <- i + 1	
	}
		
	dat <- list(train=train, test=test)
}
###############################################################























#############################################################
#####                                                   #####
#####                                                   #####
#####                    Data Estimation                #####
#####                                                   #####
#####                                                   #####
#############################################################
##### For two continous responses and functional X(t):  #####
#############################################################



##################### L2 norm of a vector #####################
vecNorm <- function(v)
{
    vectorNorm <- sqrt(sum(v^2))
    return(vectorNorm)
}
###############################################################




################## Derivative of SCAD penalty #################
DSCAD <- function(u,lambda,a)
{
    if(u<=lambda) Dp <- lambda
    else if(u< (a*lambda)) Dp <- -(u-a*lambda)/(a-1)
    else Dp <- 0
    return(Dp)
}
###############################################################






############ Compute weight matrix for W (single) #############
### This function helps to get nonzero part of Wj,          ###
### where Wj is an (M+d) by (M+d) matrix and                ###
### w_uv is an 5 by 5 matrix with j <= u,v <= j+d           ###
### j=1,...,M no. of subintervals. i.e. for j=10            ###
### (10th subinterval), w_uv is nonezero at j=10 to 14.     ###

computeW <- function(basisfd)
{
	L <- basisfd$nbasis
    rng <- getbasisrange(basisfd)
    breaks <- c(rng[1],basisfd$params,rng[2])
    M <- length(breaks) - 1 #number of subintervals of rangeval
    norder <- L-M+1
    
    # Creates an array of size M, and each element is an norder-by-norder matrix (i.e. M list of 5x5 matrix)
	W <- array(0,dim=c(norder,norder,M))
    for (j in 1:M)
    {
    	# Calculate the inprod of two basis per subinterval
    	# i.e. jth subinterval has the range from breaks[j] to breaks[j+1].
        temp <- inprod(basisfd,basisfd,rng=c(breaks[j],breaks[j+1]))
        # W collects nonzero parts per subinterval
        W[,,j] <- temp[j:(j+norder-1),j:(j+norder-1)]
    }
    return(W)
}
###############################################################






################ Algorithm - Estimation function ##############
### This function will give the estimated beta and sigma.   ###
method.alg <- function(x, xHat, y1, y2, yStar, sigmaSTN, lambda1, lambda2, gamma, int1, int2, basisfd.beta, Jmat, Ws=NULL, max.iter, tol, a, cutoff, estX, xcoef.mod)
{
##### Input: #####
# 	x: X(t).
#	xHat: Derived xHat(t).
#	y1/y2/yStar: Response variables.
#	sigmaSTN: Signal to noise sigma.
#	lambda1, lambda2, gamma: Tuning parameters specified in the objective function.
#	int1/int2: "T" if the intercept is existed for beta 1/2. Default setting is "F".
#	basisfd.beta: Basis function.
#	max.iter: Maximum number of iteration steps.
#	tol: Tolerance of two values, this will be used to set the changeThres.
#	a: Tuning parameter used for functional SCAD penalty, i.e. in the DSCAD function.
#	cutoff: Cutoff of the estimated coefficient of beta, i.e. values smaller than cutoff will be set to zeros.
#	estX: Indicator variable. In data generation step, estX=T means to derive xHat(t) from x(t).  In estimation step, estX=T means to use the derived xHat(t) for the analysis. 
#	xcoef.mod: Optional. This variable is "T" if the modified variables xHatBar(t) and xHatSD(t) are used. Default setting is "F".  	
##### Output: #####
# 	beta1/beta2: Estimated betas.
#	fittedY/fittedY1/fittedY2: fitted values of y, y1, or y2.
#	projMat: Projection matrix.
#	sigmaHat/sigmaHatNew: estimated sigma.
 
	# Get some constantsx
	Ls <- basisfd.beta$nbasis
	Ms <- length(basisfd.beta$params) +1
	ds <- Ls - Ms
	L2NNer <- sqrt(Ms/diff(getbasisrange(x$basis))) #sqrt(M/T)
	sigmaSTN <- sigmaSTN
	yStar <- yStar
	n <- nrow(yStar)
	
	
	Vs <- NULL
	if(is.null(Vs))
    {
        Vs <- eval.penalty(basisfd.beta,int2Lfd(2))
    }
    
	Ws <- NULL
	if(is.null(Ws))
    {
        Ws <- computeW(basisfd.beta)  
    }
    
    Ws1 <- Ws
    Ws2 <- Ws

	
	# sigma^(-1/2)
	sigmaSqrtmBinv <- sqrtm(sigmaSTN)$Binv
    
    yTilde <- NULL
    for(i in 1:n)
    {
    	yTilde.i <- sigmaSqrtmBinv %*% t(yStar[i,,drop = FALSE]) #2 by 1 per ith obs
    	yTilde <- rbind(yTilde, yTilde.i)
    }
    
    
	# Compute the matrix U
    if (estX) {xcoef <- xHat$coefs }else {xcoef <- x$coefs} # M+d by n
    Uvec <- t(xcoef)%*%Jmat # n by M+d, each row is an M+d dim vector
    if (xcoef.mod) {Uvec <- Uvec + 0.001}
    
    
    if(int1 && int2)
    {
    	Uvec <- cbind(matrix(1,n,1),Uvec)
    }
    
    U <- NULL
    for(i in 1:n)
    {
    	Ui <- t(direct.sum(matrix(Uvec[i,]), matrix(Uvec[i,]))) #2 by 2(M+d)
    	U <- rbind(U, Ui)
    }
    # U is an 2n by 2(M+d) matrix
    
    

    # Compute the matrix Z
    Z <- NULL
    BB <- inprod(basisfd.beta, basisfd.beta) # M+d by M+d
    Z <- rbind(cbind(BB, -BB), cbind(-BB, BB)) # 2(M+d) by 2(M+d)
    
    
    # Compute the matrix V
    Vss <- 2*n*gamma*Vs # M+d by M+d
   	VV <- direct.sum(Vss, Vss)
   	
   	
    
    if(int1 && int2)
    {
    		BB <- bdiag(0,BB)
    		Z <- rbind(cbind(BB, -BB), cbind(-BB, BB)) # 2(M+d+1) by 2(M+d+1)
    		Vss <- bdiag(0, Vss)
    		VV <- direct.sum(Vss, Vss)
    }
    
    
    # Compute the initial estimate
    bHat <- solve(t(U)%*%U + VV, t(U)%*%yTilde)
    bStore <- bHat
    
    
	if(int1 && int2)
	{
		mu1Hat <- bHat[1]
		mu2Hat <- bHat[1+Ls]
	}
    
    
    
    # (lambda=0): bStore is the solution to beta as no penalty applied
    # (lambdda>0): Perform penalty for sparse solution
    if(lambda1>0)
    {
    		# If less than tol, set to 0.
    		changeThres <- tol
        
        est <- method.lqa(y, yStar, yTilde, bHat, sigmaSTN, int1, int2, Uvec, VV, Z, Ws, Ms, Ls, L2NNer, ds, lambda1, lambda2, a, max.iter, changeThres, estX)
       
        bHatNew <- est$bHat
        bHatStarNew <- est$bHatStar
        sigmaHat <- est$sigmaHat 

        bZero <- (abs(bHatNew) < cutoff) # length of 2*nbasis
        bHatNew[bZero] <- 0  # set those elements to 0 if TRUE
    	
    		# Else, update nonzero bHatNew by using nonzero U
    		if(int1 && int2)
    		{
    			b1Zero <- bZero[c(1:(Ls+1))]
        		b2Zero <- bZero[-c(1:(Ls+1))]
        	
        		bNonZero <- as.vector(c(TRUE,!b1Zero,TRUE,!b2Zero))
    			b1NonZero <- bNonZero[c(1:(Ls+1))]
    			b2NonZero <- bNonZero[-c(1:(Ls+1))]
    			
    			bMuHatNew <- rbind(mu1Hat,matrix(bHatStarNew[,1]), mu2Hat,matrix(bHatStarNew[,2]))
    		}else 
    		{
    			bNonZero <- !bZero
    			b1NonZero <- bNonZero[c(1:Ls)]
    			b2NonZero <- bNonZero[-c(1:Ls)]
    		}
    	
        
        ###
        Uvec1new <- Uvec[,b1NonZero,drop=F]  # n by no. of nonzero basis fn
        Uvec2new <- Uvec[,b2NonZero,drop=F]  # n by no. of nonzero basis fn
        
    	
    	
    		UW <- NULL
    		for(i in 1:n)
    		{
    			UWi <- t(direct.sum(matrix(Uvec1new[i,]), matrix(Uvec2new[i,]))) #2 by 2(M+d)
    			UW <- rbind(UW, UWi)
   		}
   		
   		yTildeNew <- NULL
    		for(i in 1:n)
    		{
    			yTildeNew.i <- sqrtm(sigmaHat)$Binv %*% t(yStar[i,, drop = FALSE]) #2 by 1 per ith obs
    			yTildeNew <- rbind(yTildeNew, yTildeNew.i)
   	 	}
    	
    	
    		###
    		Vnew <- VV[bNonZero, bNonZero, drop=F]
    		Znew <- Z[bNonZero, bNonZero, drop=F]
    		
    		bnew <- solve(t(UW) %*% UW + Vnew + 2*n*lambda2*Znew, t(UW)%*%yTildeNew)
    	
    		if(int1 && int2)
    		{
    			bTilde <- matrix(0, 2+sum(2*Ls),1)
    			bTilde[bNonZero,1] <- matrix(bnew,length(bnew),1)
    		}else
    		{
    			bTilde <- matrix(0, sum(2*Ls),1)
    			bTilde[bNonZero,1] <- matrix(bnew,length(bnew),1) 
    		}
    	}
	
    
    if(int1 && int2)
    {
    		b1Tilde <- bTilde[c(1:(Ls+1)),]
    		b2Tilde <- bTilde[-c(1:(Ls+1)),]
    	
    		mu1Est <- b1Tilde[1]
    		mu2Est <- b2Tilde[1]
    		b1Tilde <- b1Tilde[-1]
    		b2Tilde <- b2Tilde[-1]
    	
    		bTildeStar <- cbind(b1Tilde, b2Tilde)
    }else
    {
    		b1Tilde <- bTilde[c(1:Ls),]
    		b2Tilde <- bTilde[-c(1:Ls),]
    		bTildeStar <- cbind(b1Tilde, b2Tilde)
    	
    		mu1Est <- NULL
    		mu2Est <- NULL
    }
    
    bEstStar <- bTildeStar %*% sqrtm(sigmaHat)$B
    b1Est <- bEstStar[,1] # Estimated b1 used to generate estimated beta1
    b2Est <- bEstStar[,2] # Estimated b1 used to generate estimated beta2
    
    # Projection matrix
    projMat <- UW %*% solve(t(UW) %*% UW + Vnew + 2*n*lambda2*Znew, t(UW))
    
    
    fittedY <- projMat %*% yTildeNew
    fittedY1 <- NULL
    fittedY2 <- NULL
    for (i in 1:n)
    {
    	fittedY1 <- rbind(fittedY1, fittedY[(2*i-1),])
    	fittedY2 <- rbind(fittedY2, fittedY[(2*i),])
    }
 
    
    sigmaHatNew <- t(yStar - Uvec %*% bEstStar) %*% as.matrix(yStar - Uvec %*% bEstStar) / n
    

    betaRes <- list(beta1=fd(coef=b1Est, basisobj=basisfd.beta), beta2=fd(coef=b2Est, basisobj=basisfd.beta), fittedY=fittedY, fittedY1=fittedY1, fittedY2=fittedY2, projMat=projMat, mu1=mu1Est, mu2=mu2Est, sigmaHat=sigmaHat, sigmaHatNew=sigmaHatNew)
    return(betaRes)
    
}
###############################################################





################### Algorithm - LAQ Method ####################
### LQA method used within the method.alg function to       ###
### penalize beta1 and beta2.                               ###
method.lqa <- function(y, yStar, yTilde, bHat, sigmaSTN, int1, int2, Uvec, VV, Z, Ws, Ms, Ls, L2NNer, ds, lambda1, lambda2, a, max.iter, changeThres, estX)
{
	Mmax <- max(Ms)
    Lmax <- max(Ls)
    K <- length(Ms) # =1, only 1 covariate
    # P <- c(0,cumsum(Ls))
    
    n <- nrow(yStar)
	sigmaMod <- sigmaSTN
	
	beta1Normj <- matrix(0,Mmax,1) # 70 by 1
	beta2Normj <- matrix(0,Mmax,1)
    b1ZeroMat <- matrix(FALSE,1,sum(Ls)) # 1 by 74
    b2ZeroMat <- matrix(FALSE,1,sum(Ls))
	beta1Norm <- rep(Inf,1)
	beta2Norm <- rep(Inf,1)
	
	
	# Set iteration loop.
	it <- 1
	while (it <= max.iter)
	{
		### 
		# Given bHat, rewrite bHat to bHatStar
		
		if(int1 && int2)
		{
			b1Hat <- bHat[c(1:(Ls+1))]
    			b2Hat <- bHat[-c(1:(Ls+1))]
    			bMuHatStar <- cbind(b1Hat,b2Hat)
    			bMuHatStar.org <- bMuHatStar %*% sqrtm(sigmaMod)$B
    		
    			b1Hat <- b1Hat[-1]
    			b2Hat <- b2Hat[-1]
    			bHatStar <- cbind(b1Hat,b2Hat)
		}else
		{
			b1Hat <- bHat[c(1:Ls)]
    			b2Hat <- bHat[-c(1:Ls)]
    			bHatStar <- cbind(b1Hat,b2Hat)
    			bHatStar.org <- bHatStar %*% sqrtm(sigmaMod)$B
		}
		
		
		###
		# Given bHatStar, update sigmaHat.
		sigmaHat <- NULL
		if(int1 && int2)
		{
			sigmaHat <- t(yStar - Uvec %*% bMuHatStar.org) %*% as.matrix(yStar - Uvec %*% bMuHatStar.org) / n
			sigmaHatSqrtmBinv <- sqrtm(sigmaHat)$Binv
		}else
		{
			sigmaHat <- t(yStar - Uvec %*% bHatStar.org) %*% as.matrix(yStar - Uvec %*% bHatStar.org) / n
			sigmaHatSqrtmBinv <- sqrtm(sigmaHat)$Binv
		}
		
		
       	###
       	# Update yTilde 
    		# yTilde will be an 2n by 1 matrix
    		yTilde <- NULL
    		for(i in 1:n)
    		{
    			yTilde.i <- sigmaHatSqrtmBinv %*% t(yStar[i,, drop=FALSE]) #2 by 1 per ith obs
    			yTilde <- rbind(yTilde, yTilde.i)
   		} 
   		 


		# Stop criterias:
		# Change of betaNorm must less than changeThres.
		beta1NormOld <- beta1Norm
		beta2NormOld <- beta2Norm
		beta1Norm <- vecNorm(b1Hat)
		beta2Norm <- vecNorm(b2Hat)
		
		change1 <- max((beta1NormOld-beta1Norm)^2)
		change2 <- max((beta2NormOld-beta2Norm)^2)
		change3 <- max((beta1Norm-beta2Norm)^2)
    		if(change1 < changeThres && change2 < changeThres) break


		###
		# Compute W1:
		W1 <- Ws
		lqaW1 <- matrix(0,Ls,Ls) # nbasis by nbasis
		for(j in 1:Ms) # 70 equally spaced subintervals
		{
			index1 <- c(j:(j+ds))
			b1j <- b1Hat[c(j:(j+ds))]
			beta1Normj[j,1] <- sqrt(t(b1j) %*% W1[,,j] %*% b1j)
			c1j <- DSCAD(beta1Normj[j,1]*L2NNer, lambda1, a) #(8) and (10.1)
			# print(cj)
			# cj = 0 means beta is big enough and no shrinkage is applied.
			# cj != 0 means beta is small and shrinkage needs to applied.
			if(c1j !=0)
			{
				# If betaNormj is extremely small, set to zero.
				if(beta1Normj[j,1] < changeThres) b1ZeroMat[index1] <- TRUE
				# O.W. apply shrinkage, i.e. eq (12) from ref. paper.
				else lqaW1[index1,index1] <- lqaW1[index1,index1] + c1j*(L2NNer/beta1Normj[j,1])*W1[,,j] 
			}
		}
		b1ZeroVec <- b1ZeroMat
    		b1NonZeroVec <- !b1ZeroVec
    	
    		if(int1)
    		{
    			lqaW1 <- direct.sum(0,lqaW1)
    			b1NonZeroVec <- as.vector(c(TRUE,b1NonZeroVec))
    		}
    		lqaW1 <- lqaW1 / 2
    	
    	
    		# Compute W2:
    		W2 <- Ws
		lqaW2 <- matrix(0,Ls,Ls) # 74 by 74 matrix
		for(j in 1:Ms) # 70 equally spaced subintervals
		{
			index2 <- c(j:(j+ds))
			b2j <- b2Hat[c(j:(j+ds))]
			beta2Normj[j,1] <- sqrt(t(b2j) %*% W2[,,j] %*% b2j)
			c2j <- DSCAD(beta2Normj[j,1]*L2NNer, lambda1, a) #(8) and (10.1)
			# print(cj)
			# cj = 0 means beta is big enough and no shrinkage is applied.
			# cj != 0 means beta is small and shrinkage needs to applied.
			if(c2j !=0)
			{
				# If betaNormj is extremely small, set to zero.
				if(beta2Normj[j,1] < changeThres) b2ZeroMat[index2] <- TRUE
				# O.W. apply shrinkage, i.e. eq (12) from ref. paper.
				else lqaW2[index2,index2] <- lqaW2[index2,index2] + c2j*(L2NNer/beta2Normj[j,1])*W2[,,j] 
			}
		}
		b2ZeroVec <- b2ZeroMat
    		b2NonZeroVec <- !b2ZeroVec
    	
    		if(int2)
    		{
    			lqaW2 <- direct.sum(0,lqaW2)
    			b2NonZeroVec <- as.vector(c(TRUE,b2NonZeroVec))
   	 	}
    	
    		lqaW2 <- lqaW2 / 2
    		
    		bNonZeroVec <- c(b1NonZeroVec, b2NonZeroVec) # 1 by 2*nbasis vector

   		   		 
   		 
   		###
    		# Update U by using updated sigmaHat and penalized results of bNonZeroVec.  Note, penaltization on bHat1 and bHat2 can be different.
        # U will be an 2n by 2(M+d) matrix
        Uvec1new <- Uvec[,b1NonZeroVec,drop=F]   # n by nbasis
        Uvec2new <- Uvec[,b2NonZeroVec,drop=F]   # n by nbasis
    	
    		U <- NULL
    		for(i in 1:n)
    		{
    			Ui <- t(direct.sum(matrix(Uvec1new[i,]), matrix(Uvec2new[i,]))) # 2 by 2(M+d)
    			U <- rbind(U, Ui)
   		}
    	
    	
    		# Get matrix W(0)_it 
		lqaW1.it <- lqaW1[b1NonZeroVec,b1NonZeroVec,drop=F]
		lqaW2.it <- lqaW2[b2NonZeroVec,b2NonZeroVec,drop=F]
    		lqaW.it <- direct.sum(lqaW1.it, lqaW2.it)
    	
    		Vp <- VV[bNonZeroVec, bNonZeroVec, drop=F]
    		Z.it <- Z[bNonZeroVec, bNonZeroVec, drop=F]
    	
    		UtUWVZ <- t(U) %*% U + 2*n*lqaW.it + Vp + 2*n*lambda2*Z.it
		UtY <- t(U) %*% yTilde
    

		# cat('\n', "rcond =", rcond(UtUWZ))
		
		b.it <- solve(UtUWVZ, UtY)
		

		bHat <- matrix(0,length(bNonZeroVec),1)
        bHat[bNonZeroVec,1] <- matrix(b.it,length(b.it),1)
        
		# print(sigmaMod)
		sigmaMod <- sigmaHat
		it <- it + 1
		
	}
	
	# Given bHat, rewrite bHat to bHatStar
	if(int1 && int2)
	{
		b1Hat <- bHat[c(1:(Ls+1))]
    		b2Hat <- bHat[-c(1:(Ls+1))]
    		bMuHatStar <- cbind(b1Hat, b2Hat)
    		bMuHatStar.org <- bMuHatStar %*% sqrtm(sigmaHat)$B
    	
    		b1Hat <- b1Hat[-1]
    		b2Hat <- b2Hat[-1]
    		bHat <- bHat[-c(1,Ls+2)]
    	
    		bHatStar <- cbind(b1Hat, b2Hat)
    	
    		sigmaHat <- t(yStar - Uvec %*% bMuHatStar.org) %*% as.matrix(yStar - Uvec %*% bMuHatStar.org) / n
	}else
	{
		b1Hat <- bHat[c(1:Ls)]
    		b2Hat <- bHat[-c(1:Ls)]
    		bHatStar <- cbind(b1Hat,b2Hat)
    		bHatStar.org <- bHatStar %*% sqrtm(sigmaHat)$B
    	
    		sigmaHat <- t(yStar - Uvec %*% bHatStar.org) %*% as.matrix(yStar - Uvec %*% bHatStar.org) / n
	}

	
	result <- list(bHat=bHat, bHatStar=bHatStar, sigmaHat=sigmaHat)
			
}

###############################################################






########################### CV Tuning ########################
### Tuning functions used within the estimate.datasets     ###
### function. Cross-validation method is used in this      ###
### analysis. Each of the three tuning parameters, i.e.    ###
### lambda1, lambda2 and gamma, contains a list of         ###
### candidates to be tuned.                                ###
tuneCV <- function(rangeval, x, xHat, y, y1, y2, yStar, sigmaSTN, Jmat, lambda1, lambda2, gamma, int1, int2, Ws=NULL, max.iter=50, tol=1e-4, a=3.7, cutoff=1e-2, estX, xcoef.mod, LLrate)
{
	n <- length(y1)
	ss <- NULL
	for(i in 1:length(lambda1))
	{
		for(j in 1:length(lambda2))
		{
			for(k in 1: length(gamma))
			{	
				no.error <- TRUE
				tryCatch( {fit <- method.alg(x, xHat, y1, y2, yStar, sigmaSTN, lambda1[i], lambda2[j], gamma[k], int1, int2, basisfd.beta, Jmat=Jmat, Ws=NULL, max.iter, tol, a, cutoff, estX, xcoef.mod)}, error = function(e) {no.error <<- FALSE})
				if(no.error) {
				beta1 <- fit$beta1 # fn obj
				beta2 <- fit$beta2 # fn obj
				sigmaHat <- fit$sigmaHat
				sigmaHatNew <- fit$sigmaHatNew
				
				# LL
				LLb1 <- TPFP.norder5(beta1,Beta1Tru.text)
				LLb2 <- TPFP.norder5(beta2,Beta2Tru.text)
				LL.beta1 <- LLb1$Lnonezero
				LL.beta2 <- LLb2$Lnonezero
				L0.beta1 <- LLb1$L0
				L0.beta2 <- LLb2$L0
				
				# BIC and AIC
				residY <- y-fit$fittedY
		        rssY <- sum(residY^2)
		        df <- sum(diag(fit$projMat))
	
		        BIC <- n*log(rssY/(n)) + log(n)*df
		        #AIC <- n*log(rssY/(n)) + 2*df
		        
		        #sig2 <- rssY/(n-df)
		        #RIC <- (n-df)*log(sig2)+df*(log(n)-1)+4/(n-df-2)
	        
		        # CV
		        Kfold <- 10
		        idx <- sample(1:n,n)
		        s <- ceiling(n/Kfold)
		        sse.cv1 <- matrix(0,1,Kfold)
		        for(f in 1:Kfold)
	            {
	            		test.index <- idx[(s*(f-1)+1):min(s*f,n)]
	                train.index <- idx[-c((s*(f-1)+1):min(s*f,n))]

	                no.error2 <- TRUE
	                tryCatch( {fit.cv <- method.alg(x[train.index], xHat[train.index], y1[train.index,], y2[train.index,], yStar[train.index,], sigmaSTN, lambda1[i], lambda2[j], gamma[k], int1, int2, basisfd.beta, Jmat, Ws=NULL, max.iter, tol, a, cutoff, estX, xcoef.mod)}, error=function(e) {no.error2 <<- FALSE})
                
	                if(no.error2){
	                beta1.cv <- fit.cv$beta1 # fn obj
					beta2.cv <- fit.cv$beta2 # fn obj
					mu1.cv <- fit.cv$mu1
					mu2.cv <- fit.cv$mu2
					
					predY.cv <- predict.slos(beta1.cv, beta2.cv, mu1.cv, mu2.cv, xHat[test.index], int1, int2, estX)
					predY1.cv <- predY.cv$y1Hat
					predY2.cv <- predY.cv$y2Hat
					
	                y1.sse.cv <- sum((y1[test.index,] - predY1.cv)^2)
	                y2.sse.cv <- sum((y2[test.index,] - predY2.cv)^2)
	                sse.cv1[f] <- y1.sse.cv + y2.sse.cv
					
					}else{
					sse.cv1[f] <- NA	
					}
 	           }
	            cv1 <- sum(sse.cv1)	
	           
 	           }else{
	            BIC=NA
	            cv1=NA
	            LL.beta1=NA
	            LL.beta2=NA
	            L0.beta1=NA
	            L0.beta2=NA
	            }
            
	            ss.i <- cbind(lambda1[i], lambda2[j], gamma[k], BIC, cv1, LL.beta1, LL.beta2, L0.beta1, L0.beta2) 
	            ss <- rbind(ss, ss.i)
			}
		}
	}
	colnames(ss) <- c('lambda1', 'lambda2', 'gamma', 'BIC', 'cv1', 'LL.beta1', 'LL.beta2', 'L0.beta1', 'L0.beta2')
	
	
	row.has.na <- apply(ss, 1, function(x){any(is.na(x))})
	ss1 <- ss[!row.has.na,]
	
	# LLrate is an optional variable and the default setting is "F".  It is "T" if we want to control the value of the False Positive (FP) index.
	if(LLrate)
	{
		maxval <- ceiling(dim(ss1)[1]*0.5)
		LLbeta1 <- ss1[,'LL.beta1']
		LLbeta2 <- ss1[,'LL.beta2']
		# select top 20% records with lowest L0
		LLcut1 <- LLbeta1[order(LLbeta1)[maxval]] # cutpoint value of L0beta1
		LLcut2 <- LLbeta2[order(LLbeta2)[maxval]] # cutpoint value of L0beta1
		# LLsubind <- LLbeta1 <= LLcut1 | LLbeta2 <= LLcut2
		ss1LL <- ss1[(LLbeta1 <= LLcut1 | LLbeta2 <= LLcut2),]
	}else{
		ss1LL=ss1
	}
	
	if(is.matrix(ss1LL))
	{
	  if(dim(ss1LL)[1]==0){
	    minCV=NA
	    lam1 = NA
	    lam2 = NA
	    gam = NA
	  }else{
		select <- which(ss1LL[,'cv1']==min(ss1LL[,'cv1']),arr.ind=T)
		minCV=min(ss1LL[,'cv1'])
		lam1 = as.numeric(ss1LL[select,'lambda1'])
		lam2 = as.numeric(ss1LL[select,'lambda2'])
		gam = as.numeric(ss1LL[select,'gamma'])
	  }
	}else if(is.vector(ss1LL)){
		minCV=ss1LL['cv1']
		lam1 = as.numeric(ss1LL['lambda1'])
		lam2 = as.numeric(ss1LL['lambda2'])
		gam = as.numeric(ss1LL['gamma'])	
	}
	
	res <- list(minCV=minCV, lam1=lam1,lam2=lam2, gam=gam, tuneRes=ss1)
}
###############################################################



########################### Estimate datasets ###################
### This function estiamtes betas and also calculates the     ###
### corresponding evaluating indexes (TP, FP, ISE, PMSE),     ###
### over nruns (>=2) replicates.                              ###
estimate.datasets <- function(nruns, data, lambda1, lambda2, gamma, max.iter, tol, a, cutoff, int1, int2, estX, xcoef.mod, LLrate, Beta.i, Beta.ii, Beta1Tru.text, Beta2Tru.text)
{
##### Input: #####
# 	nruns: Number of replicates. Must >=2.
#	data: Simulated data generated from the "Data Generation Fn" function.
#	lambda1, lambda2, gamma: Tuning parameters specified in the objective function.
#	max.iter: Maximum number of iteration steps.
#	tol: Tolerance of two values, this will be used to set the changeThres.
#	a: Tuning parameter used for functional SCAD penalty, i.e. in the DSCAD function.
#	cutoff: Cutoff of the estimated coefficient of beta, i.e. values smaller than cutoff will be set to zeros.
#	int1/int2: "T" if the intercept is existed for beta 1/2. Default setting is "F".
#	estX: Indicator variable. In data generation step, estX=T means to derive xHat(t) from x(t).  In estimation step, estX=T means to use the derived xHat(t) for the analysis. 
#	xcoef.mod: Optional. This variable is "T" if the modified variables xHatBar(t) and xHatSD(t) are used. Default setting is "F".   
#	LLrate: Optional. This is an indicator variable in the tuneCV function. "T" if researchers want to control the False Positive (FP) rate in a reasonable range. Default setting is "F".
#	Beta.i: Function of beta 1. Set the corresponding beta function, i.e. Beta.i=Beta8a.
#	Beta.ii: Function of beta 2. Set the corresponding beta function, i.e. Beta.ii=Beta8c.
#	Beta1Tru.text: Indicator variable. Set the name of beta function for this variable, i.e. Beta1Tru.text="Beta8a" for beta function Beta.i=Beta8a.
#	Beta2Tru.text: Indicator variable. 		
##### Output: #####
# 	evalInd: list of tuning parameters (lambda1, lambda2, gamma), and results of evaluation indexes (ise, iseSD, ise0, ise1, TP, FP, pmse, pmseSD) for both functions in nruns replicates. 
#	evalInd.ave: Averaged results of evalInd over nruns replicates.
# 	beta1.evalfd.ave/beta2.evalfd.ave: Averaged quantitative beta, which was evaluated from estimated functional beta in t consecutive points, over nruns replicates.  This can be considered as a quantitative method of the averaged curve of beta function over nruns replicates.

	
	t <- seq.int(0, 1, 1/199)
	

	evalInd <- matrix(0,nruns,21)
	beta1.evalfd <- matrix(0,nruns,length(t))
	beta2.evalfd <- matrix(0,nruns,length(t))	
	for (irun in 1:nruns)
	{
		cat("A1 iteration = ", irun, "\n")
		
		### Get training data
		xHat <- data$train[[irun]]$xHat
		x <- data$train[[irun]]$x
		xGeno <- data$train[[irun]]$xGeno
		y1 <- data$train[[irun]]$y1
		y2 <- data$train[[irun]]$y2
		yStar <- data$train[[irun]]$yStar
		y <- data$train[[irun]]$y
		sigmaSTN <- data$train[[irun]]$sigmaSTN
		sigma <- data$train[[irun]]$sigma
		
		
		nb <- nknots + norder - 2
		basisfd <- create.bspline.basis(rangeval= rangeval, nbasis=nb, 	norder=norder)
		basisfd.beta <- basisfd
		Jmat <- inprod(xHat$basis, basisfd.beta)


		# Get testing dataset
		if(estX) 
		{testX <- data$test[[irun]]$xHat}else 
		{testX <- data$test[[irun]]$x}
		yStar.test <- data$test[[irun]]$yStar
		y1.test <- data$test[[irun]]$y1
		y2.test <- data$test[[irun]]$y2
		
		
		# 10-fold CV tuning
		tuned.result <- tuneCV(rangeval, x, xHat, y, y1, y2, yStar, sigmaSTN, Jmat, lambda1, lambda2, gamma, int1, int2, Ws=NULL, max.iter=50, tol=1e-4, a=3.7, cutoff=1e-2, estX, xcoef.mod, LLrate)
		lam1=tuned.result$lam1
		lam2=tuned.result$lam2
		gam=tuned.result$gam
		# tuned.param[irun,]=c(lam1,lam2,gam)
		
		if(is.na(lam1)){
		  beta1.evalfd[irun,] <- NA
		  beta2.evalfd[irun,] <- NA
		  evalInd[irun,] <- cbind(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,NA, NA, NA, NA) 
		}else{
  		tryCatch( {fit <- method.alg(x, xHat, y1, y2, yStar, sigmaSTN, lam1, lam2, gam, int1, int2, basisfd.beta, Jmat=Jmat, Ws=NULL, max.iter=50, tol=1e-4, a=3.7, cutoff=1e-2, estX, xcoef.mod)}, error=function(e) {cat("ERROR :",conditionMessage(e), "irun =",irun, "lambda1 =", lam1, ", lambda2 =", lam2, ", gamma =", gam, "\n")})
  		beta1 <- fit$beta1 # fn obj
  		beta2 <- fit$beta2 # fn obj
  		b1coefs <- beta1$coefs
  		b2coefs <- beta2$coefs
  		sigmaHat <- fit$sigmaHat
		
		
  		if(int1 && int2)
  		{
  			mu1 <- fit$mu1
  			mu2 <- fit$mu2
  		}else
  		{
  			mu1 <- NA
  			mu2 <- NA
  		}


  		### Continous results
  		# Get functions for integral
  		fDiffBeta1 <- function(t) {(eval.fd(t,beta1)-Beta.i(t))^2}
  		fDiffBeta2 <- function(t) {(eval.fd(t,beta2)-Beta.ii(t))^2}
  		Beta1Tru.sq <- function(t) {(Beta.i(t))^2}
	  	Beta2Tru.sq <- function(t) {(Beta.ii(t))^2}
	
		beta1.ise <- calcISE(Beta1Tru.text, Beta1Tru.sq, fDiffBeta1)
  		beta2.ise <- calcISE(Beta2Tru.text, Beta2Tru.sq, fDiffBeta2)
	  	# ise0 - ise in null subregion per beta
		ise0B1 <- beta1.ise$ise0
  		ise0B2 <- beta2.ise$ise0
	  	ise1B1 <- beta1.ise$ise1
		ise1B2 <- beta2.ise$ise1
  		iseB1 <- beta1.ise$ise
	  	iseB2 <- beta2.ise$ise
		iseSDB1 <- beta1.ise$iseSD
  		iseSDB2 <- beta2.ise$iseSD
			
  		# TP/FP
  		tpfp1 <- TPFP.norder5(beta1,Beta1Tru.text)
  		tpfp2 <- TPFP.norder5(beta2,Beta2Tru.text)
  		tpB1 <- tpfp1$TP
  		tpB2 <- tpfp2$TP
  		fpB1 <- tpfp1$FP
  		fpB2 <- tpfp2$FP
			
		
  		# Predict y1 pmse, y2 pmse.
  		predY <- predict.slos(beta1, beta2, mu1, mu2, testX, int1, int2, estX)
  		predY1 <- predY$y1Hat
  		predY2 <- predY$y2Hat
			
  		pmse.y1 <- mean((predY1 - y1.test)^2)
  		pmseSD.y1 <- mean((predY1 - y1.test)^2)/mean(y1.test^2)
  		pmse.y2 <- mean((predY2 - y2.test)^2)
  		pmseSD.y2 <- mean((predY2 - y2.test)^2)/mean(y2.test^2)
			
			
  		# Predict y pmse.
  		pred.yStar <- cbind(predY1, predY2)
  		pmse.vec <- NULL
  		pmseSD.vec <- NULL
  		pmseSD.d.vec <- NULL
  		for(l in 1:n)
  		{
  			pmse.l <- as.matrix(yStar.test[l,,drop=F] - pred.yStar[l,,drop=F]) %*% sigmaSTN %*% t(yStar.test[l,,drop=F] - pred.yStar[l,,drop=F])
  			pmseSD.l <- as.matrix(yStar.test[l,,drop=F] - pred.yStar[l,,drop=F]) %*% sigmaSTN %*% t(yStar.test[l,,drop=F] - pred.yStar[l,,drop=F]) / as.matrix(yStar.test[l,,drop=F]) %*% sigmaSTN %*% t(yStar.test[l,,drop=F])
				
  			pmse.vec <- cbind(pmse.vec, pmse.l)
  			pmseSD.vec <- cbind(pmseSD.vec, pmseSD.l)
				
  			pmseSD.d.l <- as.matrix(yStar.test[l,,drop=F]) %*% sigmaSTN %*% t(yStar.test[l,,drop=F])
  			pmseSD.d.vec <- cbind(pmseSD.d.vec, pmseSD.d.l)
  				
  		}
  		pmse <- mean(pmse.vec)
  		#pmseSD <- mean(pmseSD.vec)
  		pmseSD <- mean(pmse.vec)/mean(pmseSD.d.vec)
			
  		beta1.evalfd[irun,] <- eval.fd(t,beta1)
  		beta2.evalfd[irun,] <- eval.fd(t,beta2)
		
  		evalInd[irun,] <- cbind(lam1, lam2, gam, iseB1, iseSDB1, ise0B1, ise1B1, tpB1, fpB1, iseB2, iseSDB2, ise0B2, ise1B2, tpB2, fpB2, pmse.y1, pmse.y2, pmse, pmseSD.y1, pmseSD.y2, pmseSD) 
  	}
	}

	colnames(evalInd) <- c('lam1', 'lam2', 'gam', 'ise.b1', 'iseSD.b1', 'ise0.b1', 'ise1.b1', 'TP.b1', 'FP.b1', 'ise.b2', 'iseSD.b2', 'ise0.b2', 'ise1.b2', 'TP.b2', 'FP.b2', 'pmse.y1', 'pmse.y2', 'pmse', 'pmseSD.y1', 'pmseSD.y2', 'pmseSD') 
	evalInd.ave <- round(apply(evalInd[,c(4:21)],2,mean), digits=4)


	# Calculate averaged beta
	beta1.evalfd.ave <- apply(beta1.evalfd,2,mean)
	beta2.evalfd.ave <- apply(beta2.evalfd,2,mean)
	
	result <- list(evalInd=evalInd, evalInd.ave=evalInd.ave, beta1.evalfd.ave=beta1.evalfd.ave, beta2.evalfd.ave=beta2.evalfd.ave)
}
###########################################################################

























#############################################################
#####                                                   #####
#####                                                   #####
#####                   Evaluation Index                #####
#####                                                   #####
#####                                                   #####
#############################################################
### Include: ISE, PMSE, TP/FP.                          #####
#############################################################


########################## Predict Y ###########################
predict.slos <- function(beta1, beta2, mu1, mu2, testX, int1, int2, estX)
{
	x <- testX
	
	y1Hat <- 0
	y2Hat <- 0
	
	g1 <- inprod(x$basis, beta1$basis)
	g2 <- inprod(x$basis, beta2$basis)
	
	if(int1 && int2)
	{
		y1Hat <- y1Hat + mu1 + t(x$coef) %*% g1 %*% beta1$coef
		y2Hat <- y2Hat + mu2 + t(x$coef) %*% g2 %*% beta2$coef
	}else
	{
		y1Hat <- y1Hat + t(x$coef) %*% g1 %*% beta1$coef
		y2Hat <- y2Hat + t(x$coef) %*% g2 %*% beta2$coef
	}
	
	
	yhat <- list(y1Hat=y1Hat, y2Hat=y2Hat)
}
###################################################################




############################ TP / FP ##############################
### For norder=5, 8abdc/6abcd/3abcd                             ###
### Note: this function depends on true beta settings of        ###
### 8abcd/6abdc/3abcd.  If different beta functions applied,    ###
### it should be updated before use.                            ### 
TPFP.norder5 <- function(Beta,BetaTru.text)
{
# 	Coefficient of Beta basis fn can be treated as N_i,0 (i.e. nbasis=29 elements of basis function in order 0)
# 	N_i,2 (m=27 elements) is of interest, where i=0,...,m-1, we need to identify the length of l0 and l1 per subinterval (equally spaced by breaks).
	rng <- getbasisrange(Beta$basis)
	rng.a <- rng[1]
	rng.b <- rng[2]
	breaks <- c(rng[1],Beta$basis$params,rng[2]) # length 28 vector
	m <- length(breaks) - 1
	
	bcoef <- Beta$coefs
	
	ni4.nonezero <- rep(0,m)
	ni4.rng <- matrix(0,m,2)
	for (i in 1:m)
	{
		if (bcoef[i]!=0 || bcoef[i+1]!=0 || bcoef[i+2]!=0 || bcoef[i+3]!=0 || bcoef[i+4]!=0)
		{
			ni4.nonezero[i] <- TRUE
			ni4.rng[i,] <- cbind(breaks[i], breaks[i+1])	
		}else
		{
			ni4.nonezero[i] <- FALSE
			ni4.rng[i,] <- cbind(0, 0)
		}
	}
	
	
	t1l=NULL
	t1r=NULL
	t2=NULL
	t3=NULL
	if (BetaTru.text =="Beta8a" || BetaTru.text =="Beta8b" || BetaTru.text =="Beta8c")
	{
		# index1 <- ((0.2-0.3) | (0.7-0.8))
		t1l=0.2 # left limit of first signal region.
		t1r=0.3 # right limit of second signal region.
		t2=0.7 # left limit of second signal region.
		t3=0.8 # right limit of second signal region.
	}else if (BetaTru.text =="Beta8d")
	{
		# index1 <- ((0.2-0.3) | (0.6-0.8))
		t1l=0.2
		t1r=0.3
		t2=0.6
		t3=0.8
	}else if (BetaTru.text =="Beta6a" || BetaTru.text =="Beta6b" || BetaTru.text =="Beta6c")
	{
		# index1 <- ((0.195-0.25) | (0.86-0.915))
		t1l= 0.195
		t1r=0.25
		t2=0.86
		t3=0.915
	}else if (BetaTru.text =="Beta6d")
	{
		# index1 <- ((0.195-0.25) | (0.825-0.92))
		t1l= 0.195
		t1r=0.25
		t2=0.825
		t3=0.92
	}else if (BetaTru.text =="Beta3a" || BetaTru.text =="Beta3b" || BetaTru.text =="Beta3c")
	{
		# index1 <- ((0.15-0.25) | (0.65-0.85))
		t1l= 0.15
		t1r=0.25
		t2=0.65
		t3=0.85
	}else if (BetaTru.text =="Beta3d")
	{
		# index1 <- ((0.15-0.25) | (0.61-0.85))
		t1l= 0.15
		t1r=0.25
		t2=0.61
		t3=0.85
	}
	
	
	l1.subint <- rep(0,m)
	l0.subint <- rep(0,m)
	for(i in 1:m)
	{
		if ((ni4.rng[i,2] <= t1r & ni4.rng[i,1] > t1l) | (ni4.rng[i,2] <= t3 & ni4.rng[i,1] > t2))
		{
			l1.subint[i] <- ni4.rng[i,2]-ni4.rng[i,1]
			l0.subint[i] <- 0
		}else if (ni4.rng[i,1] <= t1r & ni4.rng[i,2] > t1r) 
		{
			l1.subint[i] <- t1r-ni4.rng[i,1]
			l0.subint[i] <- ni4.rng[i,2] - t1r
		}else if (ni4.rng[i,1] <= t3 & ni4.rng[i,2] > t3)
		{
			l1.subint[i] <- t3-ni4.rng[i,1]
			l0.subint[i] <- ni4.rng[i,2] - t3
		}else if (ni4.rng[i,1] <= t1l & ni4.rng[i,2] > t1l)
		{
			l1.subint[i] <- ni4.rng[i,2]-t1l
			l0.subint[i] <- t1l-ni4.rng[i,1]
		}else if (ni4.rng[i,1] <= t2 & ni4.rng[i,2] > t2)
		{
			l1.subint[i] <- ni4.rng[i,2]-t2
			l0.subint[i] <- t2-ni4.rng[i,1]
		}else
		{
			l1.subint[i] <- 0
			l0.subint[i] <- ni4.rng[i,2]-ni4.rng[i,1]
		}
	}
	
	
	# Calculate the length of nonzero and zero parts.
	if (BetaTru.text =="Beta8a" || BetaTru.text =="Beta8b" || BetaTru.text =="Beta8c")
	{
		TP <- sum(l1.subint)/0.2	
		FP <- sum(l0.subint)/0.8
	}else if (BetaTru.text =="Beta8d")
	{
		TP <- sum(l1.subint)/0.3	
		FP <- sum(l0.subint)/0.7
	}else if (BetaTru.text =="Beta6a" || BetaTru.text =="Beta6b" || BetaTru.text =="Beta6c")
	{
		TP <- sum(l1.subint)/0.11	
		FP <- sum(l0.subint)/0.89
	}else if (BetaTru.text =="Beta6d")
	{
		TP <- sum(l1.subint)/0.15	
		FP <- sum(l0.subint)/0.85
	}else if (BetaTru.text =="Beta3a" || BetaTru.text =="Beta3b" || BetaTru.text =="Beta3c")
	{
		TP <- sum(l1.subint)/0.3	
		FP <- sum(l0.subint)/0.7
	}else if (BetaTru.text =="Beta3d")
	{
		TP <- sum(l1.subint)/0.34	
		FP <- sum(l0.subint)/0.66
	}

		
	# Length of total nonezero
	Lnonezero <- sum(ni4.rng[,2]-ni4.rng[,1])	
	
	TPFP <- list(TP=TP, FP=FP, L1=sum(l1.subint), L0=sum(l0.subint), Lnonezero=Lnonezero)
}
###################################################################






############################### ISE ###############################
### Calculate ISE for 8abcd/6abdc/3abcd                         ###
### Note: this function depends on true beta settings of        ###
### 8abcd/6abdc/3abcd.  If different beta functions applied,    ###
### it should be updated before use.                            ### 
calcISE <- function(BetaTru.text, BetaTru.sq, fDiffBeta)
{
	# Calculate ise0 and ise1.
	if(BetaTru.text =="Beta8a" || BetaTru.text =="Beta8b" || BetaTru.text =="Beta8c")
	{
		ise1 <- (integrate(fDiffBeta,0.2,0.3,subdivisions=5000)$value + integrate(fDiffBeta,0.7,0.8,subdivisions=5000)$value)/0.2
		ise0 <- (integrate(fDiffBeta,0,0.2,subdivisions=5000)$value + integrate(fDiffBeta,0.3,0.7,subdivisions=5000)$value + integrate(fDiffBeta,0.8,1,subdivisions=5000)$value)/0.8
	}else if(BetaTru.text =="Beta8d")
	{
		ise1 <- (integrate(fDiffBeta,0.2,0.3,subdivisions=5000)$value + integrate(fDiffBeta,0.6,0.8,subdivisions=5000)$value)/0.3
		ise0 <- (integrate(fDiffBeta,0,0.2,subdivisions=5000)$value + integrate(fDiffBeta,0.3,0.6,subdivisions=5000)$value + integrate(fDiffBeta,0.8,1,subdivisions=5000)$value)/0.7
	}else if(BetaTru.text =="Beta6a" || BetaTru.text =="Beta6b" || BetaTru.text =="Beta6c")
	{
		ise1 <- (integrate(fDiffBeta,0.195,0.25,subdivisions=5000)$value + integrate(fDiffBeta,0.86,0.915,subdivisions=5000)$value)/0.11
		ise0 <- (integrate(fDiffBeta,0,0.195,subdivisions=5000)$value + integrate(fDiffBeta,0.25,0.86,subdivisions=5000)$value + integrate(fDiffBeta,0.915,1,subdivisions=5000)$value)/0.89
	}else if(BetaTru.text =="Beta6d")
	{
		ise1 <- (integrate(fDiffBeta,0.195,0.25,subdivisions=5000)$value + integrate(fDiffBeta,0.825,0.92,subdivisions=5000)$value)/0.15
		ise0 <- (integrate(fDiffBeta,0,0.195,subdivisions=5000)$value + integrate(fDiffBeta,0.25,0.825,subdivisions=5000)$value + integrate(fDiffBeta,0.92,1,subdivisions=5000)$value)/0.85
	}else if(BetaTru.text =="Beta3a" || BetaTru.text =="Beta3b" || BetaTru.text =="Beta3c")
	{
		ise1 <- (integrate(fDiffBeta,0.15,0.25,subdivisions=5000)$value + integrate(fDiffBeta,0.65,0.85,subdivisions=5000)$value)/0.3
		ise0 <- (integrate(fDiffBeta,0,0.15,subdivisions=5000)$value + integrate(fDiffBeta,0.25,0.65,subdivisions=5000)$value + integrate(fDiffBeta,0.85,1,subdivisions=5000)$value)/0.7
	}else if(BetaTru.text =="Beta3d")
	{
		ise1 <- (integrate(fDiffBeta,0.15,0.25,subdivisions=5000)$value + integrate(fDiffBeta,0.61,0.85,subdivisions=5000)$value)/0.34
		ise0 <- (integrate(fDiffBeta,0,0.15,subdivisions=5000)$value + integrate(fDiffBeta,0.25,0.61,subdivisions=5000)$value + integrate(fDiffBeta,0.85,1,subdivisions=5000)$value)/0.66
	}
	
	
	# Calculate ise and iseSD.
	ise <- integrate(fDiffBeta,0,1,subdivisions=5000)$value /1
	
	iseSD <- integrate(fDiffBeta,0,1,subdivisions=5000)$value / integrate(BetaTru.sq,0,1,subdivisions=5000)$value
	
	ISE <- list(ise=ise, iseSD=iseSD, ise0=ise0, ise1=ise1)
}
###################################################################





