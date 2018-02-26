################################################################
# "Danish Labour Supply with Human Capital -                   #
# Evidence from a Structural Life-Cycle Model"                 #
# By: Andreas Als Langholz                                     #  
# Thesis for M.sc. in Economics 2018                           #
################################################################

################################################################
# File 1: Model functions                                      #
################################################################

##############################
### Utility ##################
##############################
Util <- function(c, h, pm) {
  # Main utility function
  
  c <- c * 1/px$scale
  
  # Convert parameters
  a1 <- pm[1]
  a2 <- pm[2] 
  b  <- pm[3]
  
  # Convert to hours
  h.time = Hours(h)
  
  # Calculate utility
  u <- (c ^ a1) / a1 - b * (h.time ^ a2) / a2 
  return(u)
}

# Marginal utility of consumtion
MargUtil <- function(c, pm){
  # Marginal utility with respect to consumption
  
  # Convert from vector
  a1 <- pm[1]
  
  # Calculate marginal Utility
  u <-  c ^ (a1-1)
  return(u)
}

# Inverse Marginal Utility of consumption
InvMargUtil <- function(u, pm){
  # Inverse marginal utility for solving the Euler equation
  
  # Convert from vector
  a1 <- pm[1]
  
  # Find consumption values
  c = u ^ (1 / (a1 - 1))
  return(c)
}

# Bequest motive
Beq <- function(A, pm, px) {
  # Bequest motive for end of time period utility
  phi <- px$phi
  
  B <- ifelse(A > 0,
              3*log(A + phi) - 1 - 3 * log(phi),
              ((A + phi) / phi) ^ 3)
  
  B <- B * pm[4]
  return(B)
}

######################################
### Labour market functions ##########
######################################
# Wages
WageHourly <- function(t, k, px, p1){ 
  # Computes estimated hour wages from first step regressions
  
  Age <- t + px$t.min - 1
  
  w <-p1$w01 + 
    p1$alpha11 * k + 
    p1$alpha12 * k ^ 2 + 
    p1$alpha13 * Age + 
    p1$alpha14 * Age ^ 2 + 
    p1$alpha15 * Age ^ 3 + 
    p1$alpha16 * Age * k
  
  return(w)
  
}

# Hours discretised
Hours <- function(h){
  # Converts the indicator for the discrete choice of hours into 
  # its numerical counterpart
  
  ht <- ifelse(h == 6, 2500,
               ifelse(h == 5, 2000,
                      ifelse(h == 4, 1500,
                             ifelse(h == 3, 1000,
                                    ifelse(h == 2, 500,
                                           ifelse(h == 1, 0,0))))))
  
  return(ht)
}

# Fixed cost of work
FCW <- function(t, k, h, pm, px, p1){
  ##########################
  # subtracts fixed cost of work from income process
  # Obs! Still manually changed per education group
  
  hours <- Hours(h)
  
  fc.h <- 127
  fc.m <- 4900
  
  E <- (WageHourly(t,k,px, p1) * pmax(hours - fc.h,0) - fc.m * (h > 1)) * px$scale
  
  return(E)
}

# Unemployment Benefit 
UnemBen <- function(t, k, h, px, p1){
  # Calculates unemployment benefits
  # Input:
  # t: time
  # k: human capital
  # h: labour supply
  # w.full: wage if full time
  # w.part: wage if part
  
  # Correcting back scale and age
  Age <- t + px$t.min - 1
  
  #Predicted wages
  wfull <- WageHourly(t,k,px,p1) * ((Hours(4) + Hours(5))/2)
  wpart <- WageHourly(t,k,px,p1) * ((Hours(2) + Hours(3))/2)
  
  ubf <- p1$mu10 + 
          p1$mu11 * log(Age) + 
          p1$mu12 * Age + 
          p1$mu13 * wfull +
          p1$mu14 * wfull ^ 2
  
  ubp <- p1$mu20 + 
    p1$mu21 * log(Age) + 
    p1$mu22 * Age + 
    p1$mu23 * wpart +
    p1$mu24 * wpart ^ 2
  
  # Vectorized conditionals
  # Unemployed
  ub <- ifelse(h == 1,
               ifelse(ubf > 10^5,ubf,10^5),
               
               # Part time employed 
               
               ifelse(h %in% part.t,
                      ifelse(ubp > 10^5/2, ubp, 10^5/2),
                      0))
  
  return(ub)
}

# Capital income
CapIn <- function(A, px) {
  # Capital income function
  cap <- (A * px$R) / (1 + px$R)
  
  cap <- ifelse(cap > 0, cap, 0)
  
  return(cap)
}

# Taxation
TaxF <- function(ti, px, p1){
  # Computes taxation from taxable income and the estimated parameters
  
  tax <- p1$delta0 + 
    p1$delta1 * pmin(ti, px$kink1) +
    p1$delta2 * pmin(pmax(ti - px$kink1, 0), px$kink2 - px$kink1) + 
    p1$delta3 * pmax(ti - px$kink2, 0)
  
  tax <- pmax(tax,0) 
  
  return(tax)
}

TaxSim <- function(ti, px, p1){
  # Taxation function without top and middle tax
  
  tax <- p1$delta0 + p1$delta1 * ti
  
  tax <- pmax(tax,0) 
  
  return(tax)
}

# Deductions
Deduc <- function(A,t,k,px,p1){
  # Computes tax deductions from first step estimations 
  A1 <- A 
  WH <- WageHourly(t,k,px,p1)
  
  D <-  p1$d0 +
        p1$d1 * log(A1) +
        p1$d2 * WH + 
        p1$d3 * WH ^ 2 +
        p1$d4 * WH ^ 3
  
  D <- ifelse(ifelse(is.na(D) == TRUE,0,D) > 30000,D,30000)
  
  return(D)
}

# job offer probabilities
JProb <- function(t, k, px, p1) {
  #################################################
  # Description
  #
  # Calculates the job offer probability in next period given age and human capital
  # Arg.:
  # t: time period
  # k: Human capital 
  #################################################################################
  
  # Calculate real age level
  t <- t + px$t.min - 1
  
  # Latent linear equation for the logit specification
  pjob <- ( 
    # Age and splines
    p1$m0 + 
      p1$m11 * t +
      p1$m12 * t ^ 2 +
      p1$m13 * (t - 30) * (t > 30) +
      p1$m14 * (t - 45) * (t > 45) +
      p1$m15 * (t - 55) * (t > 55) +
      
      # Age spliens and human capital
      p1$m21 * k +
      p1$m22 * k ^ 2 +
      p1$m23 * (t - 30) * (t > 30) * k +
      p1$m24 * (t - 45) * (t > 45) * k +
      p1$m25 * (t - 55) * (t > 55) * k 
      
  )
  
  # Calculate Logit Prob.
  J <- exp(pjob) / (1 + exp(pjob))
  
  return(J)
}

################################
####### DC - EGM Functions #####
################################

# Logsum 
LogSum <- function(NextV, px){
  ##################################################
  # Calculates logsum values of 
  # choice specific value functions
  # 
  # Inputs
  # NextV: num.A X px$num.h matrix of next period choice specific values
  # Sigma: taste shock parameter
  #
  # Output
  # logsum: logsum values vector of expected values
  ##################################################
  
  sigma <- px$sigma.taste
  
  logsum <- rep(0, nrow(NextV))
  
  # Loop over choices
  for (d in 1:px$num.h){
    
    logsum <- logsum + exp(NextV[,d])
    
  }
  
  # Take logsum
  logsum <- sigma * log(logsum)
  
  return(logsum)
}

# Choice probabilities
ChoiceProb <- function(NextV, px){
  ####################################################
  # Calculates choice probabilities for next period
  # of choice specific value functions
  # 
  # Inputs
  # NextV: num.A X px$num.h of next period choice specific values
  # Sigma: taste shock parameter
  #
  # Output
  # Choice prob: num.grid X choice prob matrix
  ####################################################
  
  sigma <- px$sigma.taste
  
  cp <- array(0, dim = dim(NextV))
  
  expsum <- rowSums(exp(NextV))
  
  # Loop over choices
  for (d in 1:px$num.h){
    
    cp[,d] <- exp(NextV[,d]) / expsum
    
  }  
  
  return(cp)
}

# Robust Logsum/CP functions
LogCP <-  function(NextV,px){
  ##################################################
  # Calculates logsum values and choice probabilities
  # using a recentered version of the closed form solutions
  # to account for numerical instability
  # 
  # Inputs
  # NextV: num.A X px$num.h matrix of next period choice specific values
  # Sigma: taste shock parameter
  #
  # Output
  # logsum: logsum values vector of expected values
  ##################################################
  
  sigma <- px$sigma.taste
  
  Vmax <- matrix(rowMax(NextV))
  
  #Logsum
  emax <- NextV - matrix(rep(Vmax,px$num.h), nrow= nrow(NextV), ncol = ncol(NextV))
  logsum <- Vmax + sigma * log(rowSums(exp(emax / sigma)))
  
  # Choiceprob
  logdif <- NextV - matrix(rep(logsum,px$num.h), nrow= nrow(NextV), ncol = ncol(NextV))
  cp <- exp(logdif / sigma)
  
  out <- list("Logsum" = logsum, "Choiceprob" = cp)
  return(out)
  
}

# Unequal grids
UnEqualGrid <- function(px){
  # Constructs grid with more bottom mass for better 
  # approximation of curvature in the utility function
  
  lo  <- px$a.min
  hi  <- px$a.max
  n   <- px$num.a 
  phi <- px$uneq
  
  w <- rep(0,n)
  w[1] <- lo
  for (i in 2:n){
    w[i] <- w[i-1] + (hi-w[i-1])/((n-i+1)^phi)
  }
  
  
  return(w)
}
UEGridM <- function(px){
  # For use in common grid upper envelope, to avoid EDG over max levels of posible interp values
  lo  <- px$a.min
  hi  <- px$a.max * 1.5
  n   <- px$num.a
  phi <- px$uneq
  
  w <- rep(0,n)
  w[1] <- lo
  for (i in 2:n){
    w[i] <- w[i-1] + (hi-w[i-1])/((n-i+1)^phi)
  }
  
  
  
  return(w)
}
UEplot <- function(lo,hi,n,phi){
  # Unequal grid for use in discretizing consumption and assets in graphs
  
  w <- rep(0,n)
  w[1] <- lo
  for (i in 2:n){
    w[i] <- w[i-1] + (hi-w[i-1])/((n-i+1)^phi)
  }
  
  
  return(w)
}

# Upper Envelope
UpperEnvelope <- function(C,V,M,px){
  ##### Description ###################################################################
  #
  # Calculates the upper envelope of the solution
  # converting the solution from a multivalued correspondance to a one-to-one function
  # between endogenous grid resources and C/V values
  # 
  # Input:
  # M: Endogenous grid of resources
  # C: consumyption correspondance to M at choice h_t
  # V: Values corresponding to Consupx$mypion C, choice h and resources M
  # All are found for a given constellation of S={FTY,PTY,i.p,i.k}
  # 
  # Output
  # uM: refined upper envelope values of EDG
  # uC: refined upper envelope values of consumption
  # uV: refined upper envelope values of value function
  # 
  # Credit: All code is translated directly from the solution given in
  # Ishakov et. al. 2017, and the related Matlab code by 
  # Thomas Jørgensen on tjeconomics.dk
  #####################################################################################
  
  Fall <- 0 
  Increase <- 1
  i <- 2
  while(i <= px$num.a -1) {
    if(M[i+1] < M[i] & M[i] > M[i-1] | V[i] < V[i - 1] & M[i] > M[i-1]) { # Detect the non-concave regions
      Fall <- ifelse(Fall == 0, i, c(Fall, i))
      k <-  i
      while(M[k+1] < M[k]){
        k <- k + 1
      }
      Increase <- c(Increase, k)
      i <- k
    }
    i <- i + 1
  }
  Fall <- c(Fall, px$num.a)
  n.kinks <- length(Fall)
  NumP <- px$num.a
  CommonM <- UEGridM(px)
  CommonV <- (-1 * 10 ^ 7) * ones(NumP, n.kinks)
  CommonC <- NaN * ones(NumP, n.kinks)
  
  for(j in 1:n.kinks){
    InRange <- which(M[Increase[j]] > CommonM & M[Fall[j]] < CommonM)
    CommonV[InRange,j] <- approxExtrap(M[Increase[j]:Fall[j]],
                                       V[Increase[j]:Fall[j]], 
                                       xout=CommonM[InRange])$y
    
    CommonC[InRange,j] <- approxExtrap(M[Increase[j]:Fall[j]],
                                       C[Increase[j]:Fall[j]], 
                                       xout=CommonM[InRange])$y 
    
  } 
  uVt <- apply(CommonV,1,FUN = max)
  id  <- which(CommonV == uVt)
  uM  <- CommonM
  if(is.na(uVt[1])){
    uVt[1] <- 0
    CommonC[1,1] <- uM[1]
  }
  # Extrapolate if sub-segs goes outside common grid yileding NA
  isNA          <- which(is.na(uVt) == TRUE)
  uVt[isNA]     <- approxExtrap(uM[-isNA], uVt[-isNA], uM[isNA])$y
  LastBeforeNA  <- min(which(is.na(uVt) == TRUE)) - 1
  LastId        <- id[LastBeforeNA]
  id[isNA]      <- LastId
  
  # Consumption by linear indexation
  LinInd <- cumsum(ones(px$num.a,1))+(id - 1)*px$num.a
  uC <- CommonC[LinInd]
  uC[isNA] <- approxExtrap(uM[-isNA], uC[-isNA], uM[isNA])$y
  
  out <- list("C" = uC, "M" = uM, "V" = uVt)
  return(out)
}

# Solution wrapper for parallel computing
SolutionWrapper <- function(Avec,i.h,t,n.r,n.c,n.v, k.markov, pm, px, p1, Sim){
  #########################################################################################
  # Description 
  #
  # Solves optimal consumption and value for a given set of state variables
  # and a discrete choice h
  # The wrapper is created to assure "Embarrassingly Parallel" runs of the EGM solution 
  # on maximum number of statespace points on a single CPU core
  #
  # #Input:
  # Grids:
  # n.r:    (M) Next period wealth/cash on hand to use for interpolation (grid)
  # n.c:    c(M,h)_(t+1) Next period optimal consupx$mypion given next wealth (M) and choice h (C[t,,,,])
  # n.v:    v(M,h)_(t+1) next period value given M and h
  # A:      End of period Asset grid (Skal muligvis være et punkt)
  #
  # Scalars:
  # t:        Current time period 
  # h:        choice of labour supply
  #
  # # Output:
  # M:        Endogenous grid of starting resources    
  # C(M,h)_t: Consupx$mypion for M and h at t and State space k X YU
  # V(M,h)_t: Value of for M and h at t and State space k X YU
  ##########################################################################################
  
  W.C <- array(NA, dim = c(px$num.k,  # Human capital
                           px$num.a)) # Asset grid))
  
  W.V <- W.C
  W.M <- W.C 
  
  for (i.k in 1:px$num.k){ 
    
    if(t == px$T) {
      # Bequest solution
      W.M[i.k, ] <- Avec
      W.C[i.k, ] <- Avec
      
      # Wealth with pensions
      Pen <- Pens(i.k,px,p1)
      W.V[i.k,] <- Beq(Avec + Pen, pm, px)
    }                                        
    
    if(t < px$T){
      
      #### Solve model by EGM ####
      # Each solution to a given choice of h runs parallel and is returned as combined list
      sol.w <- EGM(A         = Avec,
                   n.r    = n.r,
                   n.c    = n.c,
                   n.v    = n.v,
                   h      = i.h,
                   k      = i.k,
                   t      = t,
                   k.markov = k.markov,
                   px = px,
                   pm = pm,
                   p1 = p1, 
                   Sim = Sim)
      
      # Store solution values
      W.M[i.k, ] <- sol.w$M
      W.C[i.k, ] <- sol.w$C
      W.V[i.k, ] <- sol.w$V
    }
  }
  
  SolutionWL <- list(W.M, W.C, W.V)
  return(SolutionWL)
}

# EGM Step 
EGM <- function(A, t, n.r, n.c, n.v, h, k, k.markov, pm, px, p1, Sim){
  #########################################################################################
  # Description 
  #
  # Solves optimal consumptypion and value for a given set of state variables
  # and a discrete choice h
  #
  # # Input:
  # Grids:
  # n.r:      (M) Next period wealth/cash on hand to use for interpolation (grid)
  # n.c:      c(M,h)_(t+1) Next period optimal consumption given next wealth (M) and choice h (C[t,,,,])
  # n.v:      v(M,h)_(t+1) next period value given M and h
  # A:        End of period Asset grid
  # K.Markov: List of markov transition matrices for different labour supply choices
  #
  # Scalars:
  # t: Current time period (t<T)
  # k: Level of human capital in t
  # h: Labour supply
  #
  # Parameters:
  # pm: Model parameters to be estimated
  # px: Model parameters exogenously given
  # p1: Model parameters from first stage
  #    
  # Sim: Controls for simulation specifications
  #
  # # Output:
  # M:        Endogenous grid of starting resources    
  # C(M,h)_t: Consupx$mypion for M and h 
  # C(M,h)_t: Value of for M and h
  ########################################################################################
  
  ### Storage Matrices ####
  # Solutions
  C <- rep(NA, length(A))
  M <- C

  ### Income ##
  # Wage income
  WI <- WageHourly(t, k, px, p1) * Hours(h)
  
  # Capital income
  CI <- CapIn(A, px) #
  
  # Unemployment benefits
  UB  <- UnemBen(t, k, h, px, p1)
  UB  <- ifelse(Sim$Sim==TRUE & Sim$NoUB == TRUE,0,UB) # No UB if Basic income simulation is run
  
  # Deductions
  DE <- Deduc(A, t, k, px, p1)
  
  # Taxable income
  TI <- pmax(CI + WI + UB - DE,0)
  
  # Taxation baseline and simulations
  if(Sim$Sim == TRUE & Sim$Regime == TRUE){
    
    if(Sim$S1 == "TaxEL" | Sim$S2 == "TaxEL"){
      
      Tax <- TaxF(TI,px,p1) + Sim$margtax * (UB + WageHourly(t, k, px, p1) * Hours(h))
      
    } else if(Sim$S1 == "TaxT" | Sim$S2 == "TaxT") {
      
      Tax <- TaxSim(TI,px,p1)
      
    } else {
      
      Tax <- TaxF(TI,px,p1)
    }
  } else {
    
    Tax <- TaxF(TI,px,p1)
    
  }

  # Subtracting fixed cost of work
  Inc <- FCW(t,k,h,pm,px,p1)
  
  # Basic income 
  if(Sim$Sim == TRUE & Sim$Regime == TRUE & Sim$S1 == "BI"){
    
    BI <- Sim$BI
    
  } else {
    
    BI <- 0
    
  }
  
  # Compensation for Marshall elasticities
  
  CO <- ifelse(Sim$Sim==TRUE & Sim$HI.comp == TRUE,Sim$comp,0)
  
  ## Next period wealth
  NextW <-(1 + px$R) * A + Inc + UB - Tax + BI + CO
  
  ### Look up transition probabilities in markov chain given h ###
  k.markov    <- K.Markov
  trans.prob  <- k.markov[[h]][k,]
  nonzero.k   <- which(k.markov[[h]][k,] != 0)

  # Storage
  RHS <- 0
  EV  <- 0
  
  # Next period values for h decision
  NextC  <- array(NA, dim = c(length(A), px$num.h)) 
  NextV  <- NextC
  
  # Loop over K's in human capital
  for(n.k in nonzero.k) {
    
    #### For each h in t+1, compute value and cons #####
    for(d in 1:px$num.h){
      NextC[,d]  <- approxExtrap(x  = n.r[d, n.k, ],  # Next period endogenous resources (EG_t+1)
                                 y  = n.c[d, n.k, ],        # Net period consupx$mypion given EG_t+1
                                 xout = NextW)$y                            
      
      NextV[,d]  <- approxExtrap(x  = n.r[d, n.k, ],
                                 y  = n.v[d, n.k, ], 
                                 xout = NextW)$y
    }
    
    
    # Calculate choice probabilities conditional on timeperiod
    # If next period is terminal, h = 1 is certain
    LCP <- LogCP(NextV,px)
    
    if(t < (px$T-1)){
      
      choiceprob  <- LCP$Choiceprob
      
    } else {
      
      choiceprob <- matrix(c(rep(1,px$num.a),zeros(n = px$num.a, m = px$num.h-1)), nrow = px$num.a, ncol = px$num.h)
      
    }
    
    # Expected marginal utilities given choice probabilities
    EMUWork <- rep(0, length(A))
    
    # Expected marginal utility when agent have joboffer next period
    for (d in 1:px$num.h){
      EMUWork <- EMUWork + choiceprob[,d] *  MargUtil(NextC[,d],pm)
    } 
    
    # Job probability of k 
    Jprob <- JProb(t+1, n.k, px, p1)
    
    # Expected marginal utility when agent do not have job offer next period
    EMUunempl <- MargUtil(NextC[,1],pm)
    
    # Expected consumtion by job probability
    ExpMargUtil <- Jprob * EMUWork + (1 - Jprob) * EMUunempl
    
    ## Collect terms and multiply with transition probs
    # Right hand side of euler equation
    RHS <- RHS + (px$beta * (1 + px$R ) * ExpMargUtil * trans.prob[n.k])
    
    # Expected value as logsum weigthted by job probability
    EV <- EV + (LCP$Logsum * trans.prob[n.k])
  }
  
  # EGM solution to consupx$mypion C, Start of period assets M and Value V
  C <- InvMargUtil(RHS, pm)
  M <- A + C
  V <- Util(C, h, pm) + px$beta * EV
  
  # Add the interpolated constrained solution
  Ms <- min(M)
  As <- min(A)
  subGrid <- seq(from=As, to = Ms-10^-6, length.out = px$cons)
  Cs <- subGrid
  Vs <- Util(Cs,h,pm) +px$beta * EV[1]
  
  # Add points
  M <- c(subGrid,M)
  C <- c(Cs,C)
  V <- c(Vs,V)
  
  # Remove points ro preserve dimensionality
  ind.out <- round(seq(from = px$cons, to = px$num.a, length.out = px$cons))
  C <- C[-ind.out]
  M <- M[-ind.out]
  V <- V[-ind.out]
  
  # Detect non-monotonicity in the endogenous grid and run upper envelope refinement if found
  # if(sum(diff(M)<0)>0){
  #   UE <- UpperEnvelope(C,V,M,px)
  #   C <- UE$C
  #   M <- UE$M
  #   V <- UE$V
  # }
  # 
  # Combine in dataframe and return
  out <- data.frame("C" = C, "V" = V, "M" = M)
  return(out)
}

# Full solution function
SolveEGM <- function(pm, px, p1, Sim){
  #########################################################################################
  # Description 
  #
  # The full solver for all statespace choice and timepoints at a given set of parameters
  # Required solution function to maximum likelihood estimator  
  #
  # Input:
  # pm: Model parameters to be estimated
  # px: Model parameters exogenously given
  # p1: Model parameters from first stage
  #    
  # Sim: Controls for simulation specifications
  #
  # Returns:
  # M_s,t:            Endogenous grid of starting resources for all states and times S X T   
  # C(M,h,S)_t: Consupx$mypion for M for all states, times and choices S X T X H
  # V(M,h,s)_t: Value of for M for all states, times and choices S X T X H
  #########################################################################################
  
  Avec <- UnEqualGrid(px) * px$scale
  
  Solution.C <- array(NA, dim = c(px$T,       # Time
                                  px$num.h,   # h choices
                                  px$num.k,   # Human cap
                                  px$num.a))  # Avec))
  
  Solution.V <- Solution.C
  Solution.M <- Solution.C 
  
  #print("Start Solution")
  for (t in px$T:1){

    # Time tracker
    
    if(t < px$T) {
      n.r <- Solution.M[t+1, , , ]
      n.c <- Solution.C[t+1, , , ]
      n.v <- Solution.V[t+1, , , ]
    } 
    
    for(i.h in 1:px$num.h){
    # Initialise parrallelization
     for (i.k in 1:px$num.k){ 
      
        if(t == px$T) {
          # Bequest solution
          Solution.M[t, i.h ,i.k, ] <- Avec
          Solution.C[t, i.h ,i.k, ] <- Avec
          Solution.V[t, i.h ,i.k, ] <- Beq(Avec,pm,px)
          
        }                                        
      
        if(t < px$T){
        
        #### Solve model by EGM ####
        # Each solution to a given choice of h runs parallel and is returned as combined list
        SolEGM <- EGM(A      = Avec,
                      t      = t,
                      n.r    = n.r,
                      n.c    = n.c,
                      n.v    = n.v,
                      h      = i.h,
                      k      = i.k,
                      k.markov = K.Markov,
                      px = px,
                      pm = pm,
                      p1 = p1, 
                      Sim = Sim)
        
        # Store solution values
        Solution.M[t, i.h ,i.k, ] <- SolEGM$M
        Solution.C[t, i.h ,i.k, ] <- SolEGM$C
        Solution.V[t, i.h ,i.k, ] <- SolEGM$V
      }
    }
  }
   # if(t %in% c(12,22,32,41))   print(paste0("Solution time: ", px$T - t +1, " of ", px$T))
   # if(t == 1) print("Solution - Done!")

    }
  
  out <- list("C" = Solution.C, "M" = Solution.M, "V" = Solution.V)
  return(out)
}

# Speed Test of Full solution function
# time <- system.time(SolveEGM(pm,px,p1,Sim))
# print(paste0("System time: ",round(time[[3]],2), " sec. / ", round(time[[3]]/60,2), " min. per solution"))

#####################################
### Maximum Likelihood ##############
#####################################

# Wrapper function for parallel computing
InterpWrapperLogLike <- function(i.h,df.est,Solution.C, Solution.M, Solution.V, px){
  ##################################################################################
  # Description
  #
  # Wrapper function for parallel interpolation of predicted values and consumption
  # form df.est state variables
  # 
  # Input:
  # i.h: choice index
  # df.est: df.estset from registerdf.est, N X 12 vars
  # Solution.C/V: Model soluion to consumption and values
  # Solution.M: Model endogenous grid to interpolate on
  #
  # Output:
  # IntC: Interpolated consumption predicted from model solution
  # IntV: Interpolated values predicted from model solution
  ###################################################################################
  
  N <- nrow(df.est)
  IntV <- array(NA, dim = c(N,px$T, px$num.k))
  IntC <- IntV
  
  for(t in 1:px$T){
    for(i.k in 1:px$num.k){
      
      IntC[, t,i.k] <- approxExtrap(Solution.M[t,i.h, i.k, ],
                                    Solution.C[t,i.h, i.k, ],
                                    df.est$wealth)$y
      
      IntV[, t,i.k] <- approxExtrap(Solution.M[t,i.h, i.k, ],
                                    Solution.V[t,i.h, i.k, ],
                                    df.est$wealth)$y
    }
  }
  
  out <- list("IntC" = IntC, "IntV" = IntV)
  return(out)
  
}

# LogLikelihood function
LogLikelihood <-function(df.est,Solution.C, Solution.M, Solution.V, px) {
  ##################################################################################
  # Description
  #
  # Loglikelihood calculation for estimating model parameters
  # 
  # Input:
  # df.est: df.estset from registerdf.est, N X 12 vars
  # Solution.C/V: Model soluion to consumption and values
  # Solution.M: Model endogenous grid to interpolate on
  #
  # Output:
  # llike: Sum of loglikelihood contributions
  # ProbH: Probability of the made choice given model solution
  ###################################################################################
  
  
  # Number of obs
  N <- nrow(df.est)
  N
  # Storage for interpolated values on choice and statse
  AltV  <- array(NA, dim = c(N, px$T, px$num.h ,px$num.k))
  AltC <- AltV
  
  # Do a parrallel loop over h from wrapper
  InterpCV <- lapply(1:px$num.h,FUN =
                       function(x){InterpWrapperLogLike(i.h=x,
                                                        df.est,
                                                        Solution.C, 
                                                        Solution.M,
                                                        Solution.V,px)})
  
  # Collect term
  for(i.h in 1:px$num.h){
    
    AltC[ , ,i.h, ] <- InterpCV[[i.h]]$IntC
    AltV[ , ,i.h, ] <- InterpCV[[i.h]]$IntV
    
  }
  
  PredC <- matrix(nrow = N)
  PredVh <- matrix(nrow = N,ncol = px$num.h)
  
  # Look up values for each observation
  
  Obs <- 1:N
  
  PredC <- AltC[cbind(Obs,df.est$t,df.est$ht,df.est$k)]
  
  for(i.h in 1:px$num.h){
    
    PredVh[,i.h] <- AltV[cbind(Obs,df.est$t,i.h,df.est$k)]
  }
  
  # Model Value
  value <- PredVh[cbind(Obs, df.est$ht)]
  
  # Calculate logsum
  logsum <- LogCP(PredVh, px)$Logsum
  
  # Calculate Choice probs for use in model fit illustrations
  #expsum <- rowSums(exp(PredVh / px$sigma.taste))
  #probH <- (exp(value / px$sigma.taste)) / expsum
  
  # Choicepart of the likelihood
  choicepart <- 1 / px$sigma.taste * (value - logsum)
  
  # Remove non-jobreceivers from choicepart
  choicepart <- ifelse(df.est$joboffer == 1, choicepart, 0)
  
  # Consumption part of likelihood
  conspart <- - 1 / 2 * log((df.est$cons - PredC) ^ 2)
  
  # Individual likelihoods
  IndLL <- choicepart + conspart
  
  # Total likelihood
  LogLike <- sum(choicepart + conspart)
  
  # Return output
  out <- list("SumLL" = LogLike, "IndLL" = IndLL)
  
  return(out)
}

# Combined Solution and Likelihood function
FullLogLike <- function(df.est,pm,px,p1){
  ##################################################################################
  # Description
  #
  # Combined solution and loglikelihood function for a given df.est and parameterset
  # 
  # Input:
  # df.est: data.set for estimation,
  # pm: Model parameters to be estimated
  # px: Model parameters exogenously given
  # p1: Model parameters from first stage
  #
  # Output:
  # Sumll: Sum of loglikelihood contributions
  # Indll: Individual log likelihoods for observations used in BHHH
  ###################################################################################
  
  Solution <- SolveEGM(pm,px,p1,Sim,sim.args)
  
  llike <- LogLikelihood(df.est,
                         Solution.C = Solution$C,
                         Solution.M = Solution$M,
                         Solution.V = Solution$V, px)
  
  print("End Likelihood")
  return(llike)
}

# Handles for optimizer
# Sum of LogLikelihoods
LLSum <- function(pm) {
  # LogLike handle to feed to maximizer
  # Returns LL sum
  ll <- FullLogLike(df.est,pm,px,p1)
  ll <- ll$SumLL
  return(ll)
}

# Individual observations of LL for BHHH algorithm
LLObs <- function(pm) {
  # LogLike handle to feed to maximizer
  # Return individual LL contributions for all observations
  ll <- FullLogLike(df.est,pm,px,p1)
  ll <- ll$IndLL
  return(ll)
}

# Speed Test
time <- system.time(FullLogLike(df.est, pm,px,p1))
print(paste0("MLE evaluation time: ",round(time[[3]],2), " sec. / ", round(time[[3]]/60,2), " min."))

#####################################
### Simulations #####################
#####################################
Simulation <- function(Solution.bl, Solution.sim, pm, px, p1, k.markov, A.t1, H.t1, Sim){
  #########################################################################################
  # Description 
  #
  # Simulated Sim$num.sim agents for a full lifecycle
  #
  # # Input:
  # Solution.bl:   Model baseline solution without simulation elements in EGM
  # Solution.sim:  Model Simulation solution with simulation elements in EGM
  # k.markov:      Markov Transition Matrices
  #
  # Parameters:
  # pm: Model parameters to be estimated
  # px: Model parameters exogenously given
  # p1: Model parameters from first stage
  #    
  # Sim: Controls for simulation specifications
  #
  # Output:
  # Sim.out, data.frame with paneldataset of all individual specific income, tax etc.
  # for the full life-cycle
  # ######################################################################################
  
  Sim.M <- matrix(NA, nrow = px$num.sim, ncol = px$T-1)
  Sim.C <- Sim.M
  Sim.A <- Sim.M
  Sim.H <- Sim.M
  Sim.K <- Sim.M
  Sim.Age <- Sim.M
  Sim.JO <- Sim.M
  Sim.T <- Sim.M
  Sim.Tax <- Sim.M
  Sim.TI <- Sim.M
  Sim.Tau <- Sim.M
  Sim.WH <- Sim.M
  Sim.IN <- Sim.M
  Sim.UB <- Sim.M

  if(Sim$Sim == TRUE & Sim$Trans == FALSE){
    print("Use Simulation solution")
    Solution.sim.C <- Solution.sim[[1]] 
    Solution.sim.M <- Solution.sim[[2]]
    Solution.sim.V <- Solution.sim[[3]]
  }

  if(Sim$Sim == FALSE |
     (Sim$Sim == TRUE & Sim$Trans == TRUE )|
     (Sim$Sim == TRUE & Sim$Int == TRUE )){
    
    print("Use Baseline solution")
    Solution.bl.C <- Solution.bl[[1]] 
    Solution.bl.M <- Solution.bl[[2]]
    Solution.bl.V <- Solution.bl[[3]]
  }

  # Loop over time periods
  # print("Start simulation")
  
  for(t in 1:(px$T-1)){
    # print(t)
    if(t == 1){
      
      # States at beginning of simulation
      # Use starting distribution to simulate initial work choices
      h.tm1  <- H.t1
      A.tm1  <- A.t1
      k.tm1  <- rep(1,px$num.sim)
      
    } else {
      # States in beginning of period t.min + 1
        
      h.tm1  <- Sim.H[, t-1]
      A.tm1  <- Sim.A[, t-1]
      k.tm1  <- Sim.K[, t-1]
      ul.tm1 <- ifelse(h.tm1 == 1, 1, 0)
    
      }
    
    if(Sim$Sim == FALSE | 
       (Sim$Sim == TRUE & Sim$Trans == TRUE) |
       (Sim$Sim == TRUE & Sim$Int == TRUE & t < Sim$shockt))  {
      
      if(Sim$Sim == TRUE & Sim$Int == TRUE & t == (Sim$shockt-1)) {print("Baselinesol. before shock")}
      
      Solution.C <- Solution.bl.C  
      Solution.M <- Solution.bl.M 
      Solution.V <- Solution.bl.V 
      
    } else if((Sim$Sim == TRUE & Sim$Regime == TRUE) | 
              (Sim$Sim == TRUE & Sim$Int == TRUE & Sim$shockt >= t)){
      if(Sim$Sim == TRUE & Sim$Int == TRUE & t == Sim$shockt) {print("Sim solution at shock")}
      Solution.C <- Solution.sim.C 
      Solution.M <- Solution.sim.M 
      Solution.V <- Solution.sim.V 
    }
    
    #### Choice dependent state transitions ####
    ### Human capital movements
    K <- K.Markov[h.tm1]                                  # Lookup choice dependent human capital matrices for each choice and make matrix list
    
    K <- t(sapply(1:px$num.sim,                         # Matrix of rows with transition probabilities for every individual -  Faster in apply environment
                  function(i) K[[i]][k.tm1[i],]))                        
    
    # Evaluate transitions
    K.cum   <- t(sapply(1:nrow(K),                                     # Cumulate rows of probabilities
                        function(x) cumsum(K[x,])))         
    K.cum
    K.draws <- runif(nrow(K.cum))                                      # Draw Uniform evaluation values
    K.t     <- sapply(1:nrow(K.cum),                                   # Locate lowest probability larger than the uniform draw
                      function(x) min(which(K.cum[x,] >= K.draws[x]))) 
    
    Sim.K[,t] <- K.t # Store values 
    
    ## Income last period
    CI <- CapIn(A.tm1,px) # Capital income over period
    
    # Unemployment benefits
    UB <- UnemBen(t = t-1, k.tm1, h.tm1, px, p1)
    
    if(Sim$Sim==TRUE & Sim$NoUB == TRUE){ # No UB if Basic income simulation is run
      UB <- 0
    } 
    
    # Wage
    WI <- WageHourly(t = t-1,  k.tm1, px, p1) * Hours(h.tm1)
    
    # Deductions
    DE <- Deduc(A.tm1,t=t-1,k.tm1,px,p1)
    
    # Taxable income
    TI <- pmax(CI + WI + UB - DE,0)
    
    # Taxation and shocks
    
    if(Sim$Sim == TRUE & (
      (Sim$Trans  == TRUE & Sim$shockt == t) |
      (Sim$Regime == TRUE) |
      (Sim$Int    == TRUE & Sim$shockt >= t))){
      
      if(Sim$S1 == "TaxEL" | Sim$S2 == "TaxEL"){
        
        if(t == 1) print("margtax")
        Tax <- TaxF(TI,px,p1) + Sim$margtax * (WageHourly(t = t-1, k.tm1, px, p1) * Hours(h.tm1) + UB)
        
      } else if(Sim$S1 == "TaxT" | Sim$S2 == "TaxT") {
        
        if(t == 1) print("TTtax")
        
        Tax <- TaxSim(TI,px,p1)
        
        } else {
       
         Tax <- TaxF(TI,px,p1)
      
         }
    } else {
      
      if(t == 1) print("normtax")
      Tax <- TaxF(TI,px,p1)
      
    }
    
    # Income subtracted fixed costs
    Inc <- FCW(t = t-1, k.tm1,h.tm1, pm, px, p1)
    
    # Basic Income
    if(Sim$Sim == TRUE &
       (Sim$Trans  == TRUE & Sim$shockt == t) | 
       (Sim$Regime == TRUE) |
       (Sim$Int    == TRUE & Sim$shockt >= t)){
      
      BI <- ifelse(Sim$S1 == "BI" | Sim$S2 == "BI", Sim$BI, 0)
      
    }
    
    # Compensation for marshallian elasticities
    CO <- ifelse(Sim$Sim==TRUE & Sim$HI.comp == TRUE,Sim$comp,0)
    
    # Available resources start of this period
    M.t <- A.tm1*(1 + px$R) + Inc + UB - Tax + BI + CO
    
    # Calculate fall in aftertaxincome
    
    
    
    # Find interpolated values of all state spaces in the provided model solution:
    InterpCV <- lapply(1:px$num.h,FUN =
                         function(x){InterpWrapperSim(i.h=x,
                                                      t = t,
                                                      Sim.M = M.t,
                                                      Solution.C = Solution.C,
                                                      Solution.M = Solution.M,
                                                      Solution.V = Solution.V,
                                                      px = px)})
    
    # Collect term
    
    AltV <- array(NA, dim = c(px$num.sim, px$num.h ,px$num.k))
    AltC <- AltV
    
    for(i.h in 1:px$num.h){
      
      AltC[ , i.h , ] <- InterpCV[[i.h]]$IntC
      AltV[ , i.h , ] <- InterpCV[[i.h]]$IntV
      
    }
    
    PredC  <- matrix(nrow = px$num.sim)
    PredVh <- matrix(nrow = px$num.sim, ncol = px$num.h)
    
    # Look up values for each observation
    Obs <- 1:px$num.sim
    
    for(i.h in 1:px$num.h){
      
      PredVh[,i.h] <- AltV[cbind(Obs,  i.h, K.t)]
      
    }
    
    # Calculate choice probabilities and logsums
    LCP <- LogCP(PredVh,px)
    
    # Choiceprob
    CP <- LCP$Choiceprob
    
    # Evaluate choice probabilities
    Cum.CP <- t(sapply(1:px$num.sim,                           # Cumulate rows of probabilities
                       function(x) cumsum(CP[x,]))) 
    
    # Evaluate choices by iid shocks if free choice
    h.draws     <- runif(nrow(Cum.CP))                                      # Draw Uniform evaluation values
    h.t.choice  <- sapply(1:nrow(Cum.CP),                                   # Locate choice as indicator for lowest probability larger than the uniform draw
                          function(x) min(which(Cum.CP[x,] >= h.draws[x]))) 
    
    # Evaluate joboffer
    # Job offer based on last period employment
    j.prob <- JProb(t,K.t,px,p1)                     # Job offer probabilities
    
    j.offer <- ifelse(j.prob > runif(px$num.sim), 1, 0) # Evaluate joboffer on uniform draws
    
    h.t <- ifelse(j.offer == 1, h.t.choice, 1)
    
    # Find predicted consumption
    C.t <- AltC[cbind(Obs,h.t,K.t)]
    
    # Store Values
    Sim.H[, t]  <- h.t
    Sim.M[, t]  <- M.t
    Sim.C[, t]  <- C.t
    Sim.A[, t]  <- M.t - C.t
    Sim.K[, t]  <- K.t
    Sim.Age[,t] <- t + px$t.min - 1
    Sim.T[,t]   <- t
    Sim.Tax[,t] <- Tax
    Sim.TI[,t]  <- TI 
    Sim.Tau[,t] <- ifelse(is.na(Tax / TI) == FALSE, Tax / TI, 0) 
    Sim.WH[,t]  <- WI
    Sim.IN[,t]  <- Inc
    Sim.UB[,t]  <- UB
    # Timer
    #if(t == 25) print("Halfway")
  }
  
  out <- data.frame("id"     = rep(1:px$num.sim,px$T-1),
                    "Age"    = as.vector(Sim.Age),
                    "Assets" = as.vector(Sim.A),
                    "Resources" = as.vector(Sim.M),
                    "ht"     = as.vector(Sim.H),
                    "k"      = as.vector(Sim.K),
                    "cons"   = as.vector(Sim.C),
                    "hours"  = Hours(as.vector(Sim.H)),
                    "t"      = as.vector(Sim.T),
                    "Tax"    = as.vector(Sim.Tax),
                    "TI"     = as.vector(Sim.TI),
                    "Tau"    = as.vector(Sim.Tau),
                    "Wage"   = as.vector(Sim.WH),
                    "Inc"    = as.vector(Sim.IN),
                    "UB"     = as.vector(Sim.UB))
  
  out$Education <- Sim$Education
  
  #print("Simulation Done!")
  return(out)
}

SimFull <- function(pm, px, p1, k.markov, A.t1, H.t1, Sim){
  
  
  Solution.sim <- 0
  Solution.bl <- 0
  if(Sim$Sim == TRUE & Sim$Trans == FALSE & Sim$hfix == FALSE){
    
    print("Solve Simulation")
    Solution.sim <- SolveEGM(pm, px, p1, Sim)
  }
  
  if(Sim$Sim == FALSE | (Sim$Sim == TRUE & Sim$Regime == FALSE) | (Sim$Sim == TRUE & Sim$Regime == TRUE & Sim$hfix == TRUE)){
    
    print("Solve Baseline")
    Solution.bl <- SolveEGM(pm, px, p1, Sim)
  }
  
  out <- Simulation(Solution.bl = Solution.bl,
                    Solution.sim = Solution.sim,
                    pm = pm,
                    px = px,
                    p1 = p1,
                    k.markov = k.markov,
                    A.t1 = A.t1,
                    H.t1 = H.t1,
                    Sim = Sim)
  
  return(out)
  
}

####################################################################################

# Not used in paper
# Deathrates 
DProb <- function(t){
  if(t %in% 25:29) {
    d <- 0.00107
  } else if(t %in% 30:34) {
    d <- 0.00146
  } else if(t %in% 35:39) {
    d <- 0.00206
  } else if(t %in% 40:44) {
    d <- 0.00298
  } else if(t %in% 45:49) {
    d <- 0.00424
  } else if(t %in% 50:54) {
    d <- 0.00669
  } else if(t %in% 55:59) {
    d <- 0.01144
  } else if(t %in% 59:64) {
    d <- 0.01933
  } else if(t %in% 65:69) {
    d <- 0.0309
  } else if(t %in% 70:74) {
    d <- 0.04917
  } else if(t %in% 75:79) {
    d <- 0.07602
  } else if(t %in% 80:84) {
    d <- 0.11718
  } else if(t %in% 85:95) {
    d <- 0.21428
  } else {
    d <- 1 
  }
  return(d)
}

# Starting age from different education levels
EduT <- function(edu) {
  x <- 0
  if (edu == "d") {
    x <- 16
  } else if (edu == "hs") {
    x <- 18
  } else if (edu == "c") {
    x <- 22
  }
  if (edu %in% c("d","hs","c")){
    return(x)
  } else {
    print("Wrong edu")
  }
}

# interpolation functions
LinInt <- function(x, y, x0vec){
  # Storage vector
  int.out <- NA
  
  # Find larger than grid linear parameters
  ndx  <- order(x, decreasing = T)[1:2]
  xmax <- x[ndx]
  
  A.max <- (y[ndx[1]] - y[ndx[2]]) / (xmax[1] - xmax[2])
  B.max <- y[ndx[1]] - A.max * xmax[1]
  
  # Find lower than grid linear parameters
  ndx <- order(x)[1:2]
  xmin <- x[ndx]
  
  A.min <- (y[ndx[1]] - y[ndx[2]]) / (xmin[1] - xmin[2])
  B.min <- y[ndx[2]] - A.min * xmin[2]
  
  for(i in 1:length(x0vec)){
    x0 <- x0vec[i]
    
    if(x0 < max(x) && x0 > min(x)){
      
      int <- approx(x,y,xout = x0)
      int <- int$y
      # print(paste0( "middle",int))
    } else if(x0 >= max(x)) {
      
      int <- B.max + A.max * x0
      # print(paste0( "over",int))
    } else if(min(x) >= x0) {
      
      int <- B.min + A.min * x0
      # print(paste0( "under",int))
    } else {
      
      print("interpolation error")
      
    }
    int.out <- c(int.out, int)
  }
  return(int.out[-1])
}

# Terminal consumption
TermCons <- function(A,k,px,p1){
  A1 <- A * 1/px$scale
  
  c <- p1$gamma0 +
    p1$gamma1 * A1 + 
    p1$gamma2 * A1 ^ 2+ 
    p1$gamma3 * k + 
    p1$gamma4 * k ^ 2
  
  c <- c * px$scale
  
  return(c)  
}

# Pension
Pens <- function(k,px,p1){
  
  P <- p1$gamma0 + 
    p1$gamma1 * k +
    p1$gamma2 * k ^ 2 +
    p1$gamma3 *k ^ 3 
  
  return(P * px$scale)
}

#########################################
### Motions of deterministic states #####
#########################################

# part time last period
Nextpl <- function(h){
  
  n.pl <- ifelse(h %in% part.t, 2,1)
  
  return(n.pl)
  
}

# Eligibility
NextEL <- function(h,pl,EL,mEL){
  if(h %in% full.t){
    
    n.EL <- mEL
    
  }
  
  if(h %in% part.t) { # ?
    
    if (pl == 1) {
      
      n.EL <- max(EL - 1, 1)
      
    } else { 
      
      n.EL <- mEL 
      
    }
  }
  
  if(h == 1) {
    
    n.EL <- max(EL - 1, 1)
    
  }
  
  return(n.EL)
  
}
