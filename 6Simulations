################################################################
# "Danish Labour Supply with Human Capital -                   #
# Evidence from a Structural Life-Cycle Model"                 #
# By: Andreas Als Langholz                                     #  
# Thesis for M.sc. in Economics 2018                           #
################################################################

################################################################
# File 6: Simulations                                          #
################################################################

# Manual input Long HE parameter values to test run simulations
# without first running MLE estimator
pm <- data.frame(1)
pm$u.a1     <- 0.02863618
pm$u.a2     <- 1.001731



pm$u.b1     <- 0.0005724853
pm$b.delt   <- 0.3039557
pm <- c(pm$u.a1, pm$u.a2, pm$u.b1,pm$b.delt)


######################################
#### Baseline comparison #############
######################################
# Simulation controls
Sim$Sim     <- FALSE # TRUE/FALSE, if Sim args are used
Sim$Trans   <- FALSE # TRUE/FALSE, if change is transitory (not in EGM)
Sim$Regime  <- FALSE # TRUE/FALSE, if change is permanent should it be the full life-cycle (regime shift) or come as a suprise from a given age
Sim$Int     <- FALSE # TRUE/FALSE, if simulation changes are happening during life-cycle
Sim$HI.comp <- FALSE # TRUE/FALSE, if compensation for taxation is used (Hicks)
Sim$NoUB    <- FALSE # TRUE/FALSE, if UB is substituted for BI
Sim$S1      <- "TaxEL" # Specifying type of simulation
Sim$margtax <- 0.05 # MArginal taxrate used in experiemetns

# Loop over educations
edusec <- c("LVU", "Faglærte", "Ufaglærte") 

for(edu in edusec) {
  
  p1 <- p1out[which(p1out$edux==edu),]
  #pm <- pmout[which(pmout$edux==edu),]
  
  # Starting values and frequencies
  # Draw starting values from initial distribution
  In.A <- startval$Val[which(startval$edu == edu)] 
  A.t1 <- sample(In.A,px$num.sim,replace = TRUE)
  
  # Draw starting frequencies of assets and labour supply
  S.freq          <- stfreq[which(stfreq$edu == edu),]
  S.freq$cumfreq  <- cumsum(S.freq$freq)
  j.draws         <- runif(px$num.sim)
  H.t1            <- sapply(1:length(j.draws), function(x) min(which(S.freq$cumfreq > j.draws[x])))
  
  SimBase <- SimFull(pm, px, p1, k.markov, A.t1, H.t1, Sim)
  
  g.sim <- SimBase[which(SimBase$Age> 24),] %>% 
    group_by(Age) %>%
    dplyr:: summarise(m.c = mean(cons), t= mean(t), m.h = mean(ht), m.hours = mean(hours), m.a = mean(Assets), m.r = mean(Resources))
  
  SimBase$Education <- edu
  if(edu == edusec[1]){

    base.out.f <- SimBase
    base.out <- g.sim
  
    } else {
    base.out.f <- rbind(base.out.f,SimBase)
    base.out <- rbind(base.out,g.sim)
    
  }
}

#########################################
#### SIMULATE ELASTICITIES ##############
#########################################
set.seed(25) 
# Loop over educations
edusec <- c("LVU", "Faglærte", "Ufaglærte") 

for(edu in edusec) {
   
  p1 <- p1out[which(p1out$edux==edu),]
  #pm <- pmout[which(pmout$edux==edu),]
  
  # Starting values and frequencies
  # Draw starting values from initial distribution
  In.A <- startval$Val[which(startval$edu == edu)] 
  A.t1 <- sample(In.A,px$num.sim,replace = TRUE)
  
  # Draw starting frequencies of assets and labour supply
  S.freq          <- stfreq[which(stfreq$edu == edu),]
  S.freq$cumfreq  <- cumsum(S.freq$freq)
  j.draws         <- runif(px$num.sim)
  H.t1            <- sapply(1:length(j.draws), function(x) min(which(S.freq$cumfreq > j.draws[x])))
  
  Solution.sim <- 0
  Solution.bl <- 0
  
  ######## FRISCH #########################
  # Frisch: Transitory shock at a given age
  
  # 1: Solve model
  Solution.bl <- SolveEGM(pm, px, p1, Sim)
  Solution.sim <- 0
  
  # 2: setup simulation parameters
  Sim$Sim     <- FALSE
  Sim$Trans   <- FALSE
  Sim$Int     <- FALSE
  Sim$Regime  <- FALSE
  Sim$S1      <- "TaxEL" # Command "Tax, UI, BI" for whether to xrun Taxation, Unemployment Insurance or Basic income experiment
  Sim$S2      <- "X"  # If multiple experiments are run at the same time
  Sim$margtax <-0.05
  Sim$Education <- edu
  
  # 3: Make baseline simulation
  Fr.Base <- Simulation(Solution.bl = Solution.bl,
                        Solution.sim = Solution.sim,
                        pm,px,p1,K.Markov,A.t1, H.t1, Sim) 
  
  # 4: Change to simulation and make shock vector
  
  Sim$Sim   <- TRUE
  Sim$Trans <- TRUE
  t.schock  <- px$t.min:px$t.max - px$t.min + 1
  
  # 5: Loop over shocks and combine dataset only with shock years
  for(s.t in t.schock){
    Sim$shockt <- s.t
    
    df.sim <- Simulation(Solution.bl = Solution.bl,
                         Solution.sim = Solution.sim,
                         pm,px,p1,K.Markov,A.t1, H.t1, Sim) 
    
    obst <- df.sim[which(df.sim$t == s.t),]
    
    if(s.t == t.schock[1]){
      
      Fr.sim <- obst
      
    } else {
      
      Fr.sim <- rbind(Fr.sim,obst)
      
    }
  }
  
  
  # 6: Transform into mean datasets and calculate elasticity
  Fr.Base.g <- Fr.Base %>% 
    group_by(Age) %>%
    dplyr:: summarise(m.tau = mean(Tau), m.h = mean(hours))
  
  Fr.Sim.g <- Fr.sim %>% 
    group_by(Age) %>%
    dplyr:: summarise(m.tau = mean(Tau), m.h = mean(hours))
  
  df.fr <- data.frame("Age" = Fr.Base.g$Age,
                      "hrBase" = Fr.Base.g$m.h,
                      "hrsim" = Fr.Sim.g$m.h,
                      "taub" = Fr.Base.g$m.tau,
                      "taus" = Fr.Sim.g$m.tau)
  
  df.fr$el <- -((df.fr$hrBase - df.fr$hrsim) / df.fr$hrBase) / ((df.fr$taub - df.fr$taus)/(1-df.fr$taub))
  
  df.fr$el.s <- predict(loess(el ~Age,data = df.fr), data=df.fr) 
  
  df.fr$Type <- "Frisch"
  
  ########## Marshall #########################
  # MArshall: Permanent and uncompensated
  
  # 1: Simulate baseline
  Sim$Sim       <- FALSE
  Sim$Trans     <- FALSE
  Sim$Regime    <- FALSE  # TRUE/FALSE, if change is permanent should it be the full life-cycle (regime shift) or come as a suprise from a given age
  Sim$Int       <- FALSE #
  Sim$S1        <- "TaxEL" # Command "Tax, UI, BI" for whether to xrun Taxation, Unemployment Insurance or Basic income experiment
  Sim$margtax   <-0.05
  Sim$Education <- edu
  
  MA.base <- SimFull(pm,px,p1,K.Markov,A.t1, H.t1, Sim)
  
  # 2: Simulate regime change
  Sim$Sim     <- TRUE
  Sim$Regime  <- TRUE
  
  MA.sim <- SimFull(pm,px,p1,K.Markov,A.t1, H.t1, Sim)
  
  # 3: Calculate elasticities
  MA.Base.g <- MA.base %>% 
    group_by(Age) %>%
    dplyr:: summarise(m.tau = mean(Tau), m.h = mean(hours))
  
  MA.Sim.g <- MA.sim %>% 
    group_by(Age) %>%
    dplyr:: summarise(m.tau = mean(Tau), m.h = mean(hours))
  
  df.ma <- data.frame("Age" = MA.Base.g$Age,
                      "hrBase" = MA.Base.g$m.h,
                      "hrsim" = MA.Sim.g$m.h,
                      "taub" = MA.Base.g$m.tau,
                      "taus" = MA.Sim.g$m.tau)
  
  df.ma$el <- -((df.ma$hrBase - df.ma$hrsim) / df.ma$hrBase) / ((df.ma$taub - df.ma$taus)/(1-df.ma$taub))
  
  df.ma$el.s <- predict(loess(el ~Age,data = df.ma), data=df.ma) 
  
  df.ma$Type <- "Marshall"
  
  ############## Hicks ###################
  # Marshall: Permanent and compensated
  
  # 1: Simulate basline
  Sim$Sim     <- FALSE
  Sim$Trans   <- FALSE
  Sim$Regime  <- FALSE  # TRUE/FALSE, if change is permanent should it be the full life-cycle (regime shift) or come as a suprise from a given age
  Sim$Int     <- FALSE #
  Sim$S1      <- "TaxEL" # Command "Tax, UI, BI" for whether to xrun Taxation, Unemployment Insurance or Basic income experiment
  Sim$S2      <- "X"  # If multiple experiments are run at the same time
  Sim$margtax <- 0.05
  Sim$Education <- edu
  Sim$HI.comp <- FALSE
  
  HI.base <- SimFull(pm,px,p1,K.Markov,A.t1, H.t1, Sim)
  
  # 2: Calculate needed compensation
  # 2.1: calculate expected mean tax differential
  HI.base$dtax <- (HI.base$Wage + HI.base$UB) * Sim$margtax
  
  HI.c <- HI.base %>% 
    group_by(t) %>%
    dplyr:: summarise(dtax = mean(dtax))
  
  # 2.2: Calculate discounted ecpected cashflows over the life-cycle and solve for yield value
  PV <- 0
  for(i.t in 1:(px$T-1)){
    
    PV <- PV + HI.c$dtax[i.t] / (1+px$R) ^(i.t-1)
    
  }
  
  Sim$comp <-PV * ((1 - (1 / (1 + px$R)) ^ (px$T-1)) / (1 - (1 / (1 + px$R)))) ^ (-1)
  
  comp <- Sim$comp
  
  # 3 Simulate again with compensation
  Sim$Sim     <- TRUE
  Sim$Regime  <- TRUE 
  Sim$HI.comp <- TRUE
  
  HI.sim <- SimFull(pm,px,p1,K.Markov,A.t1, H.t1, Sim)
  
  HI.Base.g <- HI.base %>% 
    group_by(Age) %>%
    dplyr:: summarise(m.tau = mean(Tau), m.h = mean(hours))
  
  HI.Sim.g <- HI.sim %>% 
    group_by(Age) %>%
    dplyr:: summarise(m.tau = mean(Tau), m.h = mean(hours))
  
  df.hi <- data.frame("Age" = HI.Base.g$Age,
                      "hrBase" = HI.Base.g$m.h,
                      "hrsim" = HI.Sim.g$m.h,
                      "taub" = HI.Base.g$m.tau,
                      "taus" = HI.Sim.g$m.tau)
  
  df.hi$el   <- -((df.hi$hrBase - df.hi$hrsim) / df.hi$hrBase) / ((df.hi$taub - df.hi$taus)/(1-df.hi$taub)) 
  
  df.hi$el.s <- predict(loess(el ~Age,data = df.hi), data=df.hi)
  
  df.hi$Type <- "Hicks"
  
  # Combine to one dataframe
  df.el <- rbind(df.fr,df.ma,df.hi)
  
  df.el$Education <- edu
  
  if(edu == edusec[1]){
    df.el.out <- df.el
    comp.out <- comp
    } else {
    
    df.el.out <- rbind(df.el.out,df.el)
    comp.out <- c(comp.out,comp)
    
    }
}

#########################################
#### Simulate Taxation Changes ##########
#########################################
# Loop over educations
edusec <- c("LVU", "Faglærte", "Ufaglærte") 

# Surprise tax changes
tt.schock <- c(px$t.min, 30,35,40,45,50,55, 60) - px$t.min + 1

for(edu in edusec) {
  print(edu)
  p1 <- p1out[which(p1out$edux==edu),]
  #pm <- pmout[which(pmout$edux==edu),]
  
  # Starting values and frequencies
  # Draw starting values from initial distribution
  In.A <- startval$Val[which(startval$edu == edu)] 
  A.t1 <- sample(In.A,px$num.sim,replace = TRUE)
  
  # Draw starting frequencies of assets and labour supply
  S.freq          <- stfreq[which(stfreq$edu == edu),]
  S.freq$cumfreq  <- cumsum(S.freq$freq)
  j.draws         <- runif(px$num.sim)
  H.t1            <- sapply(1:length(j.draws), function(x) min(which(S.freq$cumfreq > j.draws[x])))
  
  # Simulate Baseline
  Sim$Sim     <- FALSE
  Sim$Trans   <- FALSE
  Sim$Regime  <- FALSE  # TRUE/FALSE, if change is permanent should it be the full life-cycle (regime shift) or come as a suprise from a given age
  Sim$Int     <- FALSE #
  Sim$S1      <- "TaxT" # Command "Tax, UI, BI" for whether to xrun Taxation, Unemployment Insurance or Basic income experiment
  Sim$S2      <- "X"  # If multiple experiments are run at the same time
  Sim$Education <- edu
  Sim$HI.comp    <- FALSE
  
  TT.base <- SimFull(pm,px,p1,K.Markov,A.t1, H.t1, Sim)
  
  TT.base.g <- TT.base %>% 
    group_by(Age) %>%
    dplyr:: summarise(m.c = mean(cons), m.h = mean(ht), m.hours = mean(hours))
  
  
  # Baseline model solution
  Solution.bl <- SolveEGM(pm, px, p1, Sim)
  
  Sim$Sim     <- TRUE
  Sim$Regime  <- TRUE  # TRUE/FALSE, if change is permanent should it be the full life-cycle (regime shift) or come as a suprise from a given age
  Sim$S1      <- "TaxT"
  
  # Simulation model solution
  Solution.sim <- SolveEGM(pm, px, p1, Sim)
  
  Sim$Regime  <- FALSE
  Sim$Int     <- TRUE
  
  for(s.t in tt.schock){
    Sim$shockt <- s.t
    print(Sim$shockt)
    
    df.sim <- Simulation(Solution.bl = Solution.bl,
                          Solution.sim = Solution.sim,
                         pm = pm,
                         px = px,
                         p1 = p1,
                         k.markov = k.markov,
                         A.t1 = A.t1,
                         H.t1 = H.t1,
                         Sim = Sim)
    
    tt.sim <- df.sim %>% 
      group_by(Age) %>%
      dplyr:: summarise(m.c = mean(cons), m.h = mean(ht), m.hours = mean(hours))
    
    df.tt      <- data.frame("Age" = tt.sim$Age, "t"=tt.sim$Age-px$t.min+1, "hschock" = tt.sim$m.hours, "hbase" = TT.base.g$m.hours)
    df.tt$difh <- (df.tt$hschock - df.tt$hbase) / df.tt$hbase * 100
    
    dift    <- df.tt$difh[which(df.tt$t == s.t)]-10
    difmean <- mean(df.tt$difh[which(df.tt$t >= s.t)]-10)
    
    df.tt  <- data.frame("Age" = s.t+px$t.min-1,"shockt" = dift,"shocktm1" = diftm, "shocktp1" = diftp, "shockmean" = difmean)
    
    if(s.t == t.schock[1]){
      
      tt.out <- df.tt
      
    } else {
      
      tt.out <- rbind(tt.out,df.tt)
      
    }
  }
  
  tt.out$Education <- edu
  
  if(edu == edusec[1]){
    
    df.tt.out <- tt.out
    
    } else {
    
    df.tt.out <- rbind(df.tt.out,tt.out)

  }
}


#####################################
#### Simulate Basic Income ##########
#####################################
# Shock values
t.schock <- c(px$t.min, 30,35,40,45,50,55, 60) - px$t.min + 1
BIval <- c(60000,120000,180000)

# Loop over educations
edusec <- c("LVU", "Faglærte", "Ufaglærte")

for(edu in edusec) {
  
  p1 <- p1out[which(p1out$edux==edu),]
  #pm <- pmout[which(pmout$edux==edu),]
  
  # Starting values and frequencies
  # Draw starting values from initial distribution
  In.A <- startval$Val[which(startval$edu == edu)] 
  A.t1 <- sample(In.A,px$num.sim,replace = TRUE)
  
  # Draw starting frequencies of assets and labour supply
  S.freq          <- stfreq[which(stfreq$edu == edu),]
  S.freq$cumfreq  <- cumsum(S.freq$freq)
  j.draws         <- runif(px$num.sim)
  H.t1            <- sapply(1:length(j.draws), function(x) min(which(S.freq$cumfreq > j.draws[x])))
  
  
  # Regime change
  Sim$Sim     <- FALSE
  Sim$Trans   <- FALSE
  Sim$Regime  <- TRUE  # TRUE/FALSE, if change is permanent should it be the full life-cycle (regime shift) or come as a suprise from a given age
  Sim$Int     <- FALSE #
  Sim$Ant     <- FALSE #
  Sim$S1      <- "BI" # Command "Tax, UI, BI" for whether to xrun Taxation, Unemployment Insurance or Basic income experiment

  Sim$Education <- edu
  Sim$HI.comp    <- FALSE
  Sim$NoUB    <- FALSE
  
  
  # Baseline simulation for comparison
  BI.base <- SimFull(pm,px,p1,K.Markov,A.t1, H.t1, Sim)
  
  BI.base.g <- BI.base %>% 
    group_by(Age) %>%
    dplyr:: summarise(m.c = mean(cons), m.h = mean(ht), m.hours = mean(hours))
  
  # Baseline model solution 
  Solution.bl <- SolveEGM(pm, px, p1, Sim)
  
  # Loop over values of Basic income
  for(BIv in BIval){  
  # Simulation model solution
  Sim$Sim     <- TRUE
  Sim$Regime  <- TRUE  
  Sim$NoUB    <- TRUE
  Sim$BI      <- BIv
  Solution.sim <- SolveEGM(pm, px, p1, Sim)
  
  # Sim controls for simulation
  Sim$Regime  <- FALSE
  Sim$Int     <- TRUE

    for(s.t in tt.schock){
      Sim$shockt <- s.t
      
      df.sim <- Simulation(Solution.bl = Solution.bl,
                           Solution.sim = Solution.sim,
                           pm = pm,
                           px = px,
                           p1 = p1,
                           k.markov = k.markov,
                           A.t1 = A.t1,
                           H.t1 = H.t1,
                           Sim = Sim)
      
      bi.sim <- df.sim %>% 
        group_by(Age) %>%
        dplyr:: summarise(m.c = mean(cons), m.h = mean(ht), m.hours = mean(hours))
      
      df.bis      <- data.frame("Age" = bi.sim$Age, "t"=bi.sim$Age-px$t.min+1, "hschock" = bi.sim$m.hours, "hbase" = BI.base.g$m.hours)
      df.bis$difh <- (df.bis$hschock - df.bis$hbase) / df.bis$hbase * 100
      
      dift    <- df.bis$difh[which(df.bis$t == s.t)]
      difmean <- mean(df.bis$difh[which(df.bis$t >= s.t)])
      
      df.bi  <- data.frame("BI"=BIv, "Age" = s.t+px$t.min-1,"shockt" = dift, "shockmean" = difmean )
      
      if(s.t == t.schock[1]){
        
        st.out <- df.bi
        
      } else {
        
        st.out <- rbind(st.out, df.bi)
        
      }
    }
  
  if(BIv == BIval[1]){
    
    bi.out <- st.out
    
    } else {
    
      bi.out <- rbind(bi.out, st.out)
    
    }
  }
  
  bi.out$Education <- edu
  
  if(edu == edusec[1]){
    
    df.bi.out <- bi.out
    
  } else {
    
    df.bi.out <- rbind(df.bi.out,bi.out)
    
  }
}
