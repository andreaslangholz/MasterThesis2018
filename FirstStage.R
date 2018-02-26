
################################################################
# "Danish Unemployment Insurance - A Structural Dynamic Model" #
# By: Andreas Als Langholz                                     #  
# Dissertation in Economics 2018                               #
################################################################

################################################################
# File 2: First stage regressions of Joboffers,                #
# Wage, Tax, Pension and Decution regressions                  #
################################################################

DATA.FS1 <- read.csv(file = "D:\\FM\\andla\\Data\\datafs.csv")

K.Markov <- list.load("D:\\FM\\andla\\Data\\KM.RData")
AE <- read.csv("D:\\FM\\andla\\Data\\avge.csv")
AvgExp <- AE[,2]

DATA.FS <- DATA.FS1[DATA.FS1$wealth > 1,]

###########################
# First Stage Regressions #
###########################
##### TAXATION #####################################
# Run same taxation Equation on full dataset
# Logarithmic (no kinks)
df.tp <- DATA.FS[-which(DATA.FS$tax %in% tail(sort(DATA.FS$tax),250) |
                          DATA.FS$taxinc %in% tail(sort(DATA.FS$taxinc),250) |
                          is.na(DATA.FS$taxinc)),]

df.tp$logtax    <- log(df.tp$tax)
df.tp$logtaxinc <- log(df.tp$taxinc)
df.tp           <- df.tp[-which(df.tp$logtax == -Inf | df.tp$logtaxinc == -Inf),]

RegTaxLog <- lm(logtax ~ logtaxinc, data =df.tp) 
summary(RegTaxLog)

df.tp$Logarithmic <- exp(predict(RegTaxLog))

# Linear Whith kinks
df.tp$t1 = pmin(df.tp$taxinc, px$kink1)
df.tp$t2 = pmin(pmax(0, df.tp$taxinc - px$kink1), px$kink2 - px$kink1)
df.tp$t3 = pmax(0, df.tp$taxinc - px$kink2)

RegTaxKink <- lm(tax ~ t1 + t2 + t3, data =df.tp)
df.tp$Kinked <- predict(RegTaxKink)
summary(RegTaxKink)

# RMSE
sqrt(mean((df.tp$Logarithmic - df.tp$tax)^2))
sqrt(mean(RegTaxKink$residuals^2))

# TEST

#######################################
##### Loop over educations ############
#######################################

edulevels <- levels(DATA.FS$edu)

for(edu in edulevels){
  
  DATA.EDU <- DATA.FS[which(DATA.FS$edu == edu & DATA.FS$age <66 & DATA.FS$age > 24),]
  summary(DATA.EDU)
  ### Joboffer logit ##################

  df.job <- data.frame("joboffer" = DATA.EDU$joboffer,
                     "ht"   = DATA.EDU$ht,
                     "age"  = DATA.EDU$age,
                     "ul"   = DATA.EDU$ul,
                     "k"    = DATA.EDU$k)

  df.job$t1  <- df.job$age
  df.job$t2  <- df.job$age ^ 2
  df.job$t3  <- (df.job$age - 30) * (df.job$age > 30)
  df.job$t4  <- (df.job$age - 45) * (df.job$age > 45)
  df.job$t5  <- (df.job$age - 55 )* (df.job$age > 55)
  df.job$t6  <- df.job$k
  df.job$t7  <- df.job$k ^2
  df.job$t8  <- (df.job$age - 30) * df.job$k * (df.job$age > 30)
  df.job$t9  <- (df.job$age - 45) * df.job$k * (df.job$age > 45)
  df.job$t10 <- (df.job$age - 55) * df.job$k * (df.job$age > 55)
  
  joblogit <- glm(joboffer ~ t1 + t2 + t3 + t4 + t5 +
                    t6 + t7 + t8 + t9 + t10 , data = df.job,
                  family = "binomial")
  
  ### WAGES #######################################
  df.wage <- data.frame("wage"  = DATA.EDU$wageh,
                        "K"     = DATA.EDU$k,
                        "KSq"   = DATA.EDU$k ^ 2,
                        "Age"   = DATA.EDU$age,
                        "AgeSq" = DATA.EDU$age ^ 2,
                        "AgeT"  = DATA.EDU$age ^ 3,
                        "work"  = DATA.EDU$ht)
  
  df.wage.full <- df.wage[which(df.wage$K > 1 & df.wage$Age < 66 & df.wage$Age >24 ),]
  
  # Estimate linear model
  RegWage <- lm(wage ~  K + KSq + Age + AgeSq + AgeT + Age*K, data = df.wage.full)

  #### Unemployment ##################################
  df.unem <- data.frame("UB"    = DATA.EDU$unemb,
                        
                        "K"     = DATA.EDU$k,
                        "KSq"   = DATA.EDU$k ^2,
                        "Age"   = DATA.EDU$age,
                        "AgeSq" = DATA.EDU$age^2,
                        "LogAge" = log(DATA.EDU$age),
                        "AgeT"  = DATA.EDU$age ^3,
                        "ul"    = DATA.EDU$ul,
                        "ht"    = DATA.EDU$ht)
  df.unem <- df.unem[which(df.unem$UB > 0),]
  
  df.unem.u         <- df.unem[which(df.unem$ht == 1), ]
  df.unem.u$predw   <- predict(RegWage,df.unem.u) * ((Hours(4) + Hours(5)) / 2) 
  df.unem.u$predwsq <- df.unem.u$predw ^ 2
  
  RegUBFull    <- lm(UB ~ LogAge+ Age + predw + predwsq, data = df.unem.u)
  
  df.unem.p         <- df.unem[which(df.unem$ht %in% part.t), ]
  df.unem.p$predw   <- predict(RegWage,df.unem.p) * (Hours(3)+Hours(2))/2
  df.unem.p$predwsq <- df.unem.p$predw ^ 2
  
  RegUBPart  <- lm(UB ~ LogAge+ Age + predw + predwsq, data = df.unem.p)
  
  ### Deductions ##################################
  df.deduc <- data.frame("deduc" = DATA.EDU$deduct,
                         "LogA"   = log(DATA.EDU$wealth),
                         "W"   = DATA.EDU$wageh,
                         "Wsq" = DATA.EDU$wageh^2,
                         "Wt" = DATA.EDU$wageh^3)
  
  RegDed <- lm(deduc~., data = df.deduc)
  summary(RegDed)
  
  ################################################
  ### Construct First stage parameter vector #####
  ################################################
  p1   <-data.frame(edux = edu)
  p1[] <-lapply(p1,as.character)
  
  # Joboffers
  p1$m0  <- joblogit$coefficients[1]
  p1$m11 <- joblogit$coefficients[2]
  p1$m12 <- joblogit$coefficients[3]
  p1$m13 <- joblogit$coefficients[4]
  p1$m14 <- joblogit$coefficients[5]
  p1$m15 <- joblogit$coefficients[6]
  p1$m21 <- joblogit$coefficients[7]
  p1$m22 <- joblogit$coefficients[8]
  p1$m23 <- joblogit$coefficients[9]
  p1$m24 <- joblogit$coefficients[10]
  p1$m25 <- joblogit$coefficients[11]
  
  # Wages
  p1$w01   <- RegWage$coefficients[1]
  p1$alpha11 <- RegWage$coefficients[2]
  p1$alpha12 <- RegWage$coefficients[3]
  p1$alpha13 <- RegWage$coefficients[4]
  p1$alpha14 <- RegWage$coefficients[5]
  p1$alpha15 <- RegWage$coefficients[6]
  p1$alpha16 <- RegWage$coefficients[6]
  
  # Taxes 
  p1$delta0 <- RegTaxKink$coefficients[1]
  p1$delta1 <- RegTaxKink$coefficients[2]
  p1$delta2 <- RegTaxKink$coefficients[3]
  p1$delta3 <- RegTaxKink$coefficients[4]
  
  # Deductions
  p1$d0 <- RegDed$coefficients[1]
  p1$d1 <- RegDed$coefficients[2]
  p1$d2 <- RegDed$coefficients[3]
  p1$d3 <- RegDed$coefficients[4]
  p1$d4 <- RegDed$coefficients[5]
  
  # Unemployment
  p1$mu10 <- RegUBFull$coefficients[1]
  p1$mu11 <- RegUBFull$coefficients[2]
  p1$mu12 <- RegUBFull$coefficients[3]
  p1$mu13 <- RegUBFull$coefficients[4]
  p1$mu14 <- RegUBFull$coefficients[5]

  p1$mu20 <- RegUBPart$coefficients[1]
  p1$mu21 <- RegUBPart$coefficients[2]
  p1$mu22 <- RegUBPart$coefficients[3]
  p1$mu23 <- RegUBPart$coefficients[4]
  p1$mu24 <- RegUBPart$coefficients[5]

  print(edu)

  # Autoexport model coefficient and standard errors in latex format
  
  jl <- as.data.frame(cbind(summary(joblogit)$coefficients[,1],summary(joblogit)$coefficients[,2],summary(joblogit)$coefficients[,4]))
  names(jl) <- c(paste0("Est-",edu),paste0("Std.err-",edu),paste0("P.val-",edu))
  
  ded <- as.data.frame(cbind(summary(RegDed)$coefficients[,1],summary(RegDed)$coefficients[,2],summary(RegDed)$coefficients[,4]))
  names(ded) <- c(paste0("Est-",edu),paste0("Std.err-",edu),paste0("P.val-",edu))
  
  ubp <- as.data.frame(cbind(summary(RegUBPart)$coefficients[,1],summary(RegUBPart)$coefficients[,2],summary(RegUBPart)$coefficients[,4]))
  names(ubp) <- c(paste0("Est-",edu),paste0("Std.err-",edu),paste0("P.val-",edu))
  
  ubf <- as.data.frame(cbind(summary(RegUBFull)$coefficients[,1],summary(RegUBFull)$coefficients[,2],summary(RegUBFull)$coefficients[,4]))
  names(ubf) <- c(paste0("Est-",edu),paste0("Std.err-",edu),paste0("P.val-",edu))
  
  wh <- as.data.frame(cbind(summary(RegWage)$coefficients[,1],summary(RegWage)$coefficients[,2],summary(RegWage)$coefficients[,4]))
  names(wh) <- c(paste0("Est-",edu),paste0("Std.err-",edu),paste0("P.val-",edu))
  
  if(edu == edulevels[1]){
    
    p1out  <- p1
    outjl  <- jl
    outded <- ded
    outubp <- ubp
    outubf <- ubf
    outwh  <- wh
    
  } else {
    
    p1out <- rbind(p1out,p1)
    outjl  <- cbind(outjl,jl)
    outded <- cbind(outded,ded)
    outubp <- cbind(outubp,ubp)
    outubf <- cbind(outubf,ubf)
    outwh  <- cbind(outwh,wh)
    
  }
  
}


# Simulation starting values
for(edu in edulevels){
  df.samp <- DATA.FS[which(DATA.FS$edu == edu),]
  we <- df.samp$wealth[which(df.samp$age < 23)]
  samp <- sample(we, 10000, replace = TRUE)  
  
  s1 <- data.frame("Val" = samp, "edu" = edu)
  
  if(edu == edulevels[1]){
    
    out <- s1
  
    } else {
    
      out <- rbind(out,s1)
  }
}

# Frequencies
# Total
freq <- DATA.FS[DATA.FS$age >26 & DATA.FS$age < 65,] %>% 
  group_by(edu,ht) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# Starting frequencies:
stfreq <- DATA.FS[DATA.FS$age <26 & DATA.FS$age > 22,] %>% 
  group_by(edu,ht) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n))


DATA.FS$Education <- ifelse(DATA.FS$edu=="Ufaglærte", "Unskilled",
                            ifelse(DATA.FS$edu=="Faglærte", "Skilled",
                                   ifelse(DATA.FS$edu=="KVU", "Short HE",
                                          ifelse(DATA.FS$edu=="MVU", "Medium HE",
                                                 ifelse(DATA.FS$edu=="LVU", "Long HE", NA) ))))

# Summaries for simulation comparisons
df.age <- DATA.FS %>%
        group_by(age,Education) %>%
        dplyr::summarise(meanc = mean(cons), 
                   meanh = mean(ht), 
                   meanw = mean(wealth), 
                   meanhour = mean(workh))
  
# Consumption by resources
# discretise wealth
num.points <- 200
df.p <- DATA.FS
cw <- UEplot(min(df.p$wealth),max(df.p$wealth)+1, num.points+1,1.5)
# Define groups (now vectorized)
cw.mat <- matrix(nrow = nrow(df.p), ncol = num.points)
for(k in 1:num.points){
  cw.mat[ ,k] <-  ifelse(df.p$wealth < cw[k + 1], k, 1000)
}
df.p$w.group  <- rowMins(cw.mat)
df.p$Resources <- cw[df.p$w.group]
wl <- df.p %>% 
  group_by(Resources,Education) %>%
  dplyr::summarise(MeanC = mean(cons), meanh = mean(ht), meanw = mean(wealth), meanhour = mean(workh))

# Write output
write.csv(p1out, file = "D:\\FM\\andla\\Data\\p1out.csv")
write.csv(out,   file = "D:\\FM\\andla\\Data\\stval.csv")
write.csv(stfreq,file = "D:\\FM\\andla\\Data\\stfreq.csv")
write.csv(freq,  file = "D:\\FM\\andla\\Data\\freq.csv")
write.csv(wl,    file = "D:\\FM\\andla\\Data\\dfres.csv")
write.csv(df.age,file = "D:\\FM\\andla\\Data\\dfage.csv")

print(xtable(outwh,digits  = c(rep(4,3*5+1))),file = "D:\\FM\\andla\\Data\\LatexFSoutput\\wagec.txt")
print(xtable(outjl,digits  = c(rep(4,3*5+1))),file = "D:\\FM\\andla\\Data\\LatexFSoutput\\jl.txt")
print(xtable(outubp,digits = c(rep(4,3*5+1))),file = "D:\\FM\\andla\\Data\\LatexFSoutput\\ubp.txt")
print(xtable(outubf,digits = c(rep(4,3*5+1))),file = "D:\\FM\\andla\\Data\\LatexFSoutput\\ubf.txt")
print(xtable(outded,digits = c(rep(4,3*5+1))),file = "D:\\FM\\andla\\Data\\LatexFSoutput\\ded.txt")


########################################################################

#################### OTHER ############################################
# 
# #### Pensions #####################################
# # Cutoff points discretization of groups
# df.pens <- DATA.pens
# 
# cutoff.p <- seq(min(df.pens$exp_tot), max(df.pens$exp_tot)+0.1, length.out = px$num.k + 1 )
# 
# # Define groups (now vectorized)
# k.mat.p <- matrix(nrow = nrow(df.pens), ncol = px$num.k)
# for(k in 1:px$num.k){
#   
#   k.mat.p[ ,k] <-  ifelse(df.pens$exp_tot < cutoff.p[k + 1], k, 100)
#   
# }
# df.pens$k  <- rowMins(k.mat.p)
# 
# df.pens$delta.year <- df.pens$year - 2001
# df.pens$wealth <- df.pens$wealth   / ((1 + px$inflation) ^ df.pens$delta.year) 
# df.pens$pens   <- df.pens$tot_pens / ((1 + px$inflation) ^ df.pens$delta.year) 
# 
# df.pens <- data.frame("pens" = df.pens$pens,
#                       "k" = df.pens$k,
#                       "kSq" = df.pens$k ^2,
#                       "kt" = df.pens$k ^3)
# 
# RegPens <- lm(pens~ k + kSq + kt, data = df.pens)
# 
# 
# ### Terminal consumption ####
# df.c <- data.frame("c" = DATA.EDU$cons,
#                    "Age"   = DATA.EDU$age,
#                    "A"     = DATA.EDU$wealth,
#                    "Asq"   = DATA.EDU$wealth^2,
#                    "k"     = DATA.EDU$k,
#                    "ksq"   = DATA.EDU$k^2)
# 
# df.c <- df.c[which(df.c$Age >= px$t.max),]
# 
# RegTerm <- lm(c ~. -Age,data= df.c)
# df.c$Pred <- predict(RegTerm, df.c)
# min(df.c$Pred)
# 
# 
# 
# # Pensions OR! Terminal consumption
# # p1$gamma0 <- RegPens$coefficients[1]
# # p1$gamma1 <- RegPens$coefficients[2]
# # p1$gamma2 <- RegPens$coefficients[3]
# # p1$gamma3 <- RegPens$coefficients[4]
# 
# p1$gamma0 <- RegTerm$coefficients[1]
# p1$gamma1 <- RegTerm$coefficients[2]
# p1$gamma2 <- RegTerm$coefficients[3]
# p1$gamma3 <- RegTerm$coefficients[4]
# p1$gamma4 <- RegTerm$coefficients[5]
