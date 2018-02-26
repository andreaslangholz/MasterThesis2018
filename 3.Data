################################################################
# "Danish Labour Supply with Human Capital -                   #
# Evidence from a Structural Life-Cycle Model"                 #
# By: Andreas Als Langholz                                     #  
# Thesis for M.sc. in Economics 2018                           #
################################################################

################################################################
# File 3: Data Preparation and Markov Chain Computations       #
################################################################
# Make a subset
df.fs <- DATA.raw

# Convert to numeric variables
df.fs$exp_tot      <- as.numeric(as.character(df.fs$exp_tot))
df.fs$experience   <- as.numeric(as.character(df.fs$experience))
df.fs$tot_norm     <- as.numeric(as.character(df.fs$tot_norm))
df.fs$tot_time_job <- as.numeric(as.character(df.fs$tot_time_job))
df.fs$wage         <- as.numeric(as.character(df.fs$wage))
df.fs$typejob      <- as.numeric(as.character(df.fs$typejob))
df.fs$tax          <- as.numeric(as.character(df.fs$Tax))
df.fs$taxinc       <- as.numeric(as.character(df.fs$taxinc))
df.fs$deduction    <- as.numeric(as.character(df.fs$deductions))
df.fs$wealth       <- as.numeric(as.character(df.fs$Wealth))
df.fs$unemb        <- as.numeric(as.character(df.fs$UnemB))
df.fs$age          <- as.numeric(as.character(df.fs$age))
df.fs$grossinc     <- as.numeric(as.character(df.fs$grossinc))
df.fs$term         <- as.numeric(as.character(df.fs$Ledighedsårsag..DUR.))
df.fs$ID_nr        <- as.numeric(as.character(df.fs$ID_nr))

############################################
# Deduct consumption from income and taxes #
############################################

# Sort for wage eaner and UI reciepients
df.fs <- df.fs[which(df.fs$typejob %in% c(4, 5, 10, 11, 12)),]

# Wealth and consumption
df.cons <- data.frame("wealth"     = c(df.fs$wealth, 0), 
                      "wealthlast" = c(0, df.fs$wealth),
                      "ID"         = c(df.fs$ID_nr, 0),
                      "IDlast"     = c(0, df.fs$ID_nr),
                      "year"       = c(df.fs$year, 0),
                      "yearlast"   = c(0, df.fs$year),
                      "taxinc"     = c(df.fs$taxinc, 0),
                      "deduction"  = c(df.fs$deduction, 0),
                      "tax"        = c(df.fs$tax, 0),
                      "unemb"      = c(df.fs$UnemB,0),
                      "age"        = c(df.fs$age, 0),
                      "wage"       = c(df.fs$wage, 0),
                      "term"       = c(df.fs$term,0),
                      "gi"         = c(df.fs$grossinc,0))


df.cons$dwealth  <- df.cons$wealth - df.cons$wealthlast
df.cons$dyear    <- df.cons$year - df.cons$yearlast
df.cons$dwextrap <- ifelse(df.cons$ID == df.cons$IDlast, df.cons$dwealth / df.cons$dyear,0)  #  + df.cons$taxinc + df.cons$deductions - df.cons$tax,0)
df.cons$cons     <- df.cons$gi - df.cons$tax - df.cons$dwextrap

df.cons <- df.cons[-1,]
df.cons <- df.cons[-nrow(df.cons),]

####################################
# Labour choices And Human Capital #
####################################

# Cumulate experience counters
df.exp <- data.frame("exp.c"    = c(df.fs$experience, 0), 
                     "exp.s"    = c(df.fs$exp_tot, 0),
                     "ID"       = c(df.fs$ID_nr, 0),
                     "IDlast"   = c(0,df.fs$ID_nr),
                     "year"     = c(df.fs$year, 0),
                     "yearlast" = c(0,df.fs$year),
                     "tot_norm" = c(df.fs$tot_norm,0),
                     "term"     = c(df.fs$term,0),
                     "age"      = c(df.fs$age,0))

df.exp$deltaY <- df.exp$year - df.exp$yearlast
df.exp$exp.c  <- ifelse(df.exp$ID == df.exp$IDlast, df.exp$exp.c * df.exp$deltaY, df.exp$exp.c) 
df.exp$exp    <- ifelse(df.exp$year == 2001, df.exp$exp.s, df.exp$exp.c)
setDT(df.exp)[, expcum:=cumsum(exp),ID]
df.exp <- df.exp[-nrow(df.exp),]

# Define labour choice (now vectorized)
ht.mat <- matrix(nrow = nrow(df.exp), ncol = px$num.h) 
ht.mat[,1] <-  ifelse(df.exp$tot_norm <= 250, 1, 10)
ht.mat[,2] <-  ifelse(df.exp$tot_norm <= 750, 2, 10)
ht.mat[,3] <-  ifelse(df.exp$tot_norm <= 1250, 3, 10)
ht.mat[,4] <-  ifelse(df.exp$tot_norm <= 1750, 4, 10)
ht.mat[,5] <-  ifelse(df.exp$tot_norm <= 2250, 5, 10)
ht.mat[,6] <-  6
df.exp$ht  <- rowMins(ht.mat[,1:6])

# Part time and unemployed last period
df.exp$htlast <- c(0, df.exp$ht[-nrow(df.exp)])
df.exp$ul <- ifelse(df.exp$htlast == 1, 1, 0)
df.exp$ul <- ifelse(is.na(df.exp$htlast), ifelse(df.exp$ht == 1,1,0),0)
df.exp$pl <- ifelse(df.exp$htlast %in% part.t, 1, 0)

# Human capital discretization ###################
df.exp <- df.exp[-which(is.na(df.exp$ht) | is.na(df.exp$expcum) |
                          df.exp$expcum >= quantile(df.exp$expcum,0.99, na.rm = TRUE)),]

# Cutoff points discretization of groups
cutoff <- seq(min(df.exp$expcum), max(df.exp$expcum) + 0.1, length.out = px$num.k + 1 )

# Construct vectof for average group expected experience
c.mat <- matrix(nrow = length(cutoff)+1,ncol = 3 )
c.mat[,1] <-c(0,cutoff)
c.mat[,2] <-c(cutoff,0)
c.mat[,3] <-rowMeans(c.mat[,1:2]) 

AvgExp <- c.mat[-c(1,nrow(c.mat)),3]

# Define groups (now vectorized)
k.mat <- matrix(nrow = nrow(df.exp), ncol = px$num.k)
for(k in 1:px$num.k){
  
  k.mat[ ,k] <-  ifelse(df.exp$expcum < cutoff[k + 1], k, 100)
  
}

df.exp$k  <- rowMins(k.mat)

# Joboffer

df.exp$term2 <- ifelse(is.na(df.exp$term), 0 , df.exp$term)
job.mat <- matrix(nrow = nrow(df.exp), ncol = 3)
job.mat[,1] <- ifelse(df.exp$ht > 1, 1,0)
job.mat[,2] <- ifelse(df.exp$term2 == 3, 1,0)
job.mat[,3] <- rowSums(job.mat[,c(1,2)])
df.exp$joboffer <- ifelse(rowSums(job.mat[,c(1,2)]) > 0,1,0)

# Cut out obs used for 
df.exp <- df.exp[-1,]
df.exp <- df.exp[-nrow(df.exp),]

##############################
# Markov Transition matrices #
##############################

# Define next period group for each observation
df.markov <- data.frame("k"      = c(0,df.exp$k), 
                        "knext"  = c(df.exp$k,0),
                        "ht"     = c(0,df.exp$ht),
                        "ID"     = c(0,df.exp$ID),
                        "IDnext" = c(df.exp$ID,0),
                        "year"   = c(0,df.exp$year))

# Drop observations with no next period 
df.markov <- df.markov[-1,]
df.markov <- df.markov[-which(df.markov$ID != df.markov$IDnext),]

# Storage for solution
K.Markov <- list(matrix(nrow = px$num.k, ncol = px$num.k),
                 matrix(nrow = px$num.k, ncol = px$num.k),
                 matrix(nrow = px$num.k, ncol = px$num.k),
                 matrix(nrow = px$num.k, ncol = px$num.k),
                 matrix(nrow = px$num.k, ncol = px$num.k),
                 matrix(nrow = px$num.k, ncol = px$num.k))

# Construct List of Markov chains
for(h in 1:px$num.h){
  
  Hset <- df.markov[which(df.markov$ht == h),]
  
  for(j in 1:px$num.k){
    
    subset.j <- Hset[which(Hset$k == j),]
    
    n.j <- nrow(subset.j)
    
    for(k in 1:px$num.k){
      
      n.k <- nrow(subset.j[which(subset.j$knext == k),])
      
      K.Markov[[h]][j,k] <- n.k / n.j
      
    }
  }
}

##################
# Final Data set #
##################
df.edu = data.frame("edu" = df.fs$edu_segment, "ID" = df.fs$ID_nr, "year" = df.fs$year)
df.final <- merge(x= df.cons, y = df.exp, by = c("ID","year"))
df.final <- merge(x= df.final, y = df.edu, by = c("ID","year"), all.x = TRUE)

# Hourly wage, contracted
df.final$wageh <- df.final$wage / df.final$tot_norm

# Discount by inflation
min.year <-min(df.final$year)
df.final$delta.year <- df.final$year - min.year 

df.final$cons      <- df.final$cons      / ((1 + px$inflation) ^ df.final$delta.year) 
df.final$wealth    <- df.final$wealth    / ((1 + px$inflation) ^ df.final$delta.year) 
df.final$taxinc    <- df.final$taxinc    / ((1 + px$inflation) ^ df.final$delta.year) 
df.final$wageh     <- df.final$wageh     / ((1 + px$inflation) ^ df.final$delta.year) 
df.final$tax       <- df.final$tax       / ((1 + px$inflation) ^ df.final$delta.year) 
df.final$deduction <- df.final$deduction / ((1 + px$inflation) ^ df.final$delta.year) 
df.final$unemb     <- df.final$unemb     / ((1 + px$inflation) ^ df.final$delta.year) 

# Truncate dataset
df.final <- df.final[which(df.final$wageh > quantile(df.final$wageh, 0.01, na.rm = TRUE)),]
df.final <- df.final[which(df.final$wageh < quantile(df.final$wageh, 0.99, na.rm = TRUE)),]
df.final <- df.final[which(df.final$wealth > quantile(df.final$wealth, 0.01, na.rm = TRUE)),]
df.final <- df.final[which(df.final$wealth < quantile(df.final$wealth, 0.99, na.rm = TRUE)),]
df.final <- df.final[-which(df.final$cons < 0| is.na(df.final$cons)),]
df.final <- df.final[-which(df.final$deduction < 0|df.final$unemb<0),]
df.final <- df.final[which(df.final$cons > quantile(df.final$cons, 0.01, na.rm = TRUE)),]
df.final <- df.final[which(df.final$cons < quantile(df.final$cons, 0.99, na.rm = TRUE)),]
df.final <- df.final[-which(df.final$edu == ""),]
df.final <- droplevels(df.final)

DATA.FS <- data.frame("id"     = df.final$ID,
                        "age"    = df.final$age.x,
                        "year"   = df.final$year,
                        "edu"    = df.final$edu,
                        "ht"     = df.final$ht,
                        "pl"     = df.final$pl,
                        "ul"     = df.final$ul,
                        "joboffer" = df.final$joboffer,
                        "k"      = df.final$k,
                        "cons"   = df.final$cons,
                        "wealth" = df.final$wealth,
                        "unemb"  = df.final$unemb,
                        "wageh"  = df.final$wageh,
                        "deduct" = df.final$deduction,
                        "tax"    = df.final$tax,
                        "taxinc" = df.final$taxinc,
                        "workh"  = df.final$tot_norm)
