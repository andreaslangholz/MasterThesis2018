################################################################
# "Danish Labour Supply with Human Capital -                   #
# Evidence from a Structural Life-Cycle Model"                 #
# By: Andreas Als Langholz                                     #  
# Thesis for M.sc. in Economics 2018                           #
################################################################

################################################################
# File 5: Maximum likelihood Estimation                        #
################################################################

# OBS OBS OBS! Runs manually
# Define which education group to estimation manually from 
# LVU, MVU, KVU, Skilled, Unskilled

eduest <- "LVU"
df.est <- DATA.E[which(DATA.E$edu==eduest ),]
p1 <- p1out[which(p1out$edux==eduest),]

# Clear data
df.est <- df.est[which(df.est$age >= px$t.min),]
df.est <- df.est[which(df.est$age <= px$t.max),]
df.est$t <- df.est$age - px$t.min + 1

# If using random sample:
# 
# id <- unique(df.est$id)
# idx <- sample(id, 10000, replace = FALSE)
# df.est <- df.est[which(df.est$id %in% idx),]

# Optimizer controls
cl = list(iterlim = 400) # Number of iterations

# If MLE has been run before
# pm <- MLE$estimate

tic <- proc.time()
  MLE <- maxLik(LLObs, start = pm, method = "bhhh", control=cl)
toc <- proc.time() - tic

# Output summary
summary(MLE)
