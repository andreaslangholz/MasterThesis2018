################################################################
# "Danish Labour Supply with Human Capital -                   #
# Evidence from a Structural Life-Cycle Model"                 #
# By: Andreas Als Langholz                                     #  
# Thesis for M.sc. in Economics 2018                           #
################################################################

################################################################
# File 7: Outouts of plots and tables                          #
################################################################

DATA.P <- DATA.FS
df.p <- DATA.P


#### Tables in latex format #############
xtable(outwh,digits  = c(rep(4,3*5+1)))
xtable(outjl,digits  = c(rep(4,3*5+1)))
xtable(outubp,digits = c(rep(4,3*5+1)))
xtable(outubf,digits = c(rep(4,3*5+1)))
xtable(outded,digits = c(rep(4,3*5+1)))
xtable(df.tt.out)
xtable(df.bi.out)

### Plots with ggplot2 ###############
df.p$Education <- ifelse(df.p$edu=="Ufaglærte", "Unskilled",
                       ifelse(df.p$edu=="Faglærte", "Skilled",
                              ifelse(df.p$edu=="KVU", "Short HE",
                                     ifelse(df.p$edu=="MVU", "Medium HE",
                                            ifelse(df.p$edu=="LVU", "Long HE", NA) ))))

# K bins
##### K histogram ################
ggplot(df.p) + geom_histogram(aes(x = k), color ="black", fill = "blue4", binwidth = 0.5)+
  ylab("Number of observations in bins") + 
  xlab("K bins") + 
  #scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8,9,10),labels = c(1,2,3,4,5,6,7,8,9,10)) + 
  theme_tufte()+
  theme(panel.border=element_rect(color = "black", fill=NA,size = 0.5))

##### Taxplot ######
id <- unique(df.tp$id)
id.sample <- sample(id, 20000, replace = FALSE)
df.tp.p <- df.tp[which(df.tp$id %in% id.sample),] 
df.tp.p<-df.tp.p[-which(df.tp.p$tax %in% tail(sort(df.tp.p$tax),250) | df.tp.p$taxinc %in% tail(sort(df.tp.p$taxinc),250))]
df.tp.p<-df.tp.p[-which(df.tp.p$taxinc > 1000000),]
df.tp.p<-df.tp.p[-which(df.tp.p$tax %in% tail(sort(df.tp.p$tax),50)),]

ggplot(data = df.tp.p,aes(x = taxinc, y = tax)) + 
  geom_point(size = 0.5) + 
 # geom_line( aes(y =  Logarithmic),color = "royalblue2", size = 1) +
  geom_line( aes(y =  Kinked),color = "red", size=1) +
  geom_rangeframe() +
  theme_tufte() + 
  geom_vline(aes(xintercept = px$kink1,color = "Kink 1"), linetype = "dashed")+
  geom_vline(aes(xintercept = px$kink2,color = "Kink 2"), linetype = "dashed")+
  ylab("Tax") + 
  xlab("Taxable Income") +
  scale_color_manual(name = "Kink Points", values = c("Kink 1" = "blue", "Kink 2" = "green"))

#+   theme(panel.border=element_rect(color = "black", fill=NA,size = 0.5))


# Consumption plots
df.fs <- df.p
id <- unique(df.fs$id)
id.sample <- sample(id, 10000, replace = FALSE)
df.fs <- df.fs[which(df.fs$id %in% id.sample),]

# scatterplot cons against wealth
ggplot(data = df.fs, aes(x=wealth, y = cons)) + geom_point(size = 0.4) + geom_smooth()

### Plot of mean consumption against wealth ####

# discretise wealth
num.points <- 200
cw <- seq(from = min(df.p$wealth), to = max(df.p$wealth) + 0.1, length.out = num.points + 1 )
cw <- UEplot(min(df.p$wealth),max(df.p$wealth)+1, num.points+1,1.5)
# Define groups (now vectorized)
cw.mat <- matrix(nrow = nrow(df.p), ncol = num.points)
for(k in 1:num.points){
  cw.mat[ ,k] <-  ifelse(df.p$wealth < cw[k + 1], k, 1000)
}

df.p$w.group  <- rowMins(cw.mat)
df.p$wealth <- cw[df.p$w.group]

wl <- df.p %>% 
  group_by(wealth,Education) %>%
  dplyr::summarise(meanc = mean(cons), meanh = mean(ht), meanw = mean(wealth), meanhour = mean(workh))

# PLot cosumption average
ggplot(wl, aes(x = wealth, y = meanc, group = Education, color = Education)) + 
  geom_line(size=1) + theme_tufte()+geom_rangeframe() + 
  facet_wrap(~Education, ncol =2, scales = "free_y") 




# without negative wealth
df.wp <- df.p[-which(df.p$wealth < 0),]

wl <- df.wp %>% 
  group_by(w.group,Education) %>%
  dplyr::summarise(meanc = mean(cons), meanh = mean(ht), meanw = mean(wealth), meanhour = mean(workh))

# PLot cosumption average
ggplot(wl, aes(x = w.group, y = meanc, group = Education, color = Education)) + 
  geom_line(size=1) + theme_tufte()+geom_rangeframe() + 
  facet_wrap(~Education, ncol =2, scales = "free_y") 


### AGEPLOTS ############

# Average consumttion and labour choice over lifecycle
df.c <- df.p[which(df.p$age < 68),]
df.c <- df.c[which(df.c$age > 22),]

pl <- df.c %>% 
  group_by(age,Education) %>%
  dplyr::summarise(meanc = mean(cons), meanh = mean(ht), meanw = mean(wealth), meanhour = mean(workh))

# PLot cosumption average
ggplot(pl, aes(x=age,y=meanc, group = Education, color = Education)) + 
 geom_line(size=1) + theme_tufte()+geom_rangeframe() + 
 facet_wrap(~Education, ncol =2, scales = "free_y") 

# Plot work choice average
ggplot(pl, aes(x=age,y=meanh, group = Education, color = Education)) + 
  geom_line(size=1) + theme_tufte()+geom_rangeframe()+ 
  facet_wrap(~Education, ncol =2, scales = "free_y") 

# Plot hours
ggplot(pl, aes(x=age,y=meanhour, group = Education, color = Education)) + 
  geom_line(size=1) + theme_tufte()+geom_rangeframe()+ 
  facet_wrap(~Education, ncol =2, scales = "free_y") 



# Same without negative wealth
df.cm <- df.c[-which(df.c$wealth < 0),]


plm <- df.cm %>% 
  group_by(age,Education) %>%
  dplyr::summarise(meanc = mean(cons), meanh = mean(ht), meanw = mean(wealth), meanhour = mean(workh))

# PLot cosumption average
ggplot(plm, aes(x=age,y=meanc, group = Education, color = Education)) + 
  geom_line(size=1) + theme_tufte()+geom_rangeframe()+ 
  facet_wrap(~Education, ncol =2, scales = "free_y") 

# Plot work choice average
ggplot(plm, aes(x=age,y=meanh, group = Education, color = Education)) + 
  geom_line(size=1) + theme_tufte()+geom_rangeframe()+ 
  facet_wrap(~Education, ncol =2, scales = "free_y") 

# Plot hours
ggplot(plmerge) +  
  geom_line(aes(x=age,y=meanhour.x, group = Education, color = Education),size=1) +
  geom_line(aes(x=age,y=meanhour.y, group = Education, color = Education),size=1) + 
  facet_wrap(~Education, ncol =2, scales = "free_y") 

  # theme_tufte()+geom_rangeframe()



# Comparison of work response

plm$Education <- ifelse(plm$Education=="Unskilled","Unskilled-NNW",
                         ifelse(plm$Education=="Skilled", "Skilled-NNW",
                                ifelse(plm$Education=="Short HE", "Short HE-NNW",
                                       ifelse(plm$Education=="Medium HE", "Medium HE-NNW",
                                              ifelse(plm$Education=="Long HE", "Long HE-NNW", NA) ))))


pl$Wealth <- "Normal"
plm$Wealth <- "No Negative Wealth"

plmerge <- rbind(pl,plm)

ggplot(plmerge, aes(x=age,y=meanhour, group = interaction(Education,Wealth),color = Education) )+ 
  geom_line(aes(linetype=Wealth),size=1) + theme_tufte()+geom_rangeframe()


ggplot(plmerge, aes(x=age,y=meanc, group = interaction(Education,Wealth),color = Education) )+ 
  geom_line(aes(linetype=Wealth),size=1) + theme_tufte()+geom_rangeframe()



#####################################################
#### First Stage ###################################3
####################################################
#### TAXES ##################

##### Taxplot ######
id <- unique(df.tp$id)
id.sample <- sample(id, 50000, replace = FALSE)
df.tp.p <- df.tp[which(df.tp$id %in% id.sample),] 

df.tp.p <-df.tp.p[-which(df.tp.p$tax %in% tail(sort(df.tp.p$tax),250) | df.tp.p$taxinc %in% tail(sort(df.tp.p$taxinc),250))]
df.tp.p <-df.tp.p[-which(df.tp.p$taxinc > 1000000),]
df.tp.p <-df.tp.p[-which(df.tp.p$tax %in% tail(sort(df.tp.p$tax),50)),]

ggplot(data = df.tp.p,aes(x = taxinc, y = tax)) + 
  geom_point(size = 0.5) + 
  geom_line( aes(y =  Kinked, color = "red4"), size=1)  +
  geom_rangeframe() +
  theme_tufte() + 
  ylab("Tax") + 
  xlab("Taxable Income") +  geom_line( aes(y =  Logarithmic,color = "blue2"), size = .5)+
  scale_color_discrete(name = "Model", labels = c("Logarithmic","Kinked")) 
# +  theme(panel.border=element_rect(color = "black", fill=NA,size = 0.5))


#### WAGES #######################
df.plot <- DATA.FS
summary(df.plot)
df.p <- df.plot[which(df.plot$ht > 1 ),]

wf <- data.frame("wage"  = df.p$wageh,
                 "K"     = df.p$k,
                 "KSq"   = df.p$k ^ 2,
                 "Kt" = df.p$k ^3,
                 "Age"   = df.p$age,
                 "AgeSq" = df.p$age ^ 2,
                 "Aget" = df.p$age ^3,
                 "work"  = df.p$ht,
                 "edu" = df.p$edu)




wf$edu <- ifelse(wf$edu=="Ufaglærte", "Unskilled",
                   ifelse(wf$edu=="Faglærte", "Skilled",
                          ifelse(wf$edu=="KVU", "Short HE",
                                 ifelse(wf$edu=="MVU", "Medium HE",
                                        ifelse(wf$edu=="LVU", "Long HE", NA) ))))

edulevels <- unique(wf$edu)

for(edu in edulevels){
 print(edu) 
  wt <- wf[which(wf$edu == edu & wf$K >1 & wf$Age < 66 & wf$Age >24),]
  #WFLog  <- lm(log(wage)~  K + KSq  + Age + AgeSq + Aget, data = wt)
  
  WFnorm <- lm(wage~  K + KSq  + Age + AgeSq + Aget  + Age*K, data = wt)
  wt$pwnorm <- predict(WFnorm, wt)
  
  wt1 <- data.frame("wage" = wt$wage, "K" = wt$K, "Age" = wt$Age)
  wt1$Education = edu
  wt1$Type = "Data"
  
  
  wt3<- data.frame("wage" = wt$pwnorm, "K" = wt$K, "Age" = wt$Age)
  wt3$Education = edu
  wt3$Type = "Model"
  
  wo <- rbind(wt1,wt3)
  
  if(edu == edulevels[1]){
    wout2 <- wo
  } else {
    wout2 <- rbind(wout2,wo)
  }
}

wqt <- wout2 %>% 
  group_by(Age,Type,Education) %>%
  dplyr::summarise(meanw = mean(wage))
wqt <- wqt[which(wqt$Age <65),]

ggplot(wqt, aes(x=Age,y=meanw, group = interaction(Education,Type),color = Education) )+ 
  geom_line(aes(linetype=Type),size=0.5) + theme_tufte()+geom_rangeframe() + 
  facet_wrap(~Education, ncol =2, scales = "free_y") + 
  ylab("Mean Hourly Wage") + 
  xlab("Age")  

wqt <- wout2 %>% 
  group_by(K,Type,Education) %>%
  dplyr::summarise(meanw = mean(wage))

ggplot(wqt, aes(x=K,y=meanw, group = interaction(Education,Type),color = Education) )+ 
  geom_line(aes(linetype=Type),size=0.5) + theme_tufte()+geom_rangeframe() + 
  facet_wrap(~Education, ncol =2, scales = "free_y")+
  ylab("Mean Hourly Wage") + 
  xlab("Human Capital (K)")  




####### UNEMPLOYMENT ########

length(is.na(DATA.FS$wealth))

DATA.EDU <- DATA.FS
summary(DATA.EDU)
df.unem <- data.frame("UB"    = DATA.EDU$unemb,
                      "A"     = DATA.EDU$wealth,
                      "Asq"   = DATA.EDU$wealth ^ 2,
                      "K"     = DATA.EDU$k,
                      "KSq"   = DATA.EDU$k ^2,
                      "Age"   = DATA.EDU$age,
                      "LogAge" = log(DATA.EDU$age),
                      "AgeSq" = DATA.EDU$age ^ 2,
                      "Aget"  = DATA.EDU$age ^3,
                      "ht"    = DATA.EDU$ht,
                      "edu" = DATA.EDU$edu,
                      "ul" = DATA.EDU$ul,
                      "wage" = DATA.EDU$wageh)

df.u<-df.unem
df.u$edu <- ifelse(df.u$edu=="Ufaglærte", "Unskilled",
                 ifelse(df.u$edu=="Faglærte", "Skilled",
                        ifelse(df.u$edu=="KVU", "Short HE",
                               ifelse(df.u$edu=="MVU", "Medium HE",
                                      ifelse(df.u$edu=="LVU", "Long HE", NA) ))))


edulevels <- unique(df.u$edu)


for(edu in edulevels){
  # Wage
  minage <- ifelse(edu !="LVU", 20, 22)
  wt <- wf[which(wf$edu == edu & wf$K >1 & wf$Age < 66 & wf$Age >24),]
  WFnorm <- lm(wage~  K + KSq  + Age + AgeSq + Aget, data = wt)
  #Unem
  wtf <- df.u[which(df.u$edu == edu & df.u$Age < 65 & df.u$Age >=minage),]
  wtf <- wtf[which(wtf$ht == 1),]
  
  # Predict fulltime wage
  wtf$predw <- predict(WFnorm,wtf) * ((Hours(4) + Hours(5)) / 2) 
  wtf$predwsq <- wtf$predw ^ 2
  #Construct parameters
  #wtf$t1 <-  wtf$predw * (px$tau * ifelse(wtf$predw >= px$UBf, wtf$predw,0))
  
  ubf <- lm(UB ~  LogAge + Age + predw + predwsq, data = wtf)
  wtf$pub <- predict(ubf, wtf)
  
  wt1 <- data.frame("UB" = wtf$UB, "K" = wtf$K, "A" = wtf$A, "ul" = wtf$ul, "Age" = wtf$Age, "predw" = wtf$predw)
  wt1$Education = edu
  wt1$Type = "Data"
  wt1$emp = "unemployed"

  wt3 <- data.frame("UB" = wtf$pub, "K" = wtf$K,"A" = wtf$A,"ul" = wtf$ul, "Age" = wtf$Age, "predw" = wtf$predw)
  wt3$Education = edu
  wt3$Type = "Model"
  wt3$emp = "unemployed"
  
  wo <- rbind(wt1,wt3)
  
  if(edu == edulevels[1]){
    woutf <- wo
  } else {
    woutf <- rbind(woutf,wo)
  }
  
  # Do the ting agains with part time employed
  wtp <- df.u[which(df.u$edu == edu & df.u$Age < 66 & df.u$Age >24),]
  wtp <- wtp[which(wtp$ht %in% part.t),]
  
  # Predict fulltime wage
  wtp$predw <- predict(WFnorm,wtp) * ((Hours(2) + Hours(3)) / 2) 
  wtp$predwsq <- wtp$predw^2
  #Construct parameters
  #wtp$t1 <-  wtp$predw * (px$tau * ifelse(wtp$predw >= px$UBp, wtp$predw,0))
  
  ubp <- lm(UB ~ LogAge + Age + predw +  predwsq, data = wtp)
  wtp$pub <- predict(ubp, wtp)
  
  wtp1 <- data.frame("UB" = wtp$UB, "A" = wtp$A, "Age" = wtp$Age, "predw" = wtp$predw)
  wtp1$Education = edu
  wtp1$Type = "Data"
  wtp1$emp = "parttime"
  
  wtp3 <- data.frame("UB" = wtp$pub, "A" = wtp$A, "Age" = wtp$Age, "predw" = wtp$predw)
  wtp3$Education = edu
  wtp3$Type = "Model"
  wtp3$emp = "parttime"
  
  wop <- rbind(wtp1,wtp3)
  
  if(edu == edulevels[1]){
    woutp <- wop
  } else {
    woutp <- rbind(woutp,wop)
  }

}

#### Discretize###
num.points <- 100
cw.a <- seq(min(woutf$A), max(woutf$A) + 0.1, length.out = num.points + 1 )
cw.w <- seq(min(woutf$predw), max(woutf$predw) + 0.1, length.out = num.points + 1 )
cw.w <- UnEgridPlot(min(woutf$predw), max(woutf$predw)+0.1, n = num.points+1,1.4)
cw.a <- UnEgridPlot(min(woutf$A), max(woutf$A)+0.1, n = num.points+1,1.4)


cw.mat.a <- matrix(nrow = nrow(woutf), ncol = num.points)
cw.mat.w <- matrix(nrow = nrow(woutf), ncol = num.points)
for(k in 1:num.points){
  cw.mat.a[ ,k] <-  ifelse(woutf$A < cw.a[k + 1], k, 1000)
  cw.mat.w[ ,k] <-  ifelse(woutf$predw < cw.w[k + 1], k, 1000)
  
}
woutf$a.group  <- rowMins(cw.mat.a)
woutf$w.group  <- rowMins(cw.mat.w)
##
num.points <- 100
cw.a <- seq(min(woutp$A), max(woutp$A) + 0.1, length.out = num.points + 1 )
cw.w <- seq(min(woutp$predw), max(woutp$predw) + 0.1, length.out = num.points + 1 )
cw.w <- UnEgridPlot(min(woutp$predw), max(woutp$predw)+0.1, n = num.points+1,1.3)
cw.a <- UnEgridPlot(min(woutp$A), max(woutp$A)+0.1, n = num.points+1,1.3)


cw.mat.a <- matrix(nrow = nrow(woutp), ncol = num.points)
cw.mat.w <- matrix(nrow = nrow(woutp), ncol = num.points)
for(k in 1:num.points){
  cw.mat.a[ ,k] <-  ifelse(woutp$A < cw.a[k + 1], k, 1000)
  cw.mat.w[ ,k] <-  ifelse(woutp$predw < cw.w[k + 1], k, 1000)
  
}
woutp$a.group  <- rowMins(cw.mat.a)
woutp$w.group  <- rowMins(cw.mat.w)

summary(woutf)
 
# UB:Age plots
wqt <- woutf%>% 
  group_by(Education,Type,Age) %>%
  dplyr::summarise(meanub = mean(UB), wealth = mean(A))

ggplot(wqt, aes(x=Age,y=meanub,color = Education) )+ 
  geom_line(aes(linetype=Type),size=0.5)+
  theme_tufte()+geom_rangeframe() +  
  facet_wrap(~Education, ncol =2, scales = "free")+
ylab("Mean Unemployment Benefits") + 
  xlab("Age")  
  
wqt <- woutp%>% 
  group_by(Education,Type,Age) %>%
  dplyr::summarise(meanub = mean(UB), wealth = mean(A))

ggplot(wqt, aes(x=Age,y=meanub,color = Education) )+ 
  geom_line(aes(linetype=Type),size=0.5)+ 
  theme_tufte()+geom_rangeframe() +
  facet_wrap(~Education, ncol =2, scales = "free") + 
  ylab("Mean Unemployment Benefits") + 
  xlab("Age")  

# UB:Wages
wqt <- woutf%>% 
  group_by(Education,Type,w.group) %>%
  dplyr::summarise(meanub = mean(UB), wealth = mean(A))

ggplot(wqt, aes(x=w.group,y=meanub,color = Education) )+ 
  geom_line(aes(linetype=Type),size=0.5)+
  theme_tufte()+geom_rangeframe() +  
  facet_wrap(~Education, ncol =2, scales = "free") + 
  ylab("Mean Unemployment Benefits") + 
  xlab("Normalized Wages")  


wqt <- woutp%>% 
  group_by(Education,Type,w.group) %>%
  dplyr::summarise(meanub = mean(UB), wealth = mean(A))

ggplot(wqt, aes(x=w.group,y=meanub,color = Education) )+ 
  geom_line(aes(linetype=Type),size=0.5)+ 
  theme_tufte()+geom_rangeframe() +
  facet_wrap(~Education, ncol =2, scales = "free") + 
ylab("Mean Unemployment Benefits") + 
  xlab("Normalized Wages")  


### Fuill dataset 
summary(df.u[which(df.u$ht %in% part.t),])
#### Discretize 
num.points <- 100
cw.a <- seq(min(df.u$A), max(df.u$A) + 0.1, length.out = num.points + 1 )
cw.w <- seq(min(df.u$wage), max(df.u$wage) + 0.1, length.out = num.points + 1 )
#cw.w <- UnEgridPlot(min(df.u$predw), max(df.u$predw)+0.1, n = num.points+1,1.3)
#cw.a <- UnEgridPlot(min(df.u$A), max(df.u$A)+0.1, n = num.points+1,1.3)


cw.mat.a <- matrix(nrow = nrow(df.u), ncol = num.points)
cw.mat.w <- matrix(nrow = nrow(df.u), ncol = num.points)
for(k in 1:num.points){
  cw.mat.a[ ,k] <-  ifelse(df.u$A < cw.a[k + 1], k, 1000)
  cw.mat.w[ ,k] <-  ifelse(df.u$wage < cw.w[k + 1], k, 1000)
  
}
df.u$a.group  <- rowMins(cw.mat.a)
df.u$w.group  <- rowMins(cw.mat.w)

wqt <- df.u %>% 
  group_by(edu,type,a.group) %>%
  dplyr::summarise(meanub = mean(UB), wealth = mean(A))

ggplot(wqt, aes(x=a.group,y=meanub,color = edu) )+ 
  geom_line(aes(linetype=type),size=1)+
  theme_tufte()+geom_rangeframe() +
  facet_wrap(~edu, ncol =2, scales = "free")




################ DEDUCTIONS #################
DATA.EDU <- DATA.FS
df.d <- data.frame("D" = DATA.EDU$deduct,
                   "age" = DATA.EDU$age,
                  "A"     = DATA.EDU$wealth,
                  "Asq"   = DATA.EDU$wealth ^ 2,
                  "At"     = DATA.EDU$wealth^3,
                  "W"     = DATA.EDU$wageh,
                  "LogA" = log(DATA.EDU$wealth),
                  "Wsq"   = DATA.EDU$wageh^2,
                  "Wt"   = DATA.EDU$wageh^3,
                  "edu" = DATA.EDU$edu)
df.d$Edu <- ifelse(df.d$edu=="Ufaglærte", "Unskilled",
            ifelse(df.d$edu=="Faglærte", "Skilled",
            ifelse(df.d$edu=="KVU", "Short HE",
            ifelse(df.d$edu=="MVU", "Medium HE",
            ifelse(df.d$edu=="LVU", "Long HE", NA) ))))


num.points <- 100
cw.w <- UnEgridPlot(min(df.d$W), max(df.d$W)+0.1, n = num.points+1,1.3)
cw.a <- UnEgridPlot(min(df.d$A), max(df.d$A)+0.1, n = num.points+1,1.3)

cw.w <- seq(min(df.d$W), max(df.d$W)+0.1, length.out = num.points+1)
cw.a <- seq(min(df.d$A), max(df.d$A)+0.1, length.out = num.points+1)

cw.mat.a <- matrix(nrow = nrow(df.d), ncol = num.points)
cw.mat.w <- matrix(nrow = nrow(df.d), ncol = num.points)
for(k in 1:num.points){
  cw.mat.a[ ,k] <-  ifelse(df.d$A < cw.a[k + 1], k, 1000)
  cw.mat.w[ ,k] <-  ifelse(df.d$W < cw.w[k + 1], k, 1000)
}

df.d$a.group  <- rowMins(cw.mat.a)
df.d$w.group  <- rowMins(cw.mat.w)

edulevels <- unique(df.d$Edu)
edulevels
for(edu in edulevels){
  
  wt <- df.d[which(df.d$Edu == edu & df.d$age < 66 & df.d$age >24),]
  
  WFnorm <- lm(D~ LogA + W + Wsq + Wt, data = wt)
  wt$pwnorm <- predict(WFnorm, wt)
  
  wt1 <- data.frame("ded" = wt$D, "A" = wt$A, "W" = wt$W, "Ag" = wt$a.group, "Wg" = wt$w.group)
  wt1$Education = edu
  wt1$Type = "Data"
  
  
  wt3<- data.frame("ded" = wt$pwnorm, "A" = wt$A, "W" = wt$W, "Ag" = wt$a.group, "Wg" = wt$w.group)
  wt3$Education = edu
  wt3$Type = "Model"
  
  wo <- rbind(wt1,wt3)
  
  if(edu == edulevels[1]){
    wout2 <- wo
  } else {
    wout2 <- rbind(wout2,wo)
  }
}


# Ded:Wages

wqt <- wout2 %>% 
  group_by(Wg,Type,Education) %>%
  dplyr::summarise(meand = mean(ded))

ggplot(wqt, aes(x=Wg,y=meand, group = interaction(Education,Type),color = Education) )+ 
  geom_line(aes(linetype=Type),size=0.5) + theme_tufte()+geom_rangeframe() +
  facet_wrap(~Education, ncol =2, scales = "free_y") +
  ylab("Mean Deductions") + 
  xlab("Normalized Wage")  

# Ded:Assets
wqt <- wout2 %>% 
  group_by(Ag,Type,Education) %>%
  dplyr::summarise(meand = mean(ded))
ggplot(wqt, aes(x=Ag,y=meand, group = interaction(Education,Type),color = Education) )+ 
  geom_line(aes(linetype=Type),size=0.5) + theme_tufte()+geom_rangeframe() + 
  facet_wrap(~Education, ncol =2, scales = "free_y") +
  ylab("Mean Deductions") + 
  xlab("Normalized Assets")  



#########################################
 ### ELASTICITY PLOTS #################
############################################

df.el.out$Education = ifelse(df.el.out$Education == "LVU", "Long HE",
                             ifelse(df.el.out$Education == "MVU", "Medium HE",
                                    ifelse(df.el.out$Education == "KVU", "Short HE",
                                           ifelse(df.el.out$Education == "Faglærte", "Skilled",
                                                  ifelse(df.el.out$Education == "Ufaglærte", "Unskilled",NA)))))


ggplot(df.el.out, aes(x=Age,y=el.s,group = interaction(Education,Type), color = Education)) + 
  geom_line(size=0.5) +
  geom_point(aes(shape = Education),size=0.5) +
  theme_minimal()+  
  facet_wrap(~Type, ncol=3, scales="free")  +
  ylab("Elasticities") +
  xlab("Age")

ggplot(df.el.out, aes(x=Age,y=el,group = interaction(Education,Type), color = Education)) + 
  geom_line() +
  geom_point(aes(shape = Education)) +
  theme_minimal()+  
  facet_wrap(~Type, ncol=1)  +
  xlab("Elasticities") +
  ylab("Age")
