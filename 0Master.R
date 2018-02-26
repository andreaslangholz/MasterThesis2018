
################################################################
# "Danish Labour Supply with Human Capital -                   #
# Evidence from a Structural Life-Cycle Model"                 #
# By: Andreas Als Langholz                                     #  
# Thesis for M.sc. in Economics 2018                           #
################################################################

# Empty all working directory
rm(list = ls())

# Set new working directory
setwd("Insert_own_WD")

# Load data
DATA.raw <- read.csv("\\datafinal.csv")

# Load libraries
packages <- c("dplyr", "plyr","tidyr","stringr","ggplot2","matrixStats", "parallel",
              "devtools","pracma", "Hmisc","foreach","doParallel","maxLik",
              "data.table", "statar", "plotly","magrittr", "qlcMatrix", "MatrixStats"
              "xtable","ggthemes", "rlist" , "qlcMatrix")

lapply(packages, require, character.only = TRUE)

# Setup parallel computing clusters
num.cores <- 4
cl <- makeCluster(num.cores)
registerDoParallel(cl)

##########################################
############ Model #######################
##########################################

#### 1. Functions #####
# Description:
# Loads all functions used in the model 
source("1Functions.R")

#### 2. Parameters #####
# Description:
# Load Initial exogenous parameters
source("2Parameters.R")

#### 3. Data #####
# Description:
# Prepares additional variables used in the model and
# construct markov transition matrices
source("3Data.R")

#### 4. First step #####
# Description:
# Runs the first step estimations over the education groups
# outputs the parameterset for to use as input in MLE
source("4FirstStep.R")

#### 5. Maximum likelihood ##### 
# Description: 
# Estimates structural parameter values by the NFXP MLE estimator
# with an outer BHHH hill-climbing algorithm
# OBS: Due to the long estimation time, each education group
# should be declared manually before running the script
source("4Estimation.R")

#### 6. Simulation ##### 
# Description: 
# Simulates the estimates of elasticities and the tax/basic income experiments
source("6Simulations.R")

#### 7. Outputs ##### 
# Description: 
# Outputs graphs and tables for use in paper
source("7Outputs.R")
