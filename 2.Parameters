################################################################
# "Danish Labour Supply with Human Capital -                   #
# Evidence from a Structural Life-Cycle Model"                 #
# By: Andreas Als Langholz                                     #  
# Thesis for M.sc. in Economics 2018                           #
################################################################

################################################################
# File 2: Exogenous parameters                                 #
################################################################

### Exogenous parameters  ####
px <- data.frame(1)

# Time
px$t.min       <- 25
px$t.max       <- 65
px$T           <- px$t.max - px$t.min + 1

# Discrete choices
px$num.h <- 6        # Labour supply choices
full.t   <- c(4,5,6) # Set of discrete choices which corresponds to working full time (Free object, cannot be in dataframe format)
part.t   <- c(2,3)   # Set of discrete choices which corresponds to working part time

# Grids
px$a.min  <- 1       #
px$a.max  <- 2700000 #
px$num.a  <- 400     # Number of asset points 
px$num.k  <- 10      # number of discrete groups of human capital
px$uneq   <- 1.2
px$cons   <- 40

# Structural exogenous parameters
px$phi          <- 100000  # Bequest paraameter
px$R            <- 0.05    # Rent
px$beta         <- 0.95    # Discounting factor
px$inflation    <- 0.02    # Inflation rate

# Taxation kinks
px$kink1 <- (177900 + 191200 / (1 + px$inflation) ^ 1 + 
               198000 / (1 + px$inflation) ^ 2 + 254000 / (1 + px$inflation) ^ 3 +
               259500 / (1 + px$inflation) ^ 4 + 265500 / (1 + px$inflation) ^ 5 +
               272600 / (1 + px$inflation) ^ 6 + 279800 / (1 + px$inflation) ^ 7) / 8

px$kink2 <- (276900 + 285200 / (1 + px$inflation) ^ 1 + 
               296300 / (1 + px$inflation) ^ 2 + 304800 / (1 + px$inflation) ^ 3 +
               311500 / (1 + px$inflation) ^ 4 + 318700 / (1 + px$inflation) ^ 5 +
               327200 / (1 + px$inflation) ^ 6 + 335800 / (1 + px$inflation) ^ 7 +
               347200 / (1 + px$inflation) ^ 8) / 9 

# Number of simulated agents
px$num.sim <- 10000 
px$sigma.taste <- 1
# Scale parameter for the 
px$scale        <- 1
px <- px[,-1]
