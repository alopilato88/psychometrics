# Load packages
library(dplyr)
library(tibble)
library(mvtnorm)
library(bootnet)

### --- Generating GGM from partial correlation matrix --- ###

# Set the number of variables 
n_var <- 25 # 25 variables -- 5 groups of 5

# Create population partial correlation matrix for one 5 x 5 then use for remaining
# clusters -- makes things simple

pc_21 <- .25
pc_31 <- .20
pc_32 <- .10
pc_41 <- .20
pc_42 <- .20
pc_43 <- .20
pc_51 <- .25
pc_52 <- .15
pc_53 <- .15
pc_54 <- .10

pc_21 <- .30
pc_31 <- .40
pc_32 <- .05
pc_41 <- .05
pc_42 <- .30
pc_43 <- .30
pc_51 <- .20
pc_52 <- .05
pc_53 <- .40
pc_54 <- .05

pc_mat <- matrix(0, nrow = n_var, ncol = n_var)
diag(pc_mat) <- -1

pc_mat[1, 2] <- pc_mat[2, 1] <- pc_21
pc_mat[3, 1] <- pc_mat[1, 3] <- pc_31 
pc_mat[3, 2] <- pc_mat[2, 3] <- pc_32 
pc_mat[4, 1] <- pc_mat[1, 4] <- pc_41 
pc_mat[4, 2] <- pc_mat[2, 4] <- pc_42
pc_mat[4, 3] <- pc_mat[3, 4] <- pc_43 
pc_mat[5, 1] <- pc_mat[1, 5] <- pc_51 
pc_mat[5, 2] <- pc_mat[2, 5] <- pc_52 
pc_mat[5, 3] <- pc_mat[3, 5] <- pc_53 
pc_mat[5, 4] <- pc_mat[4, 5] <- pc_54

pc_mat[6:10, 6:10] <- pc_mat[1:5, 1:5]
pc_mat[11:15, 11:15] <- pc_mat[1:5, 1:5]
pc_mat[16:20, 16:20] <- pc_mat[1:5, 1:5]
pc_mat[21:25, 21:25] <- pc_mat[1:5, 1:5]

# Add some between cluster connections (partial correlations)
pc_mat[1, 10] <- pc_mat[10, 1] <- .10
pc_mat[2, 7] <- pc_mat[7, 2] <- .10
pc_mat[4, 15] <- pc_mat[15, 4] <- .10
pc_mat[3, 12] <- pc_mat[12, 3] <- .10
pc_mat[4, 24] <- pc_mat[24, 4] <- .10
pc_mat[7, 18] <- pc_mat[18, 7] <- .10
pc_mat[17, 23] <- pc_mat[23, 17] <- .10
pc_mat[3, 19] <- pc_mat[19, 3] <- .10

# Invert -1*partial correlation
r_pc_inv <- solve(-pc_mat)

# Create d_pc to contain the sqrt inverse of the inverted partial correlation matrix
d_pc <- (diag(sqrt(diag(r_pc_inv))^-1, nrow(r_pc_inv)))

# Calculate the correlation matrix
r_mat <- d_pc%*%r_pc_inv%*%d_pc

# Simulate multivariate normal data using r_mat
 
n <- 500 # Sample size
ggm_data <- mvtnorm::rmvnorm(n = n, mean = rep(0, ncol(r_mat)), sigma = r_mat)

# Estimate guassian graphical network model

network <- bootnet::estimateNetwork(ggm_data, default = "pcor", threshold = "sig", alpha = .05)

network <- bootnet::estimateNetwork(
  ggm_data,
  default = "EBICglasso",
  corMethod = "cor_auto",
  tuning = .50
)
