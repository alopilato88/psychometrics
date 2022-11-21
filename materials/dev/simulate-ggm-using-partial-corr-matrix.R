# Load packages
library(dplyr)
library(tibble)
library(mvtnorm)
library(bootnet)

### --- Generating GGM from partial correlation matrix --- ###

# Create population partial correlation matrix

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

pc_21 <- .40
pc_31 <- .40
pc_32 <- .00
pc_41 <- .00
pc_42 <- .40
pc_43 <- .40
pc_51 <- .00
pc_52 <- .00
pc_53 <- .40
pc_54 <- .00

pc_mat <- diag(-1, 5)

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

# Invert -1*partial correlation
r_pc_inv <- solve(-pc_mat)

# Create d_pc to contain the sqrt inverse of the inverted partial correlation matrix
d_pc <- (diag(sqrt(diag(r_pc_inv))^-1, nrow(r_pc_inv)))

# Calculate the correlation matrix
r_mat <- d_pc%*%r_pc_inv%*%d_pc

# Simulate multivariate normal data using r_mat
 
n <- 1000 # Sample size
ggm_data <- mvtnorm::rmvnorm(n = n, mean = rep(0, ncol(r_mat)), sigma = r_mat)

# Estimate guassian graphical network model

network <- bootnet::estimateNetwork(ggm_data, default = "pcor")

network <- bootnet::estimateNetwork(
  ggm_data,
  default = "EBICglasso",
  corMethod = "cor_auto",
  tuning = .50,
  refit = TRUE
)
