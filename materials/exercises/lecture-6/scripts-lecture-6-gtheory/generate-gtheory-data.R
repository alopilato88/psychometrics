# This script simulates the data needed for lecture 6

library(lme4)

# --- Example 1 Person x Item data --- #

set.seed(3245)

# N of people and items
np <- 500
ni <- 10

# Set variance component parameters 
var_person <- 3
g_coef <- .90
d_coef <- .75 

error <- (var_person*(1 - g_coef)) / (ni^-1 * g_coef)
var_item <- (var_person - (var_person + (error/ni))*d_coef) / (ni^-1*d_coef)

id_person <- rep(1:np, each = ni)
id_item <- rep(1:ni, np)

# Create the random effects design matrix 
Z <- cbind(
  model.matrix(~as.factor(id_person) - 1),
  model.matrix(~as.factor(id_item) - 1)
)

# Generate random effects 
re_item <- rnorm(ni, mean = 0, sd = sqrt(var_item))
re_person <- rnorm(np, mean = 0, sd = sqrt(var_person))
re_error <- rnorm(np*ni, mean = 0, sd = sqrt(error))

# Generate outcome 
y <- 4 + Z%*%c(re_person, re_item) + re_error

# Structure data 
data_e1 <- 
  tibble::tibble(
    RESP_ID = id_person,
    ITEM_ID = id_item,
    RESPONSE = as.numeric(y)
  )

# Check simulation data
mod_e1 <- lme4::lmer(RESPONSE ~ 1 + (1 | RESP_ID) + (1 | ITEM_ID), 
                     data = data_e1)

# --- Example 2 - Person x Rater x Item --- # 

# N of people and items
np <- 100
ni <- 10
nr <- 10

# Set variance component parameters 
var_person <- 1
g_coef <- .40
d_coef <- .30

# Proportion of relative and absolute error made up by:
# pi, pr, pri for relative and i, r, and ir for absolute
rel_error_prop <- c(.05, .75, .20)
abs_resid_error_prop <- c(.15, .80, .05)

# Calculate error variances
rel_error <- (var_person*(1 - g_coef)) / (g_coef)
var_per_item <- rel_error*rel_error_prop[1]
var_per_rater <- rel_error*rel_error_prop[2]
var_residual <- rel_error*rel_error_prop[3]

abs_error <- (var_person*(1 - d_coef)) / (d_coef)

abs_resid_error <- abs_error - rel_error

var_item <- abs_resid_error*abs_resid_error_prop[1]
var_rater <- abs_resid_error*abs_resid_error_prop[2]
var_rater_item <- abs_resid_error*abs_resid_error_prop[3]

# Create random effect id factors
id_person <- rep(1:np, each = ni*nr)
id_item <- rep(1:ni, each = np, nr)
id_rater <- rep(1:nr, np*ni)

# Create the random effects design matrix 
Z <- cbind(
  model.matrix(~as.factor(id_person) - 1),
  model.matrix(~as.factor(id_item) - 1),
  model.matrix(~as.factor(id_rater) - 1)
)

Z_person_item <- 
  purrr::map_dfc(1:np, function(x) Z[,x]*Z[,(np+1):(np+ni)]) |>
  as.matrix()

Z_person_rater <- 
  purrr::map_dfc(1:np, function(x) Z[,x]*Z[,(np+ni+1):(np+ni+nr)]) |>
  as.matrix()

Z_item_rater <- 
  purrr::map_dfc((np+1):(np+ni), function(x) Z[,x]*Z[,(np+ni+1):(np+ni+nr)]) |>
  as.matrix()

Z <- cbind(
  Z,
  Z_person_item,
  Z_person_rater,
  Z_item_rater
)

# Generate random effects 
re_person <- rnorm(np, mean = 0, sd = sqrt(var_person))
re_rater <- rnorm(nr, mean = 0, sd = sqrt(var_rater))
re_item <- rnorm(ni, mean = 0, sd = sqrt(var_item))
re_person_rater <- rnorm(np*nr, mean = 0, sd = sqrt(var_per_rater))
re_person_item <- rnorm(np*ni, mean = 0, sd = sqrt(var_per_item))
re_item_rater <- rnorm(nr*ni, mean = 0, sd = sqrt(var_rater_item))
re_residual <- rnorm(np*ni, mean = 0, sd = sqrt(var_residual))

# Generate outcome 
y <- 4 + Z%*%c(re_person, re_item, re_rater, re_person_item, re_person_rater, re_item_rater) + re_residual

# Structure data 
data_e2 <- 
  tibble::tibble(
    RESP_ID = id_person,
    ITEM_ID = id_item,
    RATER_ID = id_rater,
    RESPONSE = as.numeric(y)
  )

# Check simulation data
mod_e1 <- lme4::lmer(RESPONSE ~ 1 + (1 | RESP_ID) + (1 | ITEM_ID), 
                     data = data_e1)
