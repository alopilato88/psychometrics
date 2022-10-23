library(dplyr)
library(tibble)
library(lavaan)
library(mvtnorm)

set.seed(43523)

# --- Generate unidimensional model with constraints --- #

n <- 1000

lv_1 <- rnorm(n, mean = 5, sd = sqrt(3))

x1 <- lv_1 + rnorm(n, mean = 0, sd = sqrt(2))
x2 <- lv_1 + rnorm(n, mean = 0, sd = sqrt(2))
x3 <- lv_1 + rnorm(n, mean = 0, sd = sqrt(2))
x4 <- lv_1 + rnorm(n, mean = 0, sd = sqrt(2))

data_constraint <- 
  tibble::tibble(
    X1 = x1,
    X2 = x2,
    X3 = x3, 
    X4 = x4
  )

congeneric_syntax <- "
LV =~ NA*X1 + X2 + X3 + X4

X1 + X2 + X3 + X4 ~ 1
LV ~~ 1*LV
"
essential_tau_syntax <- "

LV =~ A*X1 + NA*X1 + A*X2 + A*X3 + A*X4 

X1 + X2 + X3 + X4 ~ 1
LV ~~ 1*LV
"

tau_syntax <- "

LV =~ A*X1 + NA*X1 + A*X2 + A*X3 + A*X4 

X1 + X2 + X3 + X4 ~ B*1
LV ~~ 1*LV
"

parallel_fit <- "
LV =~ A*X1 + NA*X1 + A*X2 + A*X3 + A*X4 

X1 + X2 + X3 + X4 ~ B*1
LV ~~ 1*LV
X1 ~~ C*X1
X2 ~~ C*X2
X3 ~~ C*X3
X4 ~~ C*X4
"

# congeneric_fit <- lavaan::cfa(model = congeneric_syntax, data = data_constraint)
# essential_tau_fit <- lavaan::cfa(model = essential_tau_syntax, data = data_constraint)
# tau_fit <- lavaan::cfa(model = tau_syntax, data = data_constraint)
# parallel_fit <- lavaan::cfa(model = parallel_fit, data = data_constraint)

write.csv(data_constraint, "materials/data/lecture-9-data-constraint.csv", row.names = FALSE)

# --- Generate Hierarchical Model --- #
n <- 1000

hier_factor <- rnorm(n)
lower_order_f1 <- .7*hier_factor + rnorm(n, 0, sd = sqrt(1 - var(.7*hier_factor)))
lower_order_f2 <- .7*hier_factor + rnorm(n, 0, sd = sqrt(1 - var(.7*hier_factor)))
lower_order_f3 <- .7*hier_factor + rnorm(n, 0, sd = sqrt(1 - var(.7*hier_factor)))


x1 <- .8*lower_order_f1 + rnorm(n, 0, sd = sqrt(1 - var(.8*lower_order_f1)))
x2 <- .8*lower_order_f1 + rnorm(n, 0, sd = sqrt(1 - var(.8*lower_order_f1)))
x3 <- .8*lower_order_f1 + rnorm(n, 0, sd = sqrt(1 - var(.8*lower_order_f1)))
x4 <- .8*lower_order_f1 + rnorm(n, 0, sd = sqrt(1 - var(.8*lower_order_f1)))

x5 <- .8*lower_order_f2 + rnorm(n, 0, sd = sqrt(1 - var(.8*lower_order_f2)))
x6 <- .8*lower_order_f2 + rnorm(n, 0, sd = sqrt(1 - var(.8*lower_order_f2)))
x7 <- .8*lower_order_f2 + rnorm(n, 0, sd = sqrt(1 - var(.8*lower_order_f2)))
x8 <- .8*lower_order_f2 + rnorm(n, 0, sd = sqrt(1 - var(.8*lower_order_f2)))

x9 <- .8*lower_order_f3 + rnorm(n, 0, sd = sqrt(1 - var(.8*lower_order_f3)))
x10 <- .8*lower_order_f3 + rnorm(n, 0, sd = sqrt(1 - var(.8*lower_order_f3)))
x11 <- .8*lower_order_f3 + rnorm(n, 0, sd = sqrt(1 - var(.8*lower_order_f3)))
x12 <- .8*lower_order_f3 + rnorm(n, 0, sd = sqrt(1 - var(.8*lower_order_f3)))

hierarchical_data <- tibble::tibble(
  X1 = x1, 
  X2 = x2,
  X3 = x3, 
  X4 = x4,
  X5 = x5,
  X6 = x6, 
  X7 = x7,
  X8 = x8,
  X9 = x9,
  X10 = x10,
  X11 = x11, 
  X12 = x12
)

hierchical_model_syntax <- "

F1 =~ NA*X1 + X2 + X3 + X4
F2 =~ NA*X5 + X6 + X7 + X8
F3 =~ NA*X9 + X10 + X11 + X12

HIER_FACTOR =~ NA*F1 + F2 + F3

HIER_FACTOR ~~ 1*HIER_FACTOR
F1 ~~ 1*F1
F2 ~~ 1*F2
F3 ~~ 1*F3
"

multidimensional_model_syntax <- "

F1 =~ NA*X1 + X2 + X3 + X4
F2 =~ NA*X5 + X6 + X7 + X8
F3 =~ NA*X9 + X10 + X11 + X12

F1 ~~ 1*F1
F2 ~~ 1*F2
F3 ~~ 1*F3
"

# hierachical_fit <- lavaan::cfa(model = hierchical_model_syntax, data = hierachical_data)
# multidimensional_fit <- lavaan::cfa(model = multidimensional_model_syntax, data = hierachical_data)

write.csv(hierarchical_data, "materials/data/lecture-9-data-higher-order.csv", row.names = FALSE)

# --- Generate bi-factor model --- #
n <- 10000

general_factor <- rnorm(n)
m1_factor <- rnorm(n)
m2_factor <- rnorm(n)

x1 <- .8*general_factor + .5*m1_factor 
x2 <- .75*general_factor + .6*m1_factor 
x3 <- .7*general_factor + .5*m1_factor 
x4 <- .7*general_factor + .6*m1_factor 

x5 <- .7*general_factor + .6*m2_factor 
x6 <- .65*general_factor + .5*m2_factor 
x7 <- .6*general_factor + .4*m2_factor 
x8 <- .7*general_factor + .6*m2_factor 

x1 <- x1 + rnorm(n, mean = 0, sd = sqrt(1 - var(x1)))
x2 <- x2 + rnorm(n, mean = 0, sd = sqrt(1 - var(x2)))
x3 <- x3 + rnorm(n, mean = 0, sd = sqrt(1 - var(x3)))
x4 <- x4 + rnorm(n, mean = 0, sd = sqrt(1 - var(x4)))
x5 <- x5 + rnorm(n, mean = 0, sd = sqrt(1 - var(x5)))
x6 <- x6 + rnorm(n, mean = 0, sd = sqrt(1 - var(x6)))
x7 <- x7 + rnorm(n, mean = 0, sd = sqrt(1 - var(x7)))
x8 <- x8 + rnorm(n, mean = 0, sd = sqrt(1 - var(x8)))

bifactor_data <- tibble::tibble(
  X1 = x1, 
  X2 = x2, 
  X3 = x3,
  X4 = x4, 
  X5 = x5,
  X6 = x6, 
  X7 = x7, 
  X8 = x8
)

bifactor_model_syntax <- "

GEN_FACTOR =~ NA*X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8
METHOD1_FACTOR =~ NA*X1 + X2 + X3 + X4
METHOD2_FACTOR =~ NA*X5 + X6 + X7 + X8

GEN_FACTOR ~~ 1*GEN_FACTOR
METHOD1_FACTOR ~~ 1*METHOD1_FACTOR
METHOD2_FACTOR ~~ 1*METHOD2_FACTOR

METHOD1_FACTOR + METHOD2_FACTOR ~~ 0*GEN_FACTOR
METHOD1_FACTOR ~~ 0*METHOD2_FACTOR
"

cor_error_model_syntax <- "

GEN_FACTOR =~ NA*X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8

GEN_FACTOR ~~ 1*GEN_FACTOR

# X1 + X2 + X3 ~~ X4
# X1 + X2 ~~ X3
# X1 ~~ X2

X5 + X6 + X7 ~~ X8
X5 + X6 ~~ X7
X5 ~~ X6
"

# bifactor_fit <- lavaan::cfa(model = bifactor_model_syntax, data = bifactor_data)
# corr_error_fit <- lavaan::cfa(model = cor_error_model_syntax, data = bifactor_data)

write.csv(bifactor_data, "materials/data/lecture-9-data-bifactor.csv", row.names = FALSE)

# --- Generate TRI model --- #

# Generate hierchical observer factor 
n <- 10000
hier_obs_factor <- rnorm(n)
trait_factor <- rnorm(n)
identity_factor <- rnorm(n)

hier_loading <- .60
# obs_var <- (1 - var(hier_loading*hier_obs_factor))
obs_var <- 1 - var(hier_loading*hier_obs_factor)
obs_1_factor <- hier_loading*hier_obs_factor + rnorm(n, sd = sqrt(obs_var))
obs_2_factor <- hier_loading*hier_obs_factor + rnorm(n, sd = sqrt(obs_var))
obs_3_factor <- hier_loading*hier_obs_factor + rnorm(n, sd = sqrt(obs_var))

# Create items without error yet
# Self Factor
identity_loading <- sqrt(.3)
self_trait_loading <- sqrt(.5)

self_1 <- 4 + self_trait_loading*trait_factor + identity_loading*identity_factor
self_2 <- 4 + self_trait_loading*trait_factor + identity_loading*identity_factor
self_3 <- 4 + self_trait_loading*trait_factor + identity_loading*identity_factor
self_4 <- 4 + self_trait_loading*trait_factor + identity_loading*identity_factor
self_5 <- 4 + self_trait_loading*trait_factor + identity_loading*identity_factor
self_6 <- 4 + self_trait_loading*trait_factor + identity_loading*identity_factor

observer_loading <- sqrt(.4)
trait_loading <- sqrt(.4)

# Observer 1 Factor
obs1_1 <- 4 + trait_loading*trait_factor + observer_loading*obs_1_factor
obs1_2 <- 4 + trait_loading*trait_factor + observer_loading*obs_1_factor
obs1_3 <- 4 + trait_loading*trait_factor + observer_loading*obs_1_factor
obs1_4 <- 4 + trait_loading*trait_factor + observer_loading*obs_1_factor
obs1_5 <- 4 + trait_loading*trait_factor + observer_loading*obs_1_factor
obs1_6 <- 4 + trait_loading*trait_factor + observer_loading*obs_1_factor

# Observer 2 Factor
obs2_1 <- 4 + trait_loading*trait_factor + observer_loading*obs_2_factor
obs2_2 <- 4 + trait_loading*trait_factor + observer_loading*obs_2_factor
obs2_3 <- 4 + trait_loading*trait_factor + observer_loading*obs_2_factor
obs2_4 <- 4 + trait_loading*trait_factor + observer_loading*obs_2_factor
obs2_5 <- 4 + trait_loading*trait_factor + observer_loading*obs_2_factor
obs2_6 <- 4 + trait_loading*trait_factor + observer_loading*obs_2_factor

# Observer 3 Factor
obs3_1 <- 4 + trait_loading*trait_factor + observer_loading*obs_3_factor
obs3_2 <- 4 + trait_loading*trait_factor + observer_loading*obs_3_factor
obs3_3 <- 4 + trait_loading*trait_factor + observer_loading*obs_3_factor
obs3_4 <- 4 + trait_loading*trait_factor + observer_loading*obs_3_factor
obs3_5 <- 4 + trait_loading*trait_factor + observer_loading*obs_3_factor
obs3_6 <- 4 + trait_loading*trait_factor + observer_loading*obs_3_factor

# Error Strucutre
error_var <- .20
error_cor <- 0
error_cov <- error_cor*error_var

error_sigma <- diag(24)*error_var
error_sigma[1, 7] <- error_sigma[1, 13] <- error_sigma[1, 19] <- error_cov
error_sigma[2, 8] <- error_sigma[2, 14] <- error_sigma[2, 20] <- error_cov
error_sigma[3, 9] <- error_sigma[3, 15] <- error_sigma[3, 21] <- error_cov
error_sigma[4, 10] <- error_sigma[4, 16] <- error_sigma[4, 22] <- error_cov
error_sigma[5, 11] <- error_sigma[5, 17] <- error_sigma[5, 23] <- error_cov
error_sigma[6, 12] <- error_sigma[6, 18] <- error_sigma[6, 24] <- error_cov
error_sigma[7, 13] <- error_sigma[7, 19] <- error_sigma[7, 1] <- error_cov
error_sigma[8, 14] <- error_sigma[8, 20] <- error_sigma[8, 2] <- error_cov
error_sigma[9, 15] <- error_sigma[9, 21] <- error_sigma[9, 3] <- error_cov
error_sigma[10, 16] <- error_sigma[10, 22] <- error_sigma[10, 4] <- error_cov
error_sigma[11, 17] <- error_sigma[11, 23] <- error_sigma[11, 5] <- error_cov
error_sigma[12, 18] <- error_sigma[12, 24] <- error_sigma[12, 6] <- error_cov
error_sigma[13, 19] <- error_sigma[13, 1] <- error_sigma[13, 7] <- error_cov
error_sigma[14, 20] <- error_sigma[14, 2] <- error_sigma[14, 8] <- error_cov
error_sigma[15, 21] <- error_sigma[15, 3] <- error_sigma[15, 9] <- error_cov
error_sigma[16, 22] <- error_sigma[16, 4] <- error_sigma[16, 10] <- error_cov
error_sigma[17, 23] <- error_sigma[17, 5] <- error_sigma[17, 11] <- error_cov
error_sigma[18, 24] <- error_sigma[18, 6] <- error_sigma[18, 12] <- error_cov
error_sigma[19, 1] <- error_sigma[19, 7] <- error_sigma[19, 13] <- error_cov
error_sigma[20, 2] <- error_sigma[20, 8] <- error_sigma[20, 14] <- error_cov
error_sigma[21, 3] <- error_sigma[21, 9] <- error_sigma[21, 15] <- error_cov
error_sigma[22, 4] <- error_sigma[22, 10] <- error_sigma[22, 16] <- error_cov
error_sigma[23, 5] <- error_sigma[23, 11] <- error_sigma[23, 17] <- error_cov
error_sigma[24, 6] <- error_sigma[24, 12] <- error_sigma[24, 18] <- error_cov

error <- mvtnorm::rmvnorm(n = n, mean = rep(0, 24), sigma = error_sigma)

self_1 <- self_1 + error[,1]
self_2 <- self_2 + error[,2]
self_3 <- self_3 + error[,3]
self_4 <- self_4 + error[,4]
self_5 <- self_5 + error[,5]
self_6 <- self_6 + error[,6]

# Observer 1 Factor
obs1_1 <- obs1_1 + error[,7]
obs1_2 <- obs1_2 + error[,8]
obs1_3 <- obs1_3 + error[,9]
obs1_4 <- obs1_4 + error[,10]
obs1_5 <- obs1_5 + error[,11]
obs1_6 <- obs1_6 + error[,12]

# Observer 2 Factor
obs2_1 <- obs2_1 + error[,13]
obs2_2 <- obs2_2 + error[,14]
obs2_3 <- obs2_3 + error[,15]
obs2_4 <- obs2_4 + error[,16]
obs2_5 <- obs2_5 + error[,17]
obs2_6 <- obs2_6 + error[,18]

# Observer 3 Factor
obs3_1 <- obs3_1 + error[,19]
obs3_2 <- obs3_2 + error[,20]
obs3_3 <- obs3_3 + error[,21]
obs3_4 <- obs3_4 + error[,22]
obs3_5 <- obs3_5 + error[,23]
obs3_6 <- obs3_6 + error[,24]

tri_data <- 
  tibble::tibble(
    SELF_1 = self_1,
    SELF_2 = self_2,
    SELF_3 = self_3,
    SELF_4 = self_4,
    SELF_5 = self_5,
    SELF_6 = self_6,
    OBS1_1 = obs1_1,
    OBS1_2 = obs1_2,
    OBS1_3 = obs1_3,
    OBS1_4 = obs1_4,
    OBS1_5 = obs1_5,
    OBS1_6 = obs1_6,
    OBS2_1 = obs2_1,
    OBS2_2 = obs2_2,
    OBS2_3 = obs2_3,
    OBS2_4 = obs2_4,
    OBS2_5 = obs2_5,
    OBS2_6 = obs2_6,
    OBS3_1 = obs3_1,
    OBS3_2 = obs3_2,
    OBS3_3 = obs3_3,
    OBS3_4 = obs3_4,
    OBS3_5 = obs3_5,
    OBS3_6 = obs3_6
  )

tri_model_syntax <- "

TRAIT =~ 
  NA*SELF_1 + SELF_2 + SELF_3 + SELF_4 + SELF_5 + SELF_6 +
  T_OBS_1*OBS1_1 + T_OBS_2*OBS1_2 + T_OBS_3*OBS1_3 + T_OBS_4*OBS1_4 + T_OBS_5*OBS1_5 + T_OBS_6*OBS1_6 +
  T_OBS_1*OBS2_1 + T_OBS_2*OBS2_2 + T_OBS_3*OBS2_3 + T_OBS_4*OBS2_4 + T_OBS_5*OBS2_5 + T_OBS_6*OBS2_6 +
  T_OBS_1*OBS3_1 + T_OBS_2*OBS3_2 + T_OBS_3*OBS3_3 + T_OBS_4*OBS3_4 + T_OBS_5*OBS3_5 + T_OBS_6*OBS3_6
  
IDENTITY =~ 
  NA*SELF_1 + SELF_2 + SELF_3 + SELF_4 + SELF_5 + SELF_6

OBS_1 =~ 
  NA*OBS1_1 + OBS_1*OBS1_1 + OBS_2*OBS1_2 + OBS_3*OBS1_3 + OBS_4*OBS1_4 + OBS_5*OBS1_5 + OBS_6*OBS1_6

OBS_2 =~ 
  NA*OBS2_1 + OBS_1*OBS2_1 + OBS_2*OBS2_2 + OBS_3*OBS2_3 + OBS_4*OBS2_4 + OBS_5*OBS2_5 + OBS_6*OBS2_6
  
OBS_3 =~ 
  NA*OBS3_1 + OBS_1*OBS3_1 + OBS_2*OBS3_2 + OBS_3*OBS3_3 + OBS_4*OBS3_4 + OBS_5*OBS3_5 + OBS_6*OBS3_6

REPUTATION =~ 
  NA*OBS_1 + REP_OBS*OBS_1 + REP_OBS*OBS_2 + REP_OBS*OBS_3

TRAIT ~~ 1*TRAIT
IDENTITY ~~ 1*IDENTITY
REPUTATION ~~ 1*REPUTATION

OBS_1 ~~ 1*OBS_1
OBS_2 ~~ 1*OBS_2
OBS_3 ~~ 1*OBS_3

# OBS_1 ~~ OBS_VAR*OBS_1
# OBS_2 ~~ OBS_VAR*OBS_2
# OBS_3 ~~ OBS_VAR*OBS_3

TRAIT ~~ 0*IDENTITY + 0*REPUTATION
IDENTITY ~~ 0*REPUTATION

OBS1_1 + OBS2_1 + OBS3_1 ~~ R_SO_1*SELF_1
OBS1_2 + OBS2_2 + OBS3_2 ~~ R_SO_2*SELF_2
OBS1_3 + OBS2_3 + OBS3_3 ~~ R_SO_3*SELF_3
OBS1_4 + OBS2_4 + OBS3_4 ~~ R_SO_4*SELF_4
OBS1_5 + OBS2_5 + OBS3_5 ~~ R_SO_5*SELF_5
OBS1_6 + OBS2_6 + OBS3_6 ~~ R_SO_6*SELF_6

OBS1_1 + OBS2_1 ~~ COV_O_1*OBS3_1
OBS1_1 ~~ COV_O_1*OBS2_1

OBS1_2 + OBS2_2 ~~ COV_O_2*OBS3_2
OBS1_2 ~~ COV_O_2*OBS2_2

OBS1_3 + OBS2_3 ~~ COV_O_3*OBS3_3
OBS1_3 ~~ COV_O_3*OBS2_3

OBS1_4 + OBS2_4 ~~ COV_O_4*OBS3_4
OBS1_4 ~~ COV_O_4*OBS2_4

OBS1_5 + OBS2_5 ~~ COV_O_5*OBS3_5
OBS1_5 ~~ COV_O_5*OBS2_5

OBS1_6 + OBS2_6 ~~ COV_O_6*OBS3_6
OBS1_6 ~~ COV_O_6*OBS2_6

OBS1_1 ~~ R_O_1*OBS1_1
OBS2_1 ~~ R_O_1*OBS2_1
OBS3_1 ~~ R_O_1*OBS3_1

OBS1_2 ~~ R_O_2*OBS1_2
OBS2_2 ~~ R_O_2*OBS2_2
OBS3_2 ~~ R_O_2*OBS3_2

OBS1_3 ~~ R_O_3*OBS1_3
OBS2_3 ~~ R_O_3*OBS2_3
OBS3_3 ~~ R_O_3*OBS3_3

OBS1_4 ~~ R_O_4*OBS1_4
OBS2_4 ~~ R_O_4*OBS2_4
OBS3_4 ~~ R_O_4*OBS3_4

OBS1_5 ~~ R_O_5*OBS1_5
OBS2_5 ~~ R_O_5*OBS2_5
OBS3_5 ~~ R_O_5*OBS3_5

OBS1_6 ~~ R_O_6*OBS1_6
OBS2_6 ~~ R_O_6*OBS2_6
OBS3_6 ~~ R_O_6*OBS3_6
"

# tri_fit <- lavaan::cfa(model = tri_model_syntax, data = tri_data)
write.csv(tri_data, "materials/data/lecture-9-data-tri-model.csv", row.names = FALSE)
