# --- Load packages --- #

library(lavaan)
library(mvtnorm)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(ggplot2)

# --- Create helper functions --- #

create_indicator_variables <- function(
    theta,
    factor_loading,
    var_name_prefix
) {
  
  error_cov_matrix <- diag(1 - factor_loading^2)
  uniqueness_var <- mvtnorm::rmvnorm(
    n = length(theta),
    sigma = error_cov_matrix,
    mean = rep(0, ncol(error_cov_matrix))
  ) |>
    t()
  
  ind_var_data <- matrix(factor_loading, ncol = 1)%*%theta + uniqueness_var
  ind_var_data <- t(ind_var_data)
  
  output <- tibble::as_tibble(
    ind_var_data
  )
  
  names(output) <- paste0(var_name_prefix, 1:ncol(output))
  
  return(output)
  
}

create_ordinal_response <- function(variable, prob) {
  
  var_ordinal <- variable 
  cum_sum_prob <- cumsum(prob)
  var_quantile <- quantile(var_ordinal, cum_sum_prob)
  var_quantile <- matrix(var_quantile, nrow = length(var_ordinal), ncol = length(cum_sum_prob), byrow = T)
  var_log <- var_ordinal <= var_quantile
  var_response <- apply(var_log, 1, function(x) which.max(x))
  
  return(var_response)
  
}

create_multiple_ordinal_response <- function(
    data,
    prob_list
) {
  
  output <- purrr::map_dfc(
    1:ncol(data),
    function(i) {
      var <- unlist(data[, i])
      prob <- prob_list[[i]]
      ord_response <- create_ordinal_response(var, prob)
      return(ord_response)
    }
  )
  
  names(output) <- names(data)
  return(output)
}

# --- Set simulation parameters --- #

# set.seed(342534) # CFA Data Seed
set.seed(43523) # EFA Data Sees
n_theta <- 3
n <- 500

# Factor loadings
factor_loading_par <- matrix(
  c(.6, .8, .7, .7, .9, 
    .9, .7, .8, .8, .8, 
    .5, .8, .7, .7, .8),
  nrow = n_theta,
  byrow = TRUE
)

# Theta population covariance matrix and mean vector
theta_cov <- matrix(
  c(1, .5, .6, 
    .5, 1, .4, 
    .6, .4, 1),
  ncol = n_theta,
  nrow = n_theta,
  byrow = TRUE
)

theta_mean <- rep(0, n_theta)

# Ordinal response cut-offs

resp_lv_1 <- list(
  c(0.10, 0.15, 0.20, 0.20, 0.15, 0.10, 0.10),
  c(0.05, 0.05, 0.10, 0.15, 0.20, 0.20, 0.25),
  c(0.01, 0.05, 0.14, 0.40, 0.24, 0.05, 0.11),
  c(0.05, 0.15, 0.20, 0.20, 0.15, 0.10, 0.15),
  c(0.10, 0.10, 0.15, 0.20, 0.15, 0.15, 0.15)
)

resp_lv_2 <- list(
  c(0.10, 0.10, 0.15, 0.20, 0.15, 0.15, 0.15),
  c(0.05, 0.05, 0.10, 0.15, 0.20, 0.20, 0.25),
  c(0.01, 0.05, 0.04, 0.20, 0.24, 0.20, 0.21),
  c(0.05, 0.15, 0.20, 0.20, 0.15, 0.10, 0.15),
  c(0.10, 0.15, 0.20, 0.20, 0.15, 0.10, 0.10)
)

resp_lv_3 <- list(
  c(0.01, 0.02, 0.02, 0.15, 0.16, 0.30, 0.34),
  c(0.05, 0.05, 0.10, 0.15, 0.20, 0.20, 0.25),
  c(0.01, 0.05, 0.04, 0.20, 0.24, 0.20, 0.21),
  c(0.05, 0.15, 0.20, 0.20, 0.15, 0.10, 0.15),
  c(0.10, 0.10, 0.15, 0.20, 0.15, 0.15, 0.15)
)

# Generate data

theta_matrix <- mvtnorm::rmvnorm(
  n = n, 
  mean = theta_mean,
  sigma = theta_cov
)

# Create continuous data
data_1_cont <- create_indicator_variables(
  theta = theta_matrix[, 1],
  factor_loading = factor_loading_par[1, ],
  var_name_prefix = "LV_1_"
)

data_2_cont <- create_indicator_variables(
  theta = theta_matrix[, 2],
  factor_loading = factor_loading_par[2, ],
  var_name_prefix = "LV_2_"
)

data_3_cont <- create_indicator_variables(
  theta = theta_matrix[, 3],
  factor_loading = factor_loading_par[3, ],
  var_name_prefix = "LV_3_"
)

# Create ordinal data

data_1_ord <- create_multiple_ordinal_response(
  data = data_1_cont,
  prob_list = resp_lv_1
)

data_2_ord <- create_multiple_ordinal_response(
  data = data_2_cont,
  prob_list = resp_lv_2
)

data_3_ord <- create_multiple_ordinal_response(
  data = data_3_cont,
  prob_list = resp_lv_3
)

data_ordinal <- dplyr::bind_cols(
  data_1_ord,
  data_2_ord,
  data_3_ord
)

data_continuous <- dplyr::bind_cols(
  data_1_cont,
  data_2_cont,
  data_3_cont
)

data_ord_long <- 
  data_ordinal |>
  tidyr::pivot_longer(
    cols = LV_1_1:LV_3_5,
    names_to = "LV_ITEM",
    values_to = "RESPONSE"
  ) |>
  dplyr::mutate(
    LV = stringr::str_sub(LV_ITEM, 1, 4)
  )

ggplot2::ggplot(
  data = data_ord_long |> 
    dplyr::filter(LV == "LV_3") |>
    dplyr::group_by(
      LV_ITEM,
      RESPONSE
    ) |>
    dplyr::summarise(
      COUNT = dplyr::n()
    ),
  ggplot2::aes(
    x = as.factor(RESPONSE), 
    y = COUNT
  )
) + 
  ggplot2::geom_bar(stat = "identity") + 
  ggplot2::facet_wrap(LV_ITEM ~ .) +
  ggplot2::theme_minimal()

cfa_model <- "
f1 =~ NA*LV_1_1 + LV_1_2 + LV_1_3 + LV_1_4 + LV_1_5
f2 =~ NA*LV_2_1 + LV_2_2 + LV_2_3 + LV_2_4 + LV_2_5
f3 =~ NA*LV_3_1 + LV_3_2 + LV_3_3 + LV_3_4 + LV_3_5

f1~~1*f1
f2~~1*f2
f3~~1*f3
"


cfa_fit <- lavaan::cfa(model = cfa_model, data = data_ordinal, estimator = "MLM")

cfa_fit <- lavaan::cfa(model = cfa_model, data = data_ordinal)
