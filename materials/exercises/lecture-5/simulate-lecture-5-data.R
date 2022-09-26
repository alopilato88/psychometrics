# Set seed
set.seed(43256)

# N respondents and items
n_resp <- 500
n_item <- 6

# Resp and item variance
# var_resp <- 2
var_item <- 1

# Levels of reliability (alpha)
alpha_par <- .80

# Corresponding error variance
# var_error <- (n_item * var_resp) * ((1 /alpha_med) - 1)
var_error <- 3.29
var_resp <- (alpha_par * (var_error / n_item))/(1 - alpha_par)

# ID vectors
id_resp <- rep(1:n_resp, each = n_item)
id_item <- rep(1:n_item, n_resp)

# Random effects design matrices
Z_resp <- model.matrix(~as.factor(id_resp) - 1)
Z_item <- model.matrix(~as.factor(id_item) - 1)
Z <- cbind(Z_resp, Z_item)

# Random effects
re_item <- rnorm(n_item, mean = 0, sd = sqrt(var_item))
re_resp <- rnorm(n_resp, mean = 0, sd = sqrt(var_resp))
rand_effect <- c(re_resp, re_item)

# error <- rnorm(n_resp*n_item, mean = 0, sd = sqrt(var_error))
error <- rlogis(n_resp * n_item)

# Realized responses
response <- 3 + Z%*%rand_effect + error

# Data
data <-
  tibble::tibble(
    ID_RESP = as.factor(id_resp),
    ID_ITEM = as.factor(id_item),
    RESPONSE = response
  ) |>
  dplyr::mutate(
    RESPONSE_OBS = dplyr::case_when(
      RESPONSE <= -1 ~ 1,
      RESPONSE <= 0 ~ 2,
      RESPONSE <= 1 ~ 3,
      RESPONSE <= 3 ~ 4,
      RESPONSE <= 4 ~ 5, 
      RESPONSE <= 6 ~ 6,
      TRUE ~ 7
    )
  )

data_wide <- 
  data |>
  dplyr::select(
    ID_RESP, 
    ID_ITEM,
    RESPONSE_OBS
  ) |>
  tidyr::pivot_wider(
    names_from = ID_ITEM,
    values_from = RESPONSE_OBS,
    names_prefix = "ITEM_"
  )

write.csv(data_wide, "lecture-5-exercise-data.csv", row.names = FALSE)
