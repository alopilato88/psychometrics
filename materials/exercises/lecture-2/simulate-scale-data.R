library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(ltm)

# User-function to simulate item responses from a scale
simulate_item_response <- function(
  theta,
  n_items = 5,
  n_response_points = 6,
  theta_name = "JOB_SAT",
  n_bad_item = 0,
  seed = NA
) {
  
  if(!is.na(seed)) {
    set.seed(seed)
  }
  
  # Set up item parameters 
  alpha <- runif(n_items, 1.5, 3) |> round(3)
  beta <- seq(-2, 2, length = n_response_points - 1)
  
  beta_matrix <- matrix(
    beta, 
    nrow = length(theta), 
    ncol = length(beta),
    byrow = TRUE
  )
  
  theta_beta_dif <- theta - beta_matrix
  
  alpha_theta <- purrr::map_dfr(
    alpha, 
    function(x) data.frame(x*theta_beta_dif)
    )
  
  p_star <- exp(alpha_theta) / (1 + exp(alpha_theta))
  p_star_diff <- 
    -apply(p_star, 1, diff) |>
    t()
   
  prob <- cbind(1 - p_star[, 1], p_star_diff, p_star[,ncol(p_star)])
  
  item_response <- 
    apply(
      prob,
      1,
      function(x) sample(1:n_response_points, 1, prob = x)
    )
  
  output <- data.frame(
    RESPONDENT_ID = rep(1:length(theta), n_items),
    ITEM_ID = rep(1:n_items, each = length(theta)),
    ITEM_RESPONSE = item_response,
    THETA_ID = theta_name,
    THETA = rep(theta, n_items),
    ALPHA = rep(alpha, each = length(theta))
  )
  
  if(n_bad_item != 0) {
    output_1 <- 
      simulate_item_response(
      theta = rnorm(length(theta)),
      n_items = n_bad_item,
      n_response_points = n_response_points,
      n_bad_item = 0,
      theta_name = theta_name
    )
    
    output_1$ITEM_ID <- output_1$ITEM_ID + n_items
    
    output <- dplyr::bind_rows(output, output_1)
    
  }
  
  return(output)
  
}

# Generate thetas
set.seed(42) # Set seed for replicability
n <- 500 # Sample Size

# Create relationships among thetas
theta_1 <- rnorm(n)
theta_2 <- theta_1 * .5 + rnorm(n, sd = sqrt(1 - var(theta_1 * .5)))
theta_3 <- theta_1 * .2 + theta_2 * .5 + rnorm(n, sd = sqrt(1 - var(theta_1 * .2 + theta_2 * .5)))

# Generate Scales
job_sat_resp <- simulate_item_response(
  theta = theta_1,
  n_items = 5,
  n_response_points = 6,
  seed = 2314,
  n_bad_item = 2
)

org_comm_resp <- simulate_item_response(
  theta = theta_2,
  n_items = 4,
  n_response_points = 7,
  seed = 98534,
  theta_name = "AFFECTIVE_COMMITMENT",
  n_bad_item = 1
)

turnover_int_resp <- simulate_item_response(
  theta = theta_3,
  n_items = 3,
  n_response_points = 6,
  seed = 94853,
  theta_name = "TURNOVER_INTENT",
  n_bad_item = 3
) |>
  dplyr::mutate(
    ITEM_RESPONSE = 7 - ITEM_RESPONSE # Reverse code responses to get negative corr
  )

# Create final data frame
data <- dplyr::bind_rows(
  job_sat_resp,
  org_comm_resp,
  turnover_int_resp
) |>
  dplyr::mutate(
    ITEM_ID = paste0(THETA_ID, "_", ITEM_ID)
  ) |>
  dplyr::select(
    -THETA_ID:-ALPHA
  ) |>
  tidyr::pivot_wider(
    names_from = ITEM_ID,
    values_from = ITEM_RESPONSE
  ) |>
  tibble::as_tibble()

# Save csv
write.csv(data, "materials/data/lecture-2-exercise.csv", row.names = FALSE)
