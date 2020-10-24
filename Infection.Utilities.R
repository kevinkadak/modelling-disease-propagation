#Infection.Utilities.R
#Kevin Kadak (1007522888)

# Generate initial infection-status vector (fn1a)
initial_inf_stat_vec <- function(n, k) { # n is the size of the population; k is the number of initially-infected people within it
  n <- as.integer(n) # Length of vector
  k <- as.integer(k) # Number of infected people within total vector set

  total_pop <- as.numeric(!as.logical(seq(1, n))) # Generate vector of 0 by inversely converting booleans as numeric
  inf_sample <- sample(n, k, replace = FALSE) # Generate infected sample of size k.  Only generate unique values up to size n
  inf_status_vec <- replace(total_pop, inf_sample, as.numeric(as.logical(inf_sample))) # In n, replace 0 values at indecies defined by inf_sample with 1

  return(inf_status_vec) # Return infection status vector
}

# Generate probability of infection vector (fn1b)
inf_prob_vec <- function(n, mask_fraction = c(1/3, 1/3, 1/3)) {
  n <- as.integer(n)
  if (1 - sum(mask_fraction) > 1e-10 ) {
    stop("Error: Sum of mask fraction vector must total to equal 1.") # If sum of fractions in vector != 1 (ie. 100%), output error
  }

  mask_prob_list <- list( # Values for mask probability
    n95_mask = 0.01,
    non_med_mask = 0.05,
    no_mask = 0.1)

  prob_vec <- sample(c( # Construct proability vector by referencing values above
    mask_prob_list$n95_mask,
    mask_prob_list$non_med_mask,
    mask_prob_list$no_mask), size = n, replace = TRUE, prob = mask_fraction)

  return(prob_vec) # Return probability vector
}

# Generate interaction matrix (fn1c)
interaction_matrix <- function(n) {
  n <- as.integer(n)
  inter_matrix <- matrix(sample(c(1, 0), replace = TRUE, size = n**2), nrow = n, ncol = n) # Render n*n-sized matrix with values as either 1 or 0
  diag(inter_matrix) <- 0 # Convert all diagnoal items of the matrix (instances where equal row-column index coordinates) to 0
  inter_matrix[lower.tri(inter_matrix)] <- t(inter_matrix)[lower.tri(inter_matrix)] # Transpose lower half of matrix on upper half to make symmetrical

  return(inter_matrix) # Return interaction matrix
}

# Generate person (xi) to person (xj) interactions (fn1d)
xi_to_xj_interactions <- function (initial_inf_stat_vec, inf_prob_vec, interaction_matrix) {


  new_inf_stat_vec <- initial_inf_stat_vec # Assign the initial_inf_stat_vec as the starting conditionsfor interaction

  for (xi_row in 1:nrow(interaction_matrix)) { # Iterate through each row item of the interaction matrix for person xi

    xixj_interactions <- interaction_matrix[xi_row,] # Vector of which xj persons that xi has interacted with, segmented from interaction_matrix
    infected_xj <- xixj_interactions * new_inf_stat_vec # xj persons infected in the xixj_interactions vector, determined via new_inf_stat_vec
    prob_transmission <- (infected_xj * inf_prob_vec) * inf_prob_vec[xi_row] # Probability of transmitting infection to xi from xj interaction (pi * pj)

    sample_prob_transmission <- function(prob_transmission_xi) { # Take an element, then resample it to be either 1 or 0 based on its preprocessed value
      sample(c(1, 0), size=1, replace=TRUE, prob=c(prob_transmission_xi, 1 - prob_transmission_xi))
    }
    deter_inf_transmission <- sapply(prob_transmission, sample_prob_transmission) # Reproduce each value in prob_transmission vec through sample_prob_transmission

    if (sum(deter_inf_transmission) != 0) { # If any infection values in deter_inf_transmission aren't false:
      new_inf_stat_vec[xi_row] <- 1 # Assign 1 (to indicate infection) to inf status vector @ index of xi_row
    }
  }
  return(new_inf_stat_vec) # Return a updated inf status vector, indicates the number of infected people after interaction analysis
}

# Generate iterative infection analysis (fn1e)
iterate_interactions <- function(initial_inf_stat_vec, inf_prob_vec, interaction_matrix, num) {

  updated_inf_stat_vec <- c(initial_inf_stat_vec) # Initialize the first updated vector item (which will be replaced) w/ initial_inf_stat_vec output
  total_inf_vec <- c(sum(updated_inf_stat_vec)) # Initialize total infection count as the sum of the initial_inf_stat_vec output

  for (iteration in 2:(num+1)) { # Iterate from 2 to num
    updated_inf_stat_vec <- xi_to_xj_interactions(updated_inf_stat_vec, inf_prob_vec, interaction_matrix) # Run fn w/ current updated_inf_stat_vec, replace w/ produced output
    total_inf_vec <- c(total_inf_vec, sum(updated_inf_stat_vec)) # For each loop, update the total # infected as the sum of current infected status vec
   }
  return(total_inf_vec) # Return infection count of each iteration
}
