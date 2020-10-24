#Infection.Utilities.R
#Kevin Kadak (1007522888)

# Generate initial infection-status vector (fn1a)
initial_inf_stat_vec <- function(n, k) { # n is the size of the population; k is the number of initially-infected people within it
  n <- as.integer(n) # Length of vector
  k <- as.integer(k) # Number of infected people within total vector set

  total_pop <- as.numeric(!as.logical(seq(1, n))) # Vector from 1 to n, inversly converts values > 0 as TRUE, interprets & converts bool values numerically (either 0 or 1)
  inf_sample <- sample(n, k, replace = FALSE) # Generate an infected sample of size k based.  Only generate unique values up to size n
  inf_status_vec <- replace(total_pop, inf_sample, as.numeric(as.logical(inf_sample))) # In n, replace 0 values at indecies defined by inf_sample with values of 1

  return(inf_status_vec)
}

# Generate probability of infection vector (fn1b)
inf_prob_vec <- function(n, mask_fraction = c(1/3, 1/3, 1/3)) {
  n <- as.integer(n)
  if (1 - sum(mask_fraction) > 1e-10 ) {
    stop("Error: Sum of mask fraction vector must total to equal 1.") # If sum of fractions in vector != 1 (ie. 100%), output error
  }

  mask_prob_list <- list(
    n95_mask = 0.01,
    non_med_mask = 0.05,
    no_mask = 0.1)

  prob_vec <- sample(c(
    mask_prob_list$n95_mask,
    mask_prob_list$non_med_mask,
    mask_prob_list$no_mask), size = n, replace = TRUE, prob = mask_fraction)

  return(prob_vec)
}

# Generate interaction matrix (fn1c)
interaction_matrix <- function(n) {
  n <- as.integer(n)
  inter_matrix <- matrix(sample(c(1, 0), replace = TRUE, size = n**2), nrow = n, ncol = n) # Render an n*n-sized matrix, with off-diagonal values equally likely to be either 1 or 0
  diag(inter_matrix) <- 0 # Convert all diagnoal items of the matrix (instances where equal row-column index coordinates) to values of 0
  inter_matrix[lower.tri(inter_matrix)] <- t(inter_matrix)[lower.tri(inter_matrix)] # Transpose the lower half of the matrix on the upper half to make it symmetrical

  return(inter_matrix)
}

# Generate person (xi) to person (xj) interactions (fn1d)
xi_to_xj_interactions <- function (initial_inf_stat_vec, inf_prob_vec, interaction_matrix) {


  new_inf_stat_vec <- initial_inf_stat_vec # Generate an empty vector onto which each infection status of xi can be appended

  #print (initial_inf_stat_vec)
  #print (inf_prob_vec)
  #print (interaction_matrix)

  for (xi_row in 1:nrow(interaction_matrix)) { # Iterate through each row item of the interaction matrix for person xi

    xixj_interactions <- interaction_matrix[xi_row,] # Vector of which xj persons that xi has interacted with, segmented from interaction_matrix
    #cat("xixj_interactions: ", xixj_interactions, "\n")

    infected_xj <- xixj_interactions * new_inf_stat_vec # Vector of xj persons infected in the xixj_interactions vector, determined w/ initial_inf_stat_vec
    #cat("infected_xj: ", infected_xj, "\n")

    prob_transmission <- (infected_xj * inf_prob_vec) * inf_prob_vec[xi_row] # Vector of probability transmitting infection to xi from infected xj persons interacted w/ (pi * pj)
    #cat("prob_transmission: ", prob_transmission, "\n")

    #filtered_prob_transmission <- prob_transmission[prob_transmission != 0] # Filter probability of transmission vector into only xj-infected interactions with xi
    #cat("filtered_prob_transmission: ", filtered_prob_transmission, "\n")

    sample_prob_transmission <- function(prob_transmission_xi) {
      sample(c(1, 0), size=1, replace=TRUE, prob=c(prob_transmission_xi, 1 - prob_transmission_xi))
    }

    deter_inf_transmission <- sapply(prob_transmission, sample_prob_transmission)

    #cat("\n", "deter_inf_transmission: ", deter_inf_transmission, "\n", "\n")

    #print(deter_inf_transmission)
    #deter_inf_transmission <- c()
    #for (enounter in filtered_prob_transmission) { # Iterate through elements of the probability of transmission vector with xj individual(s)
      #deter_transmission <- sample(c(1, 0), size = 1, replace = TRUE, prob = c(enounter, 1 - enounter))
      #print(deter_transmission)
      #deter_inf_transmission <- c(deter_inf_transmission, deter_transmission)
    #}

    #cat("deter_transmission_vec: ", deter_inf_transmission, "\n")

    if (sum(deter_inf_transmission) != 0) { # If any of infection values in determined transmission vector aren't false, xi status = infected
      #xi_inf_status <- 1
      new_inf_stat_vec[xi_row] <- 1
    }
  }
    #cat("xi_inf_status: ", xi_inf_status, "\n",'\n')
  return(new_inf_stat_vec) # Return a vector inidcating the number of infected people (xi) after interaction analysis
}

iterate_interactions <- function(initial_inf_stat_vec, inf_prob_vec, interaction_matrix, num) {

  updated_inf_stat_vec <- c(initial_inf_stat_vec) # Initialize the first updated vector item (which will be replaced) w/ initial_inf_stat_vec output
  total_inf_vec <- c(sum(updated_inf_stat_vec)) # Initialize total infection count as the sum of the initial_inf_stat_vec output


  for (iteration in 2:(num+1)) { # Iterate from 2 to num

    # Run xi_to_xj_interactions w/  current updated_inf_stat_vec, then replace w/ the produced output
    updated_inf_stat_vec <- xi_to_xj_interactions(updated_inf_stat_vec, inf_prob_vec, interaction_matrix)

    total_inf_vec <- c(total_inf_vec, sum(updated_inf_stat_vec)) # For each loop, update the total # infected as the sum of current infected status vec

   }
  return(total_inf_vec)
}
