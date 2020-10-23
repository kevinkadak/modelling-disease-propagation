#Infection.Utilities.R
#Kevin Kadak (1007522888)

# Generate initial infection-status vector (fn1a)
initial_inf_stat_vec <- function(n, k) { # n is the size of the population; k is the number of initially-infected people within it
  n <- as.integer(n) # Length of vector
  k <- as.integer(k) # Number of infected people within total vector set

  total_pop <- as.numeric(!as.logical(seq(1, n))) # Creates a vector from 1 to n, inversly converts values > 0 as TRUE, then interprets and converts booelan values numerically (either 0 or 1)
  inf_sample <- sample(n, k, replace = FALSE) # Generate an infected sample of size k based.  Only generate unique values up to size n
  inf_status_vec <- replace(total_pop, inf_sample, as.numeric(as.logical(inf_sample))) # Within the total population, replace values of 0 at the indecies defined by inf_sample with values of 1

  return(inf_status_vec)
}

# Generate probability of infection vector (fn1b)
inf_prob_vec <- function(n, mask_fraction = c(1/3, 1/3, 1/3)) {
  n <- as.integer(n)
  if (1 - sum(mask_fraction) > 1e-10 ) {stop("Error: Sum of mask fraction vector must total to equal 1.")} # If the sum of the fractions in the vector does not equal 1 (ie. 100%), output error message

  mask_prob_list <- list(
    n95_mask = 0.01,
    non_med_mask = 0.05,
    no_mask = 0.1)

  prob_vec <- sample(c(
    mask_prob_list$n95_mask,
    mask_prob_list$non_med_mask,
    mask_prob_list$no_mask), size = n, replace = TRUE, prob = mask_fraction)
    #if (length(prob_vec) != n) {stop("Probability vector does not equal length of total population")}
  return(prob_vec)
}

# Generate interaction matrix (fn1c)
interaction_matrix <- function(n) {
  inter_matrix <- matrix(sample(c(1, 0), replace = TRUE, size = n**2), nrow = n, ncol = n) # Render an n*n-sized matrix, with off-diagonal values equally likely to be either 1 or 0
  diag(inter_matrix) <- 0 # Convert all diagnoal items of the matrix (instances where equal row-column index coordinates) to values of 0
  inter_matrix[lower.tri(inter_matrix)] <- t(inter_matrix)[lower.tri(inter_matrix)] # Transpose the lower half of the matrix on the upper half to make it symmetrical

  return(inter_matrix)
}

# Generate person (xi) to person (xj) interactions (fn1d)
xi_to_xj_interactions <- function (initial_inf_stat_vec, inf_prob_vec, interaction_matrix) {
  new_inf_stat_vec <- c()
  inter_mat <- interaction_matrix

  for (xi_row in 1:nrow(inter_mat)) { # Iterate through each row item of the interaction matrix for person xi
    #print (initial_inf_stat_vec)
    #print (inf_prob_vec)
    #print (interaction_matrix)

    xixj_interactions <- inter_mat[xi_row,] # Vector of which xj persons that xi has interacted with
    #cat("xixj_interactions: ", xixj_interactions, "\n")

    infected_xj <- initial_inf_stat_vec * xixj_interactions # Vector of which xj persons in the xixj_interactions vector are infected, determined via w/ generate_inf_vec
    #cat("infected_xj: ", infected_xj, "\n")

    prob_transmission <- infected_xj * inf_prob_vec # Among the infected xj person(s) in infected_xj, determine their probability of transmitting infection to xi (pi * pj)
    #cat("prob_transmission: ", prob_transmission, "\n")

    filtered_prob_transmission <- prob_transmission[prob_transmission != 0] # Filter probability of transmission vector into only xj-infected interactions with xi
    #at("filtered_prob_transmission: ", filtered_prob_transmission, "\n")

    #deter_inf_transmission <- sapply(prob_transmission, sample(c(1, 0), size=1, replace=TRUE, prob=c(prob_transmission, 1 - prob_transmission)))
    deter_inf_transmission <- c()
    for (enounter in filtered_prob_transmission) { # Iterate through elements of the probability of transmission vector with xj individual(s)
      deter_transmission <- sample(c(1, 0), size=1, replace=TRUE, prob=c(enounter, 1 - enounter))
      #print(deter_transmission)
      deter_inf_transmission <- c(deter_inf_transmission, deter_transmission)
    }

    #cat("deter_transmission_vec: ", deter_inf_transmission, "\n")

    if (sum(deter_inf_transmission) != 0) {xi_inf_status <- 1} # If any of the infection values in determined transmission vector are not false, xi status = infected
    else {xi_inf_status <- 0}
    new_inf_stat_vec <- c(new_inf_stat_vec, xi_inf_status)
    #print (new_inf_stat_vec)

    #cat("xi_inf_status: ", xi_inf_status, "\n",'\n')
  }
  return(new_inf_stat_vec) # Return a vector inidcating the number of infected people (xi) after interaction analysis
}


#initial_inf_stat_vec <- initial_inf_stat_vec(20, 3)
#inf_prob_vec <- inf_prob_vec(20)
#interaction_matrix <- interaction_matrix(20)


test_iterate_interactions <- function(initial_inf_stat_vec, inf_prob_vec, interaction_matrix, num) {

  updated_inf_stat_vec <- list()
  total_inf_vec <- c()

  updated_inf_stat_vec[[1]] <- initial_inf_stat_vec # first updated infection status vector item
  total_inf_vec <- c(total_inf_vec, sum(updated_inf_stat_vec[[1]])) # first total # of infected item

  print (updated_inf_stat_vec)

  #cat('total_inf_vec', total_inf_vec[[1]], '\n')

  for (i in 2:(num+1)) {
    #print (i)
    #print(updated_inf_stat_vec[[i - 1]])
    updated_inf_stat_vec[[i]] <- xi_to_xj_interactions(updated_inf_stat_vec[[i - 1]], inf_prob_vec, interaction_matrix)

    print(updated_inf_stat_vec[[i]])


    #print(sum(updated_inf_stat_vec[[i - 1]]))
    total_inf_vec <- c(total_inf_vec, sum(updated_inf_stat_vec[[i - 1]]) + total_inf_vec[[i - 1]])



    #total_inf_vec[[i]] <- sum(updated_inf_stat_vec[[i]]) + total_inf_vec[[i - 1]] # sum the number of infected people and save it
    #print(total_inf_vec[[i]])
    #updated_inf_stat_vec[[length(updated_inf_stat_vec)]] <- gg # save the new infection status vector to be used in the next loop

   }
  return(total_inf_vec)
}




#xi_to_xj_interactions(initial_inf_stat_vec, inf_prob_vec, interaction_matrix)
#test_iterate_interactions(initial_inf_stat_vec, inf_prob_vec, interaction_matrix, 20)
