#Infection.Utilities.R
#Kevin Kadak (1007522888)

# Generate initial infection-status vector
inital_inf_stat_vec <- function(n, k) { # n is the size of the population; k is the number of initially-infected people within it
  n <- as.integer(n) # Length of vector
  k <- as.integer(k) # Number of infected people within total vector set

  total_pop <- as.numeric(!as.logical(seq(1, n))) # Creates a vector from 1 to n, inversly converts values > 0 as TRUE, then interprets and converts booelan values numerically (either 0 or 1)

  inf_sample <- sample(n, k, replace = FALSE) # Generate an infected sample of size k based.  Only generate unique values up to size n
  inf_status_vec <- replace(total_pop, inf_sample, as.numeric(as.logical(inf_sample))) # Within the total population, replace values of 0 at the indecies defined by inf_sample with values of 1
  return(inf_status_vec)
}

# Generate probability of infection vector
inf_prob_vec <- function(n, mask_fraction = c(1/3, 1/3, 1/3)) {
  n <- as.integer(n)
  if (1 - sum(mask_fraction) > 1e-10 ) {stop("ERROR: Sum of mask fraction vector must total to equal 1.")} # If the sum of the fractions in the vector does not equal 1 (ie. 100%), output error message

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

# Generate interaction matrix
interaction_matrix <- function(n){
  inter_matrix <- matrix(sample(c(1, 0), replace = TRUE, size = n**2), nrow = n, ncol = n) # Render an n*n-sized matrix, with off-diagonal values equally likely to be either 1 or 0
  diag(inter_matrix) <- 0 # Convert all diagnoal items of the matrix (instances where equal row-column index coordinates) to values of 0
  inter_matrix[lower.tri(inter_matrix)] <- t(inter_matrix)[lower.tri(inter_matrix)] # Transpose the lower half of the matrix on the upper half to make it symmetrical
  return(inter_matrix)
}

xi_to_xj_interactions <- function (inf_stat_vec, inf_prob_vec, inf_interaction_matrix) {
  updated_inf_stat_vec <- c()
  for (xi_row in 1:nrow(inf_interaction_matrix)) { # Iterate through each row item of the interaction matrix for person xi
    print (inf_stat_vec)
    print (inf_prob_vec)
    print (inf_interaction_matrix)

    xixj_interactions <- inf_interaction_matrix[xi_row,] # Vector of which xj persons that xi has interacted with
    cat("xixj_interactions: ", xixj_interactions, "\n")

    infected_xj <- inf_stat_vec * xixj_interactions # Vector of which xj persons in the xixj_interactions vector are infected, determined via w/ generate_inf_vec
    cat("infected_xj: ", infected_xj, "\n")

    prob_transmission <- infected_xj * (inf_prob_vec) # Among the infected xj person(s) in infected_xj, determine their probability of transmitting infection to xi (pi * pj)
    cat("prob_transmission: ", prob_transmission, "\n")

    filtered_prob_transmission <- prob_transmission[prob_transmission != 0] # Filter probability of transmission vector into only xj-infected interactions with xi
    cat("filtered_prob_transmission: ", filtered_prob_transmission, "\n")

    #deter_inf_transmission <- sapply(prob_transmission, sample(c(1, 0), size=1, replace=TRUE, prob=c(prob_transmission, 1 - prob_transmission)))
    deter_inf_transmission <- c()
    for (enounter in filtered_prob_transmission) { # Iterate through elements of the probability of transmission vector with xj individual(s)
      deter_transmission <- sample(c(1, 0), size=1, replace=TRUE, prob=c(enounter, 1 - enounter))
      deter_inf_transmission <- append(deter_inf_transmission, deter_transmission)
    }

    cat("deter_transmission_vec: ", deter_inf_transmission, "\n")
    if (sum(deter_inf_transmission) != 0) {xi_inf_status <- 1} # If any of the infection values in determined transmission vector are not false, xi status = infected
    else {xi_inf_status <- 0}
    updated_inf_stat_vec <- append(updated_inf_stat_vec, xi_inf_status)

    cat("xi_inf_status: ", xi_inf_status, "\n")
    #for (xj_col in 1:ncol(three)) { # Within the above row iteration, iterate through each column item
    #  print(three[xi_row, xj_col])
    #}
  }
  return(updated_inf_stat_vec)
}

initial_inf_stat_vec <- inital_inf_stat_vec(12, 4)
inf_prob_vec <- inf_prob_vec(12)
inf_interaction_matrix <- interaction_matrix(12)

iterate_interactions <- function(initial_inf_stat_vec, inf_prob_vec, inf_interaction_matrix, num) { # 1e takes the initial vector, but will use the updated vector as it iterates
  iterable_uisv <- xi_to_xj_interactions(initial_inf_stat_vec, inf_prob_vec, inf_interaction_matrix)
  cat(iterable_uisv, '\n')

  total_inf_vec <- c()
  total_inf_vec <- append(total_inf_vec, sum(iterable_uisv))
  print(total_inf_vec)

  for (inf_scenario in 1:num) {
    my_new_vec <- fn1d(my_new_vec)


    jj <- sum(xi_to_xj_interactions(initial_inf_stat_vec, inf_prob_vec, inf_interaction_matrix))
    total_inf_vec <- append(total_inf_vec, jj)
    #total_inf_vec <- append(total_inf_vec, sapply(xi_to_xj_interactions(initial_inf_stat_vec, inf_prob_vec, inf_interaction_matrix), FUN = sum))
  }
  print(total_inf_vec)

  #for (inf_scenario in 1:num) {
    #jj <- sapply(iterable_uisv, sum)
    #total_inf_vec <- append(total_inf_vec, jj)

    #total_inf_vec <- append(total_inf_vec, sum(iterable_uisv))
    #total_inf_vec <- append(total_inf_vec, sapply(iterable_uisv, sum))
  #print(total_inf_vec)
#}
  #if (length(total_inf_vec) != num + 1) {print(total_inf_vec)}

  #return(total_inf_vec)
}



xi_to_xj_interactions(initial_inf_stat_vec, inf_prob_vec, inf_interaction_matrix)
#iterate_interactions(initial_inf_stat_vec, inf_prob_vec, inf_interaction_matrix, 20)
