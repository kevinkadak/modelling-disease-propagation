#Infection.Utilities.R
#Kevin Kadak (1007522888)

generate_inf_vec <- function(n, k) { # n is the size of the population; k is the number of initially-infected people within it
  n <- as.integer(n) # Length of vector
  k <- as.integer(k) # Number of infected people within total vector set

  total_pop <- as.numeric(!as.logical(seq(1, n))) # Creates a vector from 1 to n, inversly converts values > 0 as TRUE, then interprets and converts booelan values numerically (either 0 or 1)

  inf_sample <- sample(n, k, replace = FALSE) # Generate an infected sample of size k based.  Only generate unique values up to size n
  inf_status_vec <- replace(total_pop, inf_sample, as.numeric(as.logical(inf_sample))) # Within the total population, replace values of 0 at the indecies defined by inf_sample with values of 1
  return(inf_status_vec)
}


generate_prob_vec <- function(n, mask_fraction = c(1/3, 1/3, 1/3)) {
  n <- as.integer(n)
  if (1 - sum(mask_fraction) > 1e-10 ) {stop("ERROR: Sum of mask fraction vector must total to equal 1.")} # If the sum of the fractions in the vector does not equal 1 (ie. 100%), output error message

  prob_list <- list(
    n95_mask = 0.01,
    non_med_mask = 0.05,
    no_mask = 0.1)

    prob_vec <- sample(c(0.01, 0.05, 0.1), size = n, replace = TRUE, prob = mask_fraction)
    #if (length(prob_vec) != n) {stop("Probability vector does not equal length of total population")}

    return(prob_vec)
}

generate_interaction_matrix <- function(n){
  inter_matrix <- matrix(sample(c(1, 0), replace = TRUE, size = n**2), nrow = n, ncol = n) # Render an n*n-sized matrix, with off-diagonal values equally likely to be either 1 or 0
  diag(inter_matrix) <- 0 # Convert all diagnoal items of the matrix (instances where equal row-column index coordinates) to values of 0
  inter_matrix[lower.tri(inter_matrix)] <- t(inter_matrix)[lower.tri(inter_matrix)] # Transpose the lower half of the matrix on the upper half to make it symmetrical
  return(inter_matrix)
}

one <- generate_inf_vec(12, 4)
two <- generate_prob_vec(12)
three <- generate_interaction_matrix(12)





per2per_interactions <- function (one, two, three) {
  for (xi_row in 1:nrow(three)) { # Iterate through each row item of the interaction matrix
    print (one)
    print (two)
    print (three)

    xixj_interactions <- three[xi_row,]
    cat("xixj_interactions: ", xixj_interactions, "\n")
    infected_xj <- one * xixj_interactions
    cat("infected_xj: ", infected_xj, "\n")
    prob_transmission <- infected_xj *   two
    cat("prob_transmission: ", prob_transmission, "\n")

    filtered_prob_transmission <- prob_transmission[prob_transmission != 0] # Filter probability of transmission vector into only xixj interactions (where infection to xi is possible)
    cat("filtered_prob_transmission: ", filtered_prob_transmission, "\n")
    deter_transmission_vec <- c()
    for (enounter in filtered_prob_transmission) { # Iterate through elements of the probability of transmission vector with xj individual(s)
      deter_transmission <- sample(c(1, 0), size = length(filtered_prob_transmission), replace = TRUE, prob = c(enounter, 1 - enounter))
      deter_transmission_vec <- append(deter_transmission_vec, deter_transmission)
      # FIND A WAY TO USE APPLY HERE MOST LIKELY
    }
    
    cat("deter_transmission_vec: ", deter_transmission_vec, "\n")
    if (sum(deter_transmission_vec) != 0) {xi_inf_status <- TRUE}
    else {xi_inf_status <- FALSE}

    cat("xi_inf_status: ", xi_inf_status, "\n")
    #for (xj_col in 1:ncol(three)) { # Within the above row iteration, iterate through each column item
    #  print(three[xi_row, xj_col])
    #}
  }
}


per2per_interactions (one, two, three)
