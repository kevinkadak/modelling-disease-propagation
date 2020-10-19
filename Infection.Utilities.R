#Infection.Utilities.R
#Kevin Kadak (1007522888)

generate_inf_vec <- function(n, k) { # n is the size of the population; k is the number of initially-infected people
  n <- as.integer(n) # Length of vector
  k <- as.integer(k) # Number of infected people within total vector set

  total_pop <- as.numeric(!as.logical(seq(1, n))) # Creates a vector from 1 to n, inversly converts values > 0 as TRUE, then interprets and converts booelan values numerically (either 0 or 1)

  inf_sample <- sample(n, k, replace = FALSE) # Generate an infected sample of size k based.  Only generate unique values up to size n
  inf_status_vec <- replace(total_pop, inf_sample, as.numeric(as.logical(inf_sample))) # Within the total population, replace values of 0 at the indecies defined by inf_sample with values of 1
  return(inf_status_vec)
}


generate_prob_vec <- function(n, mask_fraction = c(1/3, 1/3, 1/3)) {
  n <- as.integer(n)
  if (1 - sum(mask_fraction) > 1e-10 ) {stop("ERROR: Sum of mask fraction vector must be = 1 to the 10th decimal place")} # If the sum of the fractions in the vector does not equal 1 (ie. 100%), output error message

  prob_list <- list(
    n95_mask = 0.01,
    non_med_mask = 0.05,
    no_mask = 0.1)

    prob_vec <- sample(c(0.01, 0.05, 0.1), size = n, replace = TRUE, prob = mask_fraction)
    #if (length(prob_vec) != n) {stop("Probability vector does not equal length of total population")}

    return(prob_vec)
}

generate_interaction_matrix <- function(n){
  inter_matrix <- matrix(sample(c(1, 0), replace=TRUE, size=n), nrow = n, ncol = n)
  diag(inter_matrix) <- 0
  print(inter_matrix)
  #A[lower.tri(A)] <- t(A)[lower.tri(A)]

}

#print (A[lower.tri(A)] <- t(A)[lower.tri(A)])


generate_prob_vec(12)

generate_interaction_matrix(12)
