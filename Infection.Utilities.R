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


prob_vec <- function(n, mask_fraction = c(1/3, 1/3, 1/3)) {

  prob_list <- list(
    n95 = 0.01,
    non_med = 0.05,
    no_mask = 0.1)

    if length(VECTOR) != n:
      stop("Probability vector does not equal length of total population")


    return()
}

generate_inf_vec(12, 4)
