#Infection.Utilities.R
#Kevin Kadak (1007522888)

generate_inf_vec <- function(n, k) {
  n <- as.integer(n) # Length of vector
  k <- as.integer(k) # Number of infected people within total vector set

  total_pop <- as.numeric(!as.logical(seq(1, n))) # Creates a sequence from 1 to n, inversly converts values > 0 as TRUE, then interprets and converts booelan values numerically (either 0 or 1)

  inf_sample <- sample(n, k, replace = FALSE)
  inf_status_vec <- replace(total_pop, inf_sample, as.numeric(as.logical(inf_sample)))
  #print(jerry)


  return(inf_status_vec) # n is the size of the population; k is the number of initially-infected people
}

generate_inf_vec(12, 4)
