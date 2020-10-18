#Infection.Utilities.R
#Kevin Kadak (1007522888)

inf_stat_vec <- function(n, k) {
  #n <- as.integer(n) # Length of vector
  #k <- as.integer(k) # Number of infected people within total vector set

  infected_in_vector <- sample(n, size = k)

  patty <- seq(1, n)
  return(patty) # n is the size of the population; k is the number of initially-infected people
}

inf_stat_vec(12, 3)
