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
  new_inf_stat_vec <- c()
  for (xi_row in 1:nrow(inf_interaction_matrix)) { # Iterate through each row item of the interaction matrix for person xi
    #print (inf_stat_vec)
    #print (inf_prob_vec)
    #print (inf_interaction_matrix)

    xixj_interactions <- inf_interaction_matrix[xi_row,] # Vector of which xj persons that xi has interacted with
    #cat("xixj_interactions: ", xixj_interactions, "\n")

    infected_xj <- inf_stat_vec * xixj_interactions # Vector of which xj persons in the xixj_interactions vector are infected, determined via w/ generate_inf_vec
    #cat("infected_xj: ", infected_xj, "\n")

    prob_transmission <- infected_xj * (inf_prob_vec) # Among the infected xj person(s) in infected_xj, determine their probability of transmitting infection to xi (pi * pj)
    #cat("prob_transmission: ", prob_transmission, "\n")

    filtered_prob_transmission <- prob_transmission[prob_transmission != 0] # Filter probability of transmission vector into only xj-infected interactions with xi
    #cat("filtered_prob_transmission: ", filtered_prob_transmission, "\n")

    #deter_inf_transmission <- sapply(prob_transmission, sample(c(1, 0), size=1, replace=TRUE, prob=c(prob_transmission, 1 - prob_transmission)))
    deter_inf_transmission <- c()
    for (enounter in filtered_prob_transmission) { # Iterate through elements of the probability of transmission vector with xj individual(s)
      deter_transmission <- sample(c(1, 0), size=1, replace=TRUE, prob=c(enounter, 1 - enounter))
      deter_inf_transmission <- append(deter_inf_transmission, deter_transmission)
    }

    #cat("deter_transmission_vec: ", deter_inf_transmission, "\n")
    if (sum(deter_inf_transmission) != 0) {xi_inf_status <- 1} # If any of the infection values in determined transmission vector are not false, xi status = infected
    else {xi_inf_status <- 0}
    new_inf_stat_vec <- append(new_inf_stat_vec, xi_inf_status)
    #print (new_inf_stat_vec)

    #cat("xi_inf_status: ", xi_inf_status, "\n")


    #for (xj_col in 1:ncol(three)) { # Within the above row iteration, iterate through each column item
    #  print(three[xi_row, xj_col])
    #}
  }
  #print (new_inf_stat_vec)
  return(new_inf_stat_vec) # Return a vector inidcating the number of infected people (xi) after interaction analysis
}


initial_inf_stat_vec <- inital_inf_stat_vec(25, 1)
inf_prob_vec <- inf_prob_vec(25)
inf_interaction_matrix <- interaction_matrix(25)


test_iterate_interactions <- function(initial_inf_stat_vec, inf_prob_vec, inf_interaction_matrix, num) {
   # Initialize an empty vector to contain each iteration of infectious status vector (both initial and updated)
  total_inf_vec <- c() # Initialize an empty vector to contain the total number of infected people

  first_run <- initial_inf_stat_vec

  for (i in 1:num) {
    gg <- (xi_to_xj_interactions(first_run, inf_prob_vec, inf_interaction_matrix))
    print(gg)
    total_inf_vec <- c(total_inf_vec, sum(gg))

  }
  print(total_inf_vec)
}

iterate_interactions <- function(initial_inf_stat_vec, inf_prob_vec, inf_interaction_matrix, num) { # 1e takes the initial vector, but will use the updated vector as it iterates
  storage <- list() # Initialize an empty vector to contain each iteration of infectious status vector (both initial and updated)
  total_inf_vec <- c() # Initialize an empty vector to contain the total number of infected people

  first_run <- initial_inf_stat_vec
  #print (first_run)

  storage[1] <- list(first_run)
  #storage[2] <- list(first_run + 2)
  #storage[3] <- list(first_run * 9)

  total_inf_vec <- c(total_inf_vec, sum(first_run))
  print (storage[length(storage)])
  print (tail(storage))

  #last_storage_elmt <- unlist(storage[length(storage)]

  #total_inf_vec <- append(total_inf_vec, sum(storage))
  print(total_inf_vec)



  #aa <- c(1,0,0,1,0)
  #bb <- c(0,1,1,1,0)
  #aa <- (aa | bb)
  #wow <- mapply(max, aa, bb)
  #print(wow)



  for (inf_scenario in 1:num) {

    iterable_isv <- xi_to_xj_interactions(unlist(tail(storage)), inf_prob_vec, inf_interaction_matrix)
    storage[length(storage) + 1] <- mapply(max, storage[length(storage)], iterable_isv)
    #print (storage[length(storage)])
    ###storage[length(storage) + 1] <- unlist(storage[length(storage)]) + iterable_isv[iterable_isv != unlist(storage[length(storage)])] # Append updated infection status vector to storage list
    #print (iterable_isv)
    total_inf_vec <- c(total_inf_vec, sum(iterable_isv))


  }
  print(total_inf_vec)
}

#    storage[inf_scenario] <- list(iterable_isv)
    #jj <- xi_to_xj_interactions(initial_inf_stat_vec, inf_prob_vec, inf_interaction_matrix)


    #updated_inf_stat_vec <- storage[length(storage)] * jj[jj != storage[length(storage)]]

  #  append(storage, updated_inf_stat_vec)
  #  append(total_inf_vec, sum(storage[length(storage)]))
  #}

  #return (total_inf_vec)

    #  iterable_isv <- xi_to_xj_interactions(iterable_isv + storage[length(storage)], inf_prob_vec, inf_interaction_matrix)
    #  total_inf_vec <- c(total_inf_vec, total_inf_vec[length(total_inf_vec)] + sum(iterable_isv))
    #  cat(inf_scenario, " run", iterable_isv, '\n')
    #}



  #cat("first run", iterable_isv, '\n')
  #total_inf_vec <- c(total_inf_vec, sum(iterable_isv))
  #total_inf_vec <- append(total_inf_vec, sum(iterable_isv)) # Append sum of infected persons in the interaction matrix analysis sourcing the initial_inf_stat_vec
  #total_inf_vec <- append(storage, iterable_isv)

  #for (inf_scenario in 1:(num + 1)) {
  #  print (inf_scenario)
  #  if (inf_scenario == 1) {
  #    first_run <- xi_to_xj_interactions(initial_inf_stat_vec, inf_prob_vec, inf_interaction_matrix)
  #    append(storage, first_run)
  #    append(total_inf_vec, sum(storage))

  #  } else {
  #    updated_inf_stat_vec <- storage[length(storage)] * initial_inf_stat_vec[initial_inf_stat_vec != storage[length(storage)]]
  #    cat ('\n', 'updated_inf_stat_vec', updated_inf_stat_vec)

  #    iterable_isv <- xi_to_xj_interactions(updated_inf_stat_vec, inf_prob_vec, inf_interaction_matrix) #Iterable infection status vector that will be updated through 1:num
  #    total_inf_vec <- append(total_inf_vec, total_inf_vec[length(total_inf_vec)] + sum(iterable_isv))

  #    cat(inf_scenario, " run", iterable_isv, '\n')
  #  }
  #}

  #print(total_inf_vec)




  #}
    #my_new_vec <- fn1d(my_new_vec)


    #jj <- sum(xi_to_xj_interactions(initial_inf_stat_vec, inf_prob_vec, inf_interaction_matrix))
    #total_inf_vec <- append(total_inf_vec, jj)
    #total_inf_vec <- append(total_inf_vec, sapply(xi_to_xj_interactions(initial_inf_stat_vec, inf_prob_vec, inf_interaction_matrix), FUN = sum))




  #for (inf_scenario in 1:num) {
    #jj <- sapply(iterable_uisv, sum)
    #total_inf_vec <- append(total_inf_vec, jj)

    #total_inf_vec <- append(total_inf_vec, sum(iterable_uisv))
    #total_inf_vec <- append(total_inf_vec, sapply(iterable_uisv, sum))
  #print(total_inf_vec)
#}
  #if (length(total_inf_vec) != num + 1) {cat("weird.", "length: ", length(total_inf_vec), total_inf_vec)}

  #return(total_inf_vec)






#xi_to_xj_interactions(initial_inf_stat_vec, inf_prob_vec, inf_interaction_matrix)
test_iterate_interactions(initial_inf_stat_vec, inf_prob_vec, inf_interaction_matrix, 20)
