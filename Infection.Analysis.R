args <- commandArgs(trailingOnly = TRUE) # Argument passed from the command line, will be used to indicate masked status

source('Infection.Utilities.R')

print (args)

args <- 'Masked'

#if (is.character(args) == FALSE) { # Output text if the passed command argument is not a string
#  stop("Error: Input is not a string")
#} else if (args != 'Masked' or args != 'Unmasked') { # Output text if the passed argument is not an iteration of 'masked'/'unmasked'
#  stop("Error: Input must be one of the defined strings: 'Masked', 'Unmasked'")}


mask_frac_list <- list(
  Masked = c(0.15, 0.7, 0.15),
  Unmasked = c(0.15, 0.5, 0.35))

if (args == "Masked") {
  mask_fraction <- mask_frac_list$Masked
} else if (args == "Unasked") {
  mask_fraction <- mask_frac_list$Unmasked
}


n_pop = 500
num = 20
k = 1
#  list$args
#  initial_pop <- inital_inf_stat_vec(n, 1)
#  vec.mask <- c(...)
#  'defences'
#  fn1a
#} else ('unmasked') {

#}




######
initial_inf_stat_vec <- initial_inf_stat_vec(n_pop, k) # 3. Create an initial population of 500, with 1 infection
inf_prob_vec <- inf_prob_vec(n_pop , mask_fraction) # 4. Create a vector of infection probabilities using the vectors in mask_frac_list
interaction_matrix <- interaction_matrix(n_pop) # 5. Creates an interaction matrix for the population n

xi_to_xj_interactions(initial_inf_stat_vec, inf_prob_vec, interaction_matrix)

#test_iterate_interactions(initial_inf_stat_vec, inf_prob_vec, interaction_matrix, num) # 6. Iterate through the xixj iterations 20 times and calcualte # of infected
######






# The data points increas sharpley, then plateau as the number of available hosts (people) to infect become less plentifiul as most have already been previously infected

#question: our initial probability vector randomly samples between 1 and 0, which would usally lead to half the values being 1.
  #in the examples, however,


# What do you mean by don't chance the infection status vector from within the function
