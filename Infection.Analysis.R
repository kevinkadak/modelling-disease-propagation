source('Infection.Utilities.R') # Import Infection.Utilities.R functions

# Bash/terminal data & defensive guards
args <- commandArgs(trailingOnly = TRUE[1]) # Argument passed from the command line, will be used to indicate masked status

if (is.character(args) == FALSE) { # Output text if the passed command argument is not a string
  stop("Error: Input is not a string")
} else if ((args != "Masked" || args != "Unmasked") == FALSE) { # Output text if the passed argument is not an iteration of 'masked'/'unmasked'
  stop("Error: Input must be one of the defined strings: 'Masked', 'Unmasked'") }


# Mask data & defensive guards
mask_frac_list <- list(
  Masked = c(0.15, 0.7, 0.15),
  Unmasked = c(0.15, 0.5, 0.35))

if (args == "Masked") {
  mask_fraction <- mask_frac_list$Masked
} else if (args == "Unmasked") {
  mask_fraction <- mask_frac_list$Unmasked
}


# Infection parameter data & defensive guards
inf_param_list <- list(
  n_pop = 500,
  num = 20,
  k = 1)

if (inf_param_list$n_pop < 1) {
  stop("Error: Population (n_pop) must be greater than or equal to 1")
} else if (inf_param_list$num < 1) {
  stop("Error: # of interaction iterations (num) must be greater than or equal to 1")
} else if (inf_param_list$k < 1) {stop("Error: # of infected persons (k) must be greater than or equal to 1")}


# Output statements & function calls
cat("Using the", unlist(args), "proprtions vector.\n")
cat("The number of infected people, after", inf_param_list$num, "iterations, is: \n")

initial_inf_stat_vec <- initial_inf_stat_vec(inf_param_list$n_pop, inf_param_list$k) # Create initial population of n, with k infection(s)
inf_prob_vec <- inf_prob_vec(inf_param_list$n_pop, mask_fraction) # Create vector of infection probs using mask_frac_list
interaction_matrix <- interaction_matrix(inf_param_list$n_pop) # Create interaction matrix of the population n

iterate_interactions(initial_inf_stat_vec, inf_prob_vec, interaction_matrix, inf_param_list$num) # Iterate through xixj iteractions and, num times, calcualte # of infected



#, such that if number of infections were plotted on a graph, with y representing the number of infections and x representing iterations, it would form a normal distrution bell curve.
# The key difference here is that the spikes in infection cases is sharper in the unmasked situation as the same amount of interaction leads leads to cases more suddenly

#This is a particularily important demonstration of why social distancing is of utmost importance currently.  Even if every person was preordained to eventually be COVID-19 positive, the number of results deaths and severity of conseqeuntial side effects would be far greater were the medical system too overwhelmed to properly treat everyone in a short period.



# The data points increas sharpley, then plateau as the number of available hosts (people) to infect become less plentifiul as most have already been previously infected

#question: our initial probability vector randomly samples between 1 and 0, which would usally lead to half the values being 1.
  #in the examples, however,


# What do you mean by don't chance the infection status vector from within the function
