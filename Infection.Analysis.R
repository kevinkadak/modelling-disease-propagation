#Infection.Utilities.R
#Kevin Kadak (1007522888)

source('Infection.Utilities.R') # Import Infection.Utilities.R functions

# Bash/terminal data & defensive guards
args <- commandArgs(trailingOnly = TRUE[1]) # Argument passed from the command line, will be used to indicate masked status

if (length(args) == 0) {
  stop("No condition passed.  Input Masked or Unmasked.")
} else if (is.character(args) == FALSE) { # Output text if the passed command argument is not a string
  stop("Input is not a string")
} else if ((args != "Masked" && args != "Unmasked") == TRUE) { # Output text if the passed argument is not an iteration of 'masked'/'unmasked'
  stop("Input must be one of the defined strings: 'Masked', 'Unmasked'")
}


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
cat("Using the", tolower(unlist(args)), "proprtions vector.\n")
cat("The number of infected people, after", inf_param_list$num, "iterations, is: \n")

initial_inf_stat_vec <- initial_inf_stat_vec(inf_param_list$n_pop, inf_param_list$k) # Create initial population of n, with k infection(s)
inf_prob_vec <- inf_prob_vec(inf_param_list$n_pop, mask_fraction) # Create vector of infection probs using mask_frac_list
interaction_matrix <- interaction_matrix(inf_param_list$n_pop) # Create interaction matrix of the population n

iterate_interactions(initial_inf_stat_vec, inf_prob_vec, interaction_matrix, inf_param_list$num) # Iterate through xixj iteractions and, num times, calcualte # of infected


## Insights on Resulting Values ##
##################################

# Similarities:
# Both conditions ("Masked"/"Unmasked") produced roughly the same number of infected persons at the end of num iterations, demonstrating that mask wearing
# does not affect final volumetric infection rates without intervention.  Iterations points showed an initially-small increase, followed by a more
# significant increase before plateauing and decreasing in the number of new infections to follow.
#
# Differences:
# The key difference between the model conditions is the quadratic measure of infection rates.  That is, how dramatically infection rates between a previous
# and given iteration of the model increases or decreases.  In the Unmasked condition, interaction is comparatively more likely to result in infection,
# meaning the number of people infected occurs quickly while there is a large sample through which to spread, then just as quickly slows as the number of
# uninfected 'hosts' dwindles.  This differs from the Masked condition, which demonstrates a more gradual spread of infection, resulting in less of a
# spike in cases.
#
# If number of infections for both conditions were plotted on a graph, with x representing iterations and y representing the number of infections per, it
# would form a normal distrution bell curve. The kurtosis would be comparatively smaller in the Masked condition, though it would show a visually wider
# spread of data.
#
# This is a particularily important demonstration of why social distancing is currently of utmost importance.  Even if every person was bound to
# eventually become COVID-19 positive, the number of results deaths and severity of conseqeuntial side effects would be far greater were the medical
# system too overwhelmed to properly treat everyone in a short period.
#
# Model changes:
# A sensible, yet not overly complex implemention for the model would be to account for 'removal', either from infected individuals recovering over a
# period of time (which could easily be guaged as a measure of num iteration) or deaths, in which case xi is removed, perhaps triggering after some number
# of iterations without recovery.
