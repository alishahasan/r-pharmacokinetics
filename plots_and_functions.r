# values of variables used in analysis of Tylenol dosing chart
drug_dosage = 160
k_a = 0.35
k_e = 0.27
K = k_a/(k_a - k_e)

# defining absorption as a function of time (A(t))
absorption <- function(time) {
  return( drug_dosage * exp(-k_a*time) )
}


# defining elimination as a function of time (E(t))
elimination <- function(time) {
  return((drug_dosage * K) * ((exp(-k_e*time)) - (exp(-k_a*time))))
}

# defining the derivative of absorption as a function of time (A'(t))
elimderiv <- function(time) {
  return(drug_dosage * K * (-k_e*exp(-k_e*time) + k_a*exp(-k_a*time)))
}

# defining the derivative of elimination as a function of time (E'(t))
absorpderiv <- function(time) {
  return(drug_dosage * -k_a * exp(-k_a * time))
}

# defining the elimination of 400 mg of ibuprofen as a function of time in order to determine dosing intervals. this function was not graphed; the dosing interval was determined by generating a table of values in the console, which was manually done and is not coded here
E <- function(time) {
  return(2000 * (exp(-0.4*time) - exp(-0.5*time)))
}


# library calling for packages that contain graphing functions
library(tidyverse)
require(ggthemes)

# graphing + designing the absorption over time graph
ggplot() + 
  stat_function(fun = absorption, geom = 'line', colour = '#b04ede', size = 0.5) + 
  stat_function(fun = absorption, geom = 'point', size = 1) + 
  xlim(0, 240) + theme_minimal() + 
  labs(title = "Amount of Drug in Stomach",
       x = "Time (m)",
       y = "Amount in Stomach (mg)") +
  theme(
    axis.title = element_text(family = 'Helvetica', size = 14, color = 'black'),
    axis.text = element_text(family = 'Helvetica', size = 14, color = 'black'),
    plot.title = element_text(family = 'Helvetica', size = 20, color = 'black')
  )

# graphing + designing the elimination over time graph
ggplot() + 
  stat_function(fun = elimination, geom = 'line', colour = '#b04ede', size = 0.5) +
  stat_function(fun = elimination, geom = 'point') +
  xlim(0, 240) + theme_minimal() +
  labs(title = "Amount of Drug in Blood Stream",
       x = "Time (m)",
       y = "Amount in Blood Stream (mg)") + 
    theme(
      axis.title = element_text(family = 'Helvetica', size = 14, color = 'black'),
      axis.text = element_text(family = 'Helvetica', size = 14, color = 'black'),
      plot.title = element_text(family = 'Helvetica', size = 20, color = 'black')
    )

# graphing + designing the derivative of elimination
ggplot() + 
  stat_function(fun = elimderiv, geom = 'line', color = 'red', size = 0.5) +
  stat_function(fun = elimderiv, geom = 'point') +
  xlim(0, 240) + theme_minimal() +
  labs(title = "Rate of Elimination",
       x = "Time (m)",
       y = "E'(t)") + 
  theme(
    axis.title = element_text(family = 'Helvetica', size = 14, color = 'black'),
    axis.text = element_text(family = 'Helvetica', size = 14, color = 'black'),
    plot.title = element_text(family = 'Helvetica', size = 20, color = 'black')
  )

# graphing + designing the derivative of absorption
ggplot() + 
  stat_function(fun = absorpderiv, geom = 'line', color = 'red', size = 0.5) +
  stat_function(fun = absorpderiv, geom = 'point') +
  xlim(0, 240) + theme_minimal() +
  labs(title = "Rate of Absorption",
       x = "Time (m)",
       y = "A'(t)") + 
  theme(
    axis.title = element_text(family = 'Helvetica', size = 14, color = 'black'),
    axis.text = element_text(family = 'Helvetica', size = 14, color = 'black'),
    plot.title = element_text(family = 'Helvetica', size = 20, color = 'black')
  )
