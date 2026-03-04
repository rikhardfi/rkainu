# Kainu et al. 2015 reference spirometry values as function
calculate_lung_function <- function(height, age, measured_value, flow_volume_variable, sex) {
  
  # Filter the coefficients_dataset to obtain the correct coefficients for the given flow_volume_variable and sex
  coefficient_row <- coefficients_dataset %>%
    filter(flow_volume_variable == !!flow_volume_variable & sex == !!sex) %>%
    slice(1)
  
  # Check if coefficients are available
  if (nrow(coefficient_row) == 0) {
    warning(paste("No coefficients found for flow_volume_variable:", flow_volume_variable, "and sex:", sex))
    return(NA)
  }
  
  # Extract coefficients from the filtered row
  a0 <- coefficient_row$a0
  a1 <- coefficient_row$a1
  a2 <- coefficient_row$a2
  b0 <- coefficient_row$b0
  b1 <- coefficient_row$b1
  
  # Adjust the age coefficient for FEV1 in males (sex==0) to be negative
  if (flow_volume_variable == "FEV1" && sex == 0) {
    a2 <- -abs(a2)
  }
  
  # Use Mspline and Spline values from the Kainu_adjustments dataset
  adjustment_row <- Kainu_adjustments %>%
    filter(sex == !!sex & flow_volume_variable == !!flow_volume_variable) %>%
    slice(1)
  
  # Check if Mspline and Spline are available
  if (nrow(adjustment_row) == 0) {
    warning(paste("No spline adjustments found for flow_volume_variable:", flow_volume_variable, "and sex:", sex))
    return(NA)
  }
  
  # Extract Mspline and Spline from the adjustment dataset
  Mspline <- adjustment_row$Mspline
  Spline <- adjustment_row$Spline
  
  # Mean prediction (M_i) using natural logarithm (ln)
  M_i <- exp(a0 + a1 * log(height) + a2 * log(age) + Mspline)
  
  # Standard deviation prediction (S_i) using natural logarithm (ln)
  S_i <- exp(b0 + b1 * log(age) + Spline)
  
  # Lower limit of normal (LLN)
  LLN_i <- M_i - 1.645 * S_i
  
  # Z-score calculation
  z_i <- (measured_value - M_i) / S_i
  
  # Return a list with the results
  return(list(M_i = M_i, S_i = S_i, LLN_i = LLN_i, z_i = z_i))
}

