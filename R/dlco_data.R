# Internal coefficient data for DLCO reference equations (Kainu et al. 2017)
#
# Equation form:
#   f = exp(a0 + a1*age + a2*ln(age) + a3*(1/height_m) + a4*weight_kg)
#
# SD = (predicted_mean - predicted_LLN) / 1.645
# z-score = (measured - predicted) / SD

.DLCO_VALID_PARAMS <- c("DLCO", "DLCOVA", "VA")

.dlco_coefficients <- list(
  DLCO = list(
    mean  = list("1" = c(a0 = 3.005, a1 = -0.016, a2 = 0.388, a3 = -2.508, a4 = 0.001),
                 "2" = c(a0 = 2.640, a1 = -0.015, a2 = 0.395, a3 = -2.646, a4 = 0.003)),
    lln   = list("1" = c(a0 = 2.686, a1 = -0.017, a2 = 0.401, a3 = -2.510, a4 = 0.001),
                 "2" = c(a0 = 2.341, a1 = -0.015, a2 = 0.402, a3 = -2.649, a4 = 0.003))
  ),
  DLCOVA = list(
    mean  = list("1" = c(a0 = 0.472, a1 = -0.007, a2 = 0, a3 = 0.526, a4 = 0),
                 "2" = c(a0 = -0.527, a1 = -0.006, a2 = 0, a3 = 1.682, a4 = 0.003)),
    lln   = list("1" = c(a0 = 0.213, a1 = -0.007, a2 = 0, a3 = 0.516, a4 = 0),
                 "2" = c(a0 = -0.796, a1 = -0.006, a2 = 0, a3 = 1.675, a4 = 0.003))
  ),
  VA = list(
    mean  = list("1" = c(a0 = 2.853, a1 = -0.009, a2 = 0.379, a3 = -3.391, a4 = 0),
                 "2" = c(a0 = 3.269, a1 = -0.009, a2 = 0.374, a3 = -4.376, a4 = 0)),
    lln   = list("1" = c(a0 = 2.587, a1 = -0.009, a2 = 0.390, a3 = -3.395, a4 = 0),
                 "2" = c(a0 = 3.005, a1 = -0.009, a2 = 0.381, a3 = -4.379, a4 = 0))
  )
)
