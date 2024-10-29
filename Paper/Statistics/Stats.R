load("DenseGrid.RData")

library(mgcv)
library(ggplot2)

# generalised additive model
stats_fct <- function(case, df) {
  formula <- formula(errors ~ s(KaHD) + s(I0) + s(IHD) + s(ID))
  if (case == "ida" || case == "gda") {
    formula <- formula(errors ~ s(KaHG) + s(I0) + s(IHD) + s(ID))
  }
  gam(formula, data = df)
}

# NOTE: Examine Smooth Terms: estimated degrees of freedom (edf)
# Higher edf values indicate more complexity => non-linearity
dba_m <- stats_fct("dba", dba)
summary(dba_m)
ida_m <- stats_fct("ida", ida)
summary(ida_m)
gda_m <- stats_fct("gda", gda)
summary(gda_m)

# NOTE: Non-linear patterns in these plots
# support the idea that the relationships
# are not merely linear.
par(mfrow = c(2, 2))
plot(dba_m)
plot(ida_m)
plot(gda_m)
