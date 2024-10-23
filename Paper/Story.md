# Story

## Different parameter sets

Figure Nr.1 showing the measured signal values.
Moreover, five different optimization results are shown.
Below are the four different parameter depicted as boxplots.
Figure Nr.1 a, b and c show that the trajectories of the model 
are very similar for different parameter sets.

## Parameter space instead of a single value

Figure Nr.2 3D scatter plot showing the parameter space 
of very good parameter sets. This plot shows that there is
not a single value but instead it is an entire region
containing the optimal parameter sets.
=> The result is not a specific value but rather this region itself,
which was not fully explored using local optimization
=> Showing a hard boundary of the result region
=> Showing that the results of an optimization are not random

## Sensitivity analysis

=> each model (including polynome and concentrations)
  has restrictive parameters which are crucial during optimization
=> or are specific parameters crucial based on the model and the concentrations
are not relevant?
=> Shows how stable the region is

# Cluster analysis

Run clustering algorithms (e.g., k-means or PCA) on the parameter sets
showing good fits to see if natural groups exist. 

# The impact of measurement noise

Compare boxplots of measurements with the boxplots of predicted values

# Root analysis

Use of optimized parameters and the additional parameters concentrations of e.g. guest) to calc the roots of the polynoms defined in the loss functions. 

- Do Bifurcations exist (sudden appearance/disappearance of multiple roots)
- Is handling of multiple roots correct? 
