# From Single Best-Fit to Parameter Landscapes: Rethinking Binding Constant Estimation from Titration Experiments

## Parameter variability

For the three models DBA, IDA and GDA 10 simulations were conducted to identify the variability of the resulting parameters (see *DecentFitParameterVariance/FigNr1_Run_SimulationsR*). Nice plots can be created using the script *DecentFitParameterVariance/FigNr1_Visualisation.R*.

## Sensitivity and parameter space exploration

A Sobol Sensitivity analysis was conducted (see ./Sensitivity/Sensitivity.R).
Moreover, a grid was created using all four parameters of each model. Next, the error was calculated for each entry of the grid (./Regions/3DPlotsDataGeneration.R). The obtained information was depicted in 3D plots showing
the impact of the two most important parameters (identified in the Sensitivity analysis) on the error (./Regions/FigNr3.R).

## Parameter distributions

The analysis of a large number of optimizations in combination with several repetitions in the wet laboratory can be found in MeasurementSimulationVariance.

## Versions

The analysis located in DecentFitParameterVariance, LocationSpread,
MeasurementVariance, Sensitivity, and Regions are conducted with tsf V.0.2.
The code can be found here: https://github.com/ComPlat/Thermosimfit/releases/tag/v.1.2

The code of the GlobalAnalysis and LocationSpread was conducted using tsf. V0.3 and the code can be found here: https://github.com/ComPlat/Thermosimfit/releases/tag/v.1.3
