# 0 = h + hd + hga -h0
# 0 = d + hd -d0
# 0 = ga + hga -ga0
# 0 = hga / (h*ga) -kga
# 0 = hd / (h*d) -kd]

import numpy as np
from scipy.optimize import brentq
import pandas as pd
import matplotlib.pyplot as plt
Kd = 1e9
Kg = 1e9
Id = 1e6
Ihd = 1e7
h0 = 1e-9
d0 = 2e-9
g0_values = np.array([0.0, 2e-9, 4e-9, 6e-9, 8e-9, 1e-8, 2e-8, 4e-8])
valid_g0 = []
Signal_values = []
def equation_h(h, Kd, Kg, h0, d0, g0):
    if h <= 0:
        return np.inf
    denom_Kd = 1 + Kd * h
    if denom_Kd == 0:
        return np.inf
    h_d = (Kd * h * d0) / denom_Kd
    if g0 == 0:
        h_g = 0.0
    else:
        denom_Kg = 1 + Kg * h
        if denom_Kg == 0:
            return np.inf
        h_g = (Kg * h * g0) / denom_Kg
    residual = h + h_d + h_g - h0
    return residual
for g0 in g0_values:
    try:
        h_lower = 1e-20
        h_upper = h0
        h_sol = brentq(
            equation_h,
            h_lower,
            h_upper,
            args=(Kd, Kg, h0, d0, g0),
            xtol=1e-14,
            maxiter=1000
        )
        if h_sol <= 0:
            print("Invalid h for g0 = {g0:.2e}. Skipping.")
            continue
        denom_Kd = 1 + Kd * h_sol
        d_sol = d0 / denom_Kd
        if d_sol <= 0:
            print("Invalid d for g0 = {g0:.2e}. Skipping.")
            continue
        h_d_sol = Kd * h_sol * d_sol
        Signal = Id * d_sol + Ihd * h_d_sol
        valid_g0.append(g0)
        Signal_values.append(Signal)
    except Exception as e:
        print("Error solving for g0 = {g0:.2e}: {e}")
        continue
valid_g0 = np.array(valid_g0)
Signal_values = np.array(Signal_values)
results_table = pd.DataFrame({
    'g0 (M)': valid_g0,
    'Signal': Signal_values
})
print("Computed Signal values:")
print(results_table)
plt.figure(figsize=(8, 6))
plt.plot(valid_g0, Signal_values, marker='o', linestyle='-')
plt.xlabel('g0 (M)')
plt.ylabel('Signal (arbitrary units)')
plt.title('Signal vs. g0')
plt.grid(True)
plt.show()

