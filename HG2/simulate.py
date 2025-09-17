import numpy as np
from scipy.optimize import brentq
import os
import matplotlib.pyplot as plt

# --- User‐set parameters (original units) ---
Kg1 = 1e6      # first‐step binding constant
Kg2 = 2e5      # second‐step binding constant
I0 = 0       # baseline signal
IH   = 0       # signal coefficient for free H (set to 0: no signal from free H)
IG   = 1e6     # signal coefficient for free G
IHG = 1e7      # signal coefficient for HG
IHG2 = 1e5     # signal coefficient for HG2


H0 = 3 * 1e-4    # total host concentration (M)

# Generate G0 range: 0 to 1e-3 M in 20 steps
G0_values = np.linspace(0, 1e-3, 20)

signals = []

for G0 in G0_values:
    # Define mass‐balance equation in free ligand g
    def balance(g):
        # H_free = H0 / (1 + Kg1*g + Kg1*Kg2*g^2)
        denom = 1 + Kg1*g + (Kg1*Kg2)*(g**2)
        Hf = H0 / denom
        HG  = Kg1 * Hf * g
        HG2 = Kg2 * HG * g
        return g + HG + 2*HG2 - G0

    # Solve for free G in [0, max(G0, ε)]
    g_lo, g_hi = 0.0, max(G0, 1e-12)
    try:
        g_free = brentq(balance, g_lo, g_hi, xtol=1e-14)
    except ValueError:
        g_free = np.nan

    # Compute species concentrations
    denom = 1 + Kg1*g_free + (Kg1*Kg2)*(g_free**2)
    Hf  = H0 / denom
    HG  = Kg1 * Hf * g_free
    HG2 = Kg2 * HG * g_free

    # Compute signal
    S = I0 + IH*Hf + IG*g_free + IHG*HG + IHG2*HG2
    signals.append(S)

output = np.column_stack((G0_values, signals))
results_dir = ''
outfile = os.path.join(results_dir, 'signal.txt')
np.savetxt(outfile, output, header='G0(M)\tSignal', fmt='%.6e', comments='')
