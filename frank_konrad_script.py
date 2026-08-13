import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.colors import Normalize
from scipy.optimize import brentq

# Change these parameters
csv_path = './ida_params.csv'

Kd = 1.7e7
h0 = 4.3e-6
d0 = 6e-6

g0 = [
    0, 0.0000004975, 0.0000009901, 0.0000014778, 0.0000019608, 0.000002439, 0.0000029126, 0.0000033816, 0.0000038462, 0.0000043062, 0.0000047619, 0.0000052133,
    0.0000056604, 0.0000061033, 0.0000065421, 0.0000069767, 0.0000074074, 0.0000078341, 0.0000082569, 0.0000086758, 0.0000090909
]
signal1 = [
    5723.38, 5093.82, 4442.1, 3824.87, 3270.18, 2752.29, 2295.3, 1936.14, 1636.69, 1388.49, 1203.12,
    1055.42, 938.988, 831.478, 758.139, 687.357, 630.271, 583.479, 542.406, 507.428, 474.301
]
signal2 = [
    5826.29, 5207.25, 4530, 3876.8, 3314.88, 2787.08, 2322.61, 1951.38, 1644.28, 1399, 1211.24,
    1052.89, 931.75, 829.022, 754.79, 683.962, 629.471, 580.133, 539.111, 504.288, 473.968
]
signal3 = [
    5846.1, 5251.42, 4579.22, 3922.71, 3351.84, 2791.56, 2333.07, 1969.98, 1653.07, 1398.69, 1210.67,
    1054.34, 936.536, 827.836, 757.271, 687.888, 628.26, 582.921, 541.771, 505.525, 478.098
]

def compute_signal_ida(params, g0_values, Kd, h0, d0):
    I0, Kg, Id, Ihd = params
    Signal_values = []
    for g0 in g0_values:
        try:

            def equation_h(h):
                denom_Kd = 1 + Kd * h
                denom_Kg = 1 + Kg * h
                h_d = (Kd * h * d0) / denom_Kd
                h_g = (Kg * h * g0) / denom_Kg
                return h + h_d + h_g - h0

            h_sol = brentq(equation_h, 1e-20, h0, xtol=1e-14, maxiter=1000)
            denom_Kd = 1 + Kd * h_sol
            d_free = d0 / denom_Kd
            h_d = Kd * h_sol * d_free

            Signal = I0 + Id * d_free + Ihd * h_d
            Signal_values.append(Signal)
        except Exception:
            Signal_values.append(np.nan)
    return np.array(Signal_values)

df = pd.read_csv(csv_path).rename(columns={'Ka(HG) [1/M]': 'Kg', 'I(0)': 'I0', 'I(D) [1/M]': 'Id', 'I(HD) [1/M]': 'Ihd'})

params = df[['I0', 'Kg', 'Id', 'Ihd']].astype(float).to_numpy()

df['signals'] = [compute_signal_ida(p, g0, Kd, h0, d0) for p in params]

g0_vals = np.array(g0)
errors = df['error'].astype(float).values
norm = Normalize(vmin=np.nanmin(errors), vmax=np.nanmax(errors))
cmap = plt.get_cmap('viridis')
colors = cmap(norm(errors))

fig, ax = plt.subplots(figsize=(9, 6))

for color, sig in zip(colors, df['signals'].values):
    ax.plot(g0_vals, sig, color=color, linewidth=1.5, marker='o', markersize=4, alpha=0.9)

sm = plt.cm.ScalarMappable(norm=norm, cmap=cmap)
sm.set_array(errors)
cbar = fig.colorbar(sm, ax=ax, orientation='vertical')
cbar.ax.set_xlabel('Error', fontsize=12, fontweight='light', labelpad=8)

cbar.ax.tick_params(axis='x', direction='in')
ax.set_xlabel(r'$G_0$ [µM]', fontsize=12, fontweight='light')
ax.set_ylabel('Intensity [a.u.]', fontsize=12, fontweight='light')
ax.ticklabel_format(style='sci', scilimits=(-2, 2), axis='both', useMathText=True)
ax.minorticks_on()
ax.grid(which='major', linestyle='-', linewidth=0.5, color='gray', alpha=0.9)
ax.grid(which='minor', linestyle=':', linewidth=0.5, color='lightgray', alpha=0.9)
ax.tick_params(direction='in', which='both')
fig.tight_layout()

# Save the figure
fig.savefig('forward_sim_plot.png', dpi=300)

plt.show()
