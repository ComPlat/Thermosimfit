import os
import time
import numpy as np
from scipy.optimize import brentq, minimize
import matplotlib.pyplot as plt
from datetime import datetime

start_time = time.time()

# --- Paths ---
file_path = '/Users/Frank/Downloads/HG2-fitting/signal.txt'
results_dir = os.path.dirname(file_path)
os.makedirs(results_dir, exist_ok=True)

# --- Constants ---
h0_in_M = 3*1e-4   # M (simulation H0)

# Initial guess parameters (original units)
guess_Kg1, guess_Kg2 = 2e5, 0.8e7
# assume no signal from free H by default (IH=0) and introduce IG for free G
guess_IH, guess_IG, guess_IHG, guess_IHG2 = 0.0, 1.0e5, 1.1e6, 2e6

# Fitting settings
num_trials = 200
rmse_factor = 2
r2_min = 0.8

# --- Load and normalize ---
data = np.loadtxt(file_path, skiprows=1)
G0 = data[:,0]
Signal = data[:,1]
max_conc = G0.max()
max_sig = Signal.max()

g0_dim = G0 / max_conc
sig_dim = Signal / max_sig
h0_dim = h0_in_M / max_conc

# Baseline (I0) guess in original units and its dimensionless center
# Use ~2% of the dynamic range as a neutral starting point
guess_I0 = 0.02 * max_sig if max_sig > 0 else 1.0
I0_c = (guess_I0 / max_sig) if max_sig > 0 else 0.02

# Dimensionless guess centers
Kg1_c = guess_Kg1 * max_conc
Kg2_c = guess_Kg2 * max_conc
IH_c  = guess_IH  * (max_conc/max_sig)
IG_c  = guess_IG  * (max_conc/max_sig)
IHG_c = guess_IHG * (max_conc/max_sig)
IHG2_c= guess_IHG2* (max_conc/max_sig)

# Bounds ±10× around true centers
bounds = [
    (1e-8, 2.0),               # I0 free (dimensionless baseline), wide positive range
    (0.1*Kg1_c, 10*Kg1_c),
    (0.1*Kg2_c, 10*Kg2_c),
    (0.0, 0.0),                # IH fixed to 0 (no signal from free H)
    (0.01*IG_c,   1000000000*IG_c),
    (0.01*IHG_c,  1000000000*IHG_c),
    (0.01*IHG2_c, 1000000000*IHG2_c),
]

print("Using dimensionless bounds:")
for name, b in zip(["I0","Kg1","Kg2","IH","IG","IHG","IHG2"], bounds):
    print(f"  {name}: {b[0]:.2e} … {b[1]:.2e}")

# --- Model functions ---
def compute_signal(p, g0, h0):
    I0, K1, K2, IH, IG, IHG, IHG2 = p
    S = np.zeros_like(g0)
    eps = 1e-12
    for i,G in enumerate(g0):
        if G<=eps:
            S[i] = I0 + IH*h0  # g_free ~ 0 when G is ~0
            continue
        def mb(x):
            d = 1 + K1*x + (K1*K2)*x*x
            Hf = h0/d
            HG = K1*Hf*x
            HG2= K2*HG*x
            return x + HG + 2*HG2 - G
        try:
            g_free = brentq(mb, eps, max(G, eps*10), xtol=1e-14)
        except ValueError:
            S[i] = I0
            continue
        d = 1 + K1*g_free + (K1*K2)*g_free*g_free
        Hf = h0/d
        HG = K1*Hf*g_free
        HG2= K2*HG*g_free
        S[i] = I0 + IH*Hf + IG*g_free + IHG*HG + IHG2*HG2
    return S

def residuals(p, g0, y, h0):
    return np.nan_to_num(y - compute_signal(p, g0, h0), nan=1e6)

def fit_metrics(y, y_pred):
    rmse = np.sqrt(np.nanmean((y-y_pred)**2))
    ssr  = np.nansum((y-y_pred)**2)
    sst  = np.nansum((y-np.nanmean(y))**2)
    r2   = 1 - ssr/sst if sst>0 else np.nan
    return rmse, r2

# --- Fit loop (single replicate) ---
print("Fitting replicate 1…")
sim_p0 = np.array([I0_c, Kg1_c, Kg2_c, IH_c, IG_c, IHG_c, IHG2_c])
initials = [sim_p0]
# Sample initial guesses across the full dimensionless bounds (log-uniform)
for _ in range(num_trials):
    p0 = np.zeros(7)
    # Sample all parameters (including I0); respect fixed bounds (e.g., IH)
    for idx in range(0, len(bounds)):
        low, high = bounds[idx]
        if low == high:
            p0[idx] = low
        else:
            p0[idx] = 10 ** np.random.uniform(np.log10(low), np.log10(high))
    initials.append(p0)

# Diagnostic: show the range of initial Kg1 guesses
initial_Kg1_vals = [p0[1]/max_conc for p0 in initials]
print(f"Initial Kg1 sampling range (M^-1): "
      f"{min(initial_Kg1_vals):.2e} – {max(initial_Kg1_vals):.2e}")
# Diagnostic: initial sampling ranges for other parameters
initial_Kg2_vals = [p0[2]/max_conc for p0 in initials]
print(f"Initial Kg2 sampling range (M^-1): "
      f"{min(initial_Kg2_vals):.2e} – {max(initial_Kg2_vals):.2e}")
initial_I0_vals = [p0[0]*max_sig for p0 in initials]
print(f"Initial I0 sampling range (signal units): "
      f"{min(initial_I0_vals):.2e} – {max(initial_I0_vals):.2e}")
initial_IH_vals = [p0[3]*max_sig for p0 in initials]
print(f"Initial IH sampling range (signal units): "
      f"{min(initial_IH_vals):.2e} – {max(initial_IH_vals):.2e}")
initial_IG_vals = [p0[4]*(max_sig/max_conc) for p0 in initials]
print(f"Initial IG sampling range (signal units): "
      f"{min(initial_IG_vals):.2e} – {max(initial_IG_vals):.2e}")
initial_IHG_vals = [p0[5]*(max_sig/max_conc) for p0 in initials]
print(f"Initial IHG sampling range (M^-1·signal): "
      f"{min(initial_IHG_vals):.2e} – {max(initial_IHG_vals):.2e}")
initial_IHG2_vals = [p0[6]*(max_sig/max_conc) for p0 in initials]
print(f"Initial IHG2 sampling range (M^-1·signal): "
      f"{min(initial_IHG2_vals):.2e} – {max(initial_IHG2_vals):.2e}")

results = []
for p0 in initials:
    res = minimize(lambda p: np.sum(residuals(p,g0_dim,sig_dim,h0_dim)**2),
                   p0, method='L-BFGS-B', bounds=bounds)
    pred = compute_signal(res.x, g0_dim, h0_dim)
    rmse, r2 = fit_metrics(sig_dim, pred)
    results.append((res.x, rmse, r2))

# select and filter
results.sort(key=lambda t: t[1])
best_rmse = results[0][1]
filtered = [r for r in results if r[1]<=best_rmse*rmse_factor and r[2]>=r2_min]
if not filtered:
    print("No fits passed thresholds.")
    exit()

# median params
med = np.median([r[0] for r in filtered], axis=0)
rmse_med, r2_med = fit_metrics(sig_dim, compute_signal(med, g0_dim, h0_dim))

# back‐scale
I0o   = med[0]*max_sig
Kg1o  = med[1]/max_conc
Kg2o  = med[2]/max_conc
IHo   = med[3]*max_sig
IGo   = med[4]*(max_sig/max_conc)
IHGo  = med[5]*(max_sig/max_conc)
IHG2o = med[6]*(max_sig/max_conc)

print("Recovered parameters (orig units):")
print(f"  I0   = {I0o:.2e}")
print(f"  Kg1  = {Kg1o:.2e}")
print(f"  Kg2  = {Kg2o:.2e}")
print(f"  IH   = {IHo:.2e}")
print(f"  IG   = {IGo:.2e}")
print(f"  IHG  = {IHGo:.2e}")
print(f"  IHG2 = {IHG2o:.2e}")
print(f"Fit RMSE={rmse_med:.3e}, R²={r2_med:.3f}")

# --- Plot ---
xs = np.linspace(0, G0.max(), 200)
ys = compute_signal(med, xs/max_conc, h0_dim)*max_sig
plt.figure(figsize=(6,4))
plt.plot(G0, Signal, 'o', label='data')
plt.plot(xs, ys, '-', label='fit')
plt.xlabel('G₀ (M)')
plt.ylabel('Signal')
plt.legend()
 # Add fit parameters box
param_text = (
    f"I0 = {I0o:.2e}\n"
    f"Kg1 = {Kg1o:.2e}\n"
    f"Kg2 = {Kg2o:.2e}\n"
    f"IH = {IHo:.2e}\n"
    f"IG = {IGo:.2e}\n"
    f"IHG = {IHGo:.2e}\n"
    f"IHG2 = {IHG2o:.2e}\n"
    f"RMSE = {(rmse_med*max_sig):.3f}\n"
    f"R² = {r2_med:.3f}"
)
ax = plt.gca()
ax.text(
    0.97, 0.95, param_text,
    transform=ax.transAxes,
    verticalalignment='top',
    horizontalalignment='right',
    fontsize=8,
    bbox=dict(boxstyle='round', facecolor='white', alpha=0.8)
)  # closing ax.text parenthesis
plt.tight_layout()
plot_file = os.path.join(results_dir, f"fit_plot_replica_{1}.png")
plt.savefig(plot_file, bbox_inches='tight')
print(f"Plot saved to {plot_file}")
plt.close()

# --- Export results ---
print(f"Preparing to write results into {results_dir}")
replica_file = os.path.join(results_dir, f"fit_results_replica_{1}_norm.txt")
print(f"Writing results to: {replica_file}")
with open(replica_file, 'w') as f:
    # Input section
    f.write("Input:\n")
    f.write(f"h0 (M): {h0_in_M:.6e}\n")
    f.write(f"guess_Kg1 (M^-1): {guess_Kg1:.6e}\n")
    f.write(f"guess_Kg2 (M^-1): {guess_Kg2:.6e}\n")
    f.write(f"guess_IH (signal): {guess_IH:.6e}\n")
    f.write(f"guess_IG (signal): {guess_IG:.6e}\n")
    f.write(f"guess_IHG (signal): {guess_IHG:.6e}\n")
    f.write(f"guess_IHG2 (signal): {guess_IHG2:.6e}\n\n")

    # Output section
    f.write("Output:\nMedian parameters:\n")
    f.write(f"I0 (signal): {I0o:.2e}\n")
    f.write(f"Kg1 (M^-1): {Kg1o:.2e}\n")
    f.write(f"Kg2 (M^-1): {Kg2o:.2e}\n")
    f.write(f"IH (signal): {IHo:.2e}\n")
    f.write(f"IG (signal): {IGo:.2e}\n")
    f.write(f"IHG (signal): {IHGo:.2e}\n")
    f.write(f"IHG2 (signal): {IHG2o:.2e}\n")
    f.write(f"RMSE: {rmse_med*max_sig:.3f}\n")
    f.write(f"R²: {r2_med:.3f}\n\n")

    # Acceptable fits
    f.write("Acceptable Fit Parameters:\n")
    f.write("I0\tKg1\tKg2\tIH\tIG\tIHG\tIHG2\tRMSE\tR²\n")
    for p, rm, r2 in filtered:
        f.write(f"{(p[0]*max_sig):.2e}\t{(p[1]/max_conc):.2e}\t{(p[2]/max_conc):.2e}\t"
                f"{(p[3]*max_sig):.2e}\t{(p[4]*(max_sig/max_conc)):.2e}\t"
                f"{(p[5]*(max_sig/max_conc)):.2e}\t{(p[6]*(max_sig/max_conc)):.2e}\t"
                f"{(rm*max_sig):.3f}\t{r2:.3f}\n")

    # Standard deviations
    I0_vals  = [p[0]*max_sig for p,_,_ in filtered]
    Kg1_vals = [p[1]/max_conc for p,_,_ in filtered]
    Kg2_vals = [p[2]/max_conc for p,_,_ in filtered]
    IH_vals   = [p[3]*max_sig for p,_,_ in filtered]
    IG_vals   = [p[4]*(max_sig/max_conc) for p,_,_ in filtered]
    IHG_vals  = [p[5]*(max_sig/max_conc) for p,_,_ in filtered]
    IHG2_vals = [p[6]*(max_sig/max_conc) for p,_,_ in filtered]
    f.write("\nStandard Deviations:\n")
    f.write(f"I0 Std Dev: {np.std(I0_vals):.2e}\n")
    f.write(f"Kg1 Std Dev: {np.std(Kg1_vals):.2e}\n")
    f.write(f"Kg2 Std Dev: {np.std(Kg2_vals):.2e}\n")
    f.write(f"IH Std Dev: {np.std(IH_vals):.2e}\n")
    f.write(f"IG Std Dev: {np.std(IG_vals):.2e}\n")
    f.write(f"IHG Std Dev: {np.std(IHG_vals):.2e}\n")
    f.write(f"IHG2 Std Dev: {np.std(IHG2_vals):.2e}\n\n")

    # Original data
    f.write("Original Data:\nG0 (M)\tSignal\n")
    for x,y in zip(G0, Signal):
        f.write(f"{x:.6e}\t{y:.6e}\n")
    # Fitting curve
    f.write("\nFitting Curve:\nG0 (M)\tSignal\n")
    for xx,yy in zip(xs, ys):
        f.write(f"{xx:.6e}\t{yy:.6e}\n")
    f.write(f"\nDate of Export: {datetime.now():%Y-%m-%d %H:%M:%S}\n")

# Also export all acceptable fits to CSV for easy verification
csv_file = os.path.join(results_dir, f"acceptable_fits_replica_{1}.csv")
with open(csv_file, 'w') as cf:
    cf.write("I0,Kg1,Kg2,IH,IG,IHG,IHG2,RMSE,R2\n")
    for p, rm, r2 in filtered:
        cf.write(
            f"{(p[0]*max_sig):.6e},"
            f"{(p[1]/max_conc):.6e},"
            f"{(p[2]/max_conc):.6e},"
            f"{(p[3]*max_sig):.6e},"
            f"{(p[4]*(max_sig/max_conc)):.6e},"
            f"{(p[5]*(max_sig/max_conc)):.6e},"
            f"{(p[6]*(max_sig/max_conc)):.6e},"
            f"{(rm*max_sig):.6f},"
            f"{r2:.6f}\n"
        )
print(f"Exported all acceptable fits to {csv_file}")
print(f"Directory contents of results_dir: {os.listdir(results_dir)}")
print(f"Results for Replica {1} saved to {replica_file}")

print(f"Done in {time.time()-start_time:.2f}s")
