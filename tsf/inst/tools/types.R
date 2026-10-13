# Types for the ast2ast code in inst/tools/, compiled by ast2ast::make_package
# (development/make_package_tsf.R).

# shared by all VAPRO cases
new_type(
  SolveD_HD_Result,
  slots(
    d |> type(vec(double)),
    hd |> type(vec(double))
  )
)
new_type(
  LossEval,
  slots(
    total_err |> type(double),
    betas |> type(vec(double)),
    fitted_signal |> type(vec(double))
  )
)
new_type(
  GridResult,
  slots(
    param_hat |> type(double),
    loss_hat |> type(double),
    betas |> type(vec(double)),
    fitted_signal |> type(vec(double))
  )
)

# IDA
new_type(
  EquationParamsIda,
  slots(
    Kd |> type(double),
    Kg |> type(double),
    h0 |> type(double),
    d0 |> type(double),
    g |> type(double)
  )
)
new_type(
  AddParamsIda,
  slots(
    kd |> type(double),
    h0 |> type(double),
    d0 |> type(double),
    ga |> type(vec(double)),
    n_sigs |> type(int),
    signal |> type(vec(double))
  )
)
