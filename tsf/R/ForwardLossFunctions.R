forward_dba_dye_const <- function(Kd, Id, Ihd, d0, h0_values) {
  sol <- solve_h_dba(Kd, h0_values, d0)
  valid <- !is.na(sol$d)
  results_table <- data.frame(
    h0 = h0_values[valid],
    Signal = Id * sol$d[valid] + Ihd * sol$hd[valid]
  )
  return(results_table)
}

forward_dba_host_const <- function(Kd, Id, Ihd, h0, d0_values) {
  sol <- solve_h_dba(Kd, h0, d0_values)
  valid <- !is.na(sol$d)
  results_table <- data.frame(
    d0 = d0_values[valid],
    Signal = Id * sol$d[valid] + Ihd * sol$hd[valid]
  )
  return(results_table)
}

forward_ida <- function(Kg, Ihd, Id, Kd, h0, d0, g0_values) {
  sol <- solve_h_ida_gda(Kd, Kg, h0, d0, g0_values)
  valid <- !is.na(sol$d)
  results_table <- data.frame(
    guest = g0_values[valid],
    Signal = Id * sol$d[valid] + Ihd * sol$hd[valid]
  )
  return(results_table)
}

forward_gda <- function(Kd, Kg, Id, Ihd, h0, g0, d0_values) {
  sol <- solve_h_ida_gda(Kd, Kg, h0, d0_values, g0)
  valid <- !is.na(sol$d)
  results_table <- data.frame(
    d0 = d0_values[valid],
    Signal = Id * sol$d[valid] + Ihd * sol$hd[valid]
  )
  return(results_table)
}
