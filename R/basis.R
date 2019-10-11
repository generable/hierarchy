

radial_b_spline_basis_f = function(x_k, x) {
  o = vector(mode = 'numeric', length = length(x))
  d = abs(x_k - x)
  zero = (d == 0)
  one = (d > 0 & d <= 1)
  two = (d > 1 & d <= 2)
  three = (d > 2)
  o[zero] = 2/3
  o[one] = 2/3 - d[one]^2 + d[one]^3/2
  o[two] = 4/3 - 2 * d[two] + d[two]^2 - d[two]^3/6
  o[three] = 0
  return(o)
}

scale_to_unit_spaced_knots = function(x, k, min_x = min(x), max_x = max(x)) {
  range = k - 3
  scaled_x = (x - min_x) / (max_x - min_x) * range
  return(scaled_x)
}

radial_b_spline = function(x, k, lb = min, ub = max) {
  if (!is.function(lb))
    min_x = lb
  else
    min_x = lb(x)
  if (!is.function(ub))
    max_x = ub
  else
    max_x = ub(x)
  knot_locations = seq(from = -1, to = (k - 2), length.out=k)
  scaled_x = scale_to_unit_spaced_knots(x, k, min_x, max_x)
  basis_points = sapply(knot_locations, radial_b_spline_basis_f, x = scaled_x)
  attr(basis_points, 'knot_locations') = knot_locations
  return(basis_points)
}


