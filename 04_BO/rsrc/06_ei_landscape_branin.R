# Used in: 04_BO/06_acqf_opt.tex
#
# 3D EI acquisition-function landscape on the Branin function

source("rsrc/_setup.R")

set.seed(123)


# Fit GP on an LHS initial design and evaluate EI on a regular grid.
ei_landscape = function(n_init, res = 60L) {
  instance = OptimInstanceSingleCrit$new(objective = branin_obj_rfundt, terminator = trm("none"))
  design = generate_design_lhs(instance$search_space, n = n_init)$data
  instance$eval_batch(design)

  surrogate = my_gp_surrogate(instance$archive)
  acq_function = acqf("ei", surrogate = surrogate)
  acq_function$surrogate$update()
  acq_function$update()

  x1_seq = seq(-5, 10, length.out = res)
  x2_seq = seq( 0, 15, length.out = res)
  grid = as.data.table(expand.grid(x1 = x1_seq, x2 = x2_seq))
  grid[, ei := acq_function$eval_dt(grid[, .(x1, x2)])$acq_ei]
  grid
}

# EI is clipped at its 99% quantile so a handful of extreme cells
# (huge GP variance at the domain corners with few points) don't squash the
# visible range of the rest of the surface.
clip_ei = function(grid, q = 0.99) {
  cap = quantile(grid$ei, q)
  grid[, ei := pmin(ei, cap)]
  grid
}

# Render via the shared `save_persp` helper from _setup.R.
render_ei = function(name, grid) {
  x1 = sort(unique(grid$x1)); x2 = sort(unique(grid$x2))
  save_persp(name, x1, x2, clip_ei(grid)$ei,
             pal_cols = c("#f7fbff", "#9ecae1", "#3182bd", "#08306b"),
             theta = 20, phi = 20, zlab = "EI")
}

render_ei("06_ei_landscape_branin_few",  ei_landscape(n_init = 10))
render_ei("06_ei_landscape_branin_many", ei_landscape(n_init = 40))
