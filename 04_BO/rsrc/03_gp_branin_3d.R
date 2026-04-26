# Used in: 04_BO/03_gps.tex
#
# 3D visualization of a GP fit to the Branin function:
#   1) true objective surface
#   2) GP posterior mean surface (with training points overlaid)
#   3) GP posterior std surface (uncertainty)

source("rsrc/_setup.R")

set.seed(7)

# ------------------------------------------------------------------------------
# Fit GP on a small LHS design.

instance = OptimInstanceSingleCrit$new(objective = branin_obj_rfundt, terminator = trm("none"))
design = generate_design_lhs(instance$search_space, n = 12L)$data
instance$eval_batch(design)
design_dt = copy(instance$archive$data)

surrogate = my_gp_surrogate(instance$archive)
surrogate$update()

# ------------------------------------------------------------------------------
# Dense grid of predictions.

res = 60L
x1 = seq(-5, 10, length.out = res)
x2 = seq( 0, 15, length.out = res)
grid = as.data.table(expand.grid(x1 = x1, x2 = x2))
grid[, y := branin(x1, x2)]
pred = surrogate$predict(grid)
grid[, mean := pred$mean]
grid[, sd   := pred$se]

# Predict at training points too -- needed to place markers on the sd surface
# (sd ~ 0 for interpolating GP, but use the actual prediction for correctness).
pred_design = surrogate$predict(design_dt[, .(x1, x2)])
design_dt[, sd := pred_design$se]

# ------------------------------------------------------------------------------
# Render via the shared `save_persp` helper from _setup.R.

# truth
save_persp("03_gp_branin_true", x1, x2, grid$y,
           pal_cols = c("#f7fcf5", "#a1d99b", "#238b45", "#00441b"),
           zlab = "c")

# posterior mean (with training points)
save_persp("03_gp_branin_mean", x1, x2, grid$mean,
           pal_cols = c("#f7fbff", "#9ecae1", "#3182bd", "#08306b"),
           points = data.table(x1 = design_dt$x1, x2 = design_dt$x2, z = design_dt$y),
           zlab = "μ")

# posterior sd (with training points)
save_persp("03_gp_branin_sd", x1, x2, grid$sd,
           pal_cols = c("#fff5eb", "#fdae6b", "#d94801", "#7f2704"),
           points = data.table(x1 = design_dt$x1, x2 = design_dt$x2, z = design_dt$sd),
           zlab = "σ")
