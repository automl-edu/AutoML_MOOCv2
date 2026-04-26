# Used in: 04_BO/03_gps.tex
#
# 3D visualization of a GP fit to the Branin function:
#   1) true objective surface
#   2) GP posterior mean surface (with training points overlaid)
#   3) GP posterior std surface (uncertainty)

library(bbotk)
library(data.table)
library(mlr3mbo)
library(mlr3learners)
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
# Perspective plot helper. Colors facets by the 2x2-block mean of z. Optionally
# overlays 3D points via trans3d() on top of the surface.

save_persp = function(name, z_vec, pal_cols, points = NULL, zlab = "") {
  pal = colorRampPalette(pal_cols)(120)
  z = matrix(z_vec, nrow = length(x1), ncol = length(x2))
  zfacet = (z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] + z[-nrow(z), -ncol(z)]) / 4
  facet_col = pal[cut(zfacet, length(pal), include.lowest = TRUE)]

  path = file.path("images", paste0(name, ".pdf"))
  cairo_pdf(path, width = 4, height = 3)
  par(mar = c(0.4, 1.4, 0.3, 0.8))
  tmat = persp(x1, x2, z,
               theta = 25, phi = 22, expand = 0.55,
               # sadly, persp does not accept expression so we use unicode here
               xlab = "λ₁", ylab = "λ₂", zlab = zlab,
               ticktype = "detailed", nticks = 4,
               col = facet_col, border = "grey40", lwd = 0.2,
               cex.axis = 0.6, cex.lab = 0.8)
  if (!is.null(points)) {
    p3 = trans3d(points$x1, points$x2, points$z, tmat)
    points(p3$x, p3$y, pch = 21, bg = "#ffcc00", col = "black", cex = 1.1, lwd = 0.8)
  }
  dev.off()
}

# ------------------------------------------------------------------------------
# Render.

# truth
save_persp("03_gp_branin_true", grid$y,
           pal_cols = c("#f7fcf5", "#a1d99b", "#238b45", "#00441b"),
           zlab = "c")

# posterior mean (with training points)
save_persp("03_gp_branin_mean", grid$mean,
           pal_cols = c("#f7fbff", "#9ecae1", "#3182bd", "#08306b"),
           points = data.table(x1 = design_dt$x1, x2 = design_dt$x2, z = design_dt$y),
           zlab = "μ")

# posterior sd (with training points)
save_persp("03_gp_branin_sd", grid$sd,
           pal_cols = c("#fff5eb", "#fdae6b", "#d94801", "#7f2704"),
           points = data.table(x1 = design_dt$x1, x2 = design_dt$x2, z = design_dt$sd),
           zlab = "σ")
