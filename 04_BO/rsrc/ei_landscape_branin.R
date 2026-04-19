# Used in: 04_BO/06_acqf_opt.tex
#
# 3D EI acquisition-function landscape on the Branin function

library(bbotk)
library(data.table)
library(mlr3mbo)
library(mlr3learners)
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

# Render a colored perspective plot of the EI surface.
save_ei_persp = function(name, grid, width = 4, height = 3) {
  pal = colorRampPalette(c("#f7fbff", "#9ecae1", "#3182bd", "#08306b"))(100)
  x1 = sort(unique(grid$x1)); x2 = sort(unique(grid$x2))
  z  = matrix(grid$ei, nrow = length(x1), ncol = length(x2))
  # average z over each 2x2 corner block -> one color per facet (nx-1 * ny-1)
  zfacet = (z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] + z[-nrow(z), -ncol(z)]) / 4
  facet_col = pal[cut(zfacet, length(pal), include.lowest = TRUE)]

  path = file.path("images", paste0(name, ".pdf"))
  cairo_pdf(path, width = width, height = height)
  par(mar = c(0.4, 1.4, 0.3, 0.8))
  persp(x1, x2, z,
        theta = 20, phi = 20, expand = 0.55,
        # sadly, persp does not except expression so we use unicode here
        xlab = "λ₁", ylab = "λ₂", zlab = "EI",
        ticktype = "detailed", nticks = 4,
        col = facet_col, border = "grey40", lwd = 0.2,
        cex.axis = 0.6, cex.lab = 0.8)
  dev.off()
}

# EI is clipped at its 99% quantile so a handful of extreme cells
# (huge GP variance at the domain corners with few points) don't squash the
# visible range of the rest of the surface.
clip_ei = function(grid, q = 0.99) {
  cap = quantile(grid$ei, q)
  grid[, ei := pmin(ei, cap)]
  grid
}

few  = ei_landscape(n_init = 10)
save_ei_persp("ei_landscape_branin_few",  clip_ei(few))

many = ei_landscape(n_init = 40)
save_ei_persp("ei_landscape_branin_many", clip_ei(many))
