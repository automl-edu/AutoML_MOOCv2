# Used in: 04_BO/08_parallel_bo.tex
#
# Kriging Believer / fantasizing: after each batch pick x_k, add a fantasized
# observation (x_k, mu(x_k)) to the archive, re-update the surrogate + EI, and
# pick the next argmax. The posterior variance collapses around x_k -> EI drops
# there naturally, and the next argmax moves to a different peak.
#
# Note: mlr3mbo ships this as the loop function `bayesopt_mpcl` (Multipoint
# Constant Liar, args q, liar = mean for Kriging Believer). Here we roll the
# loop by hand so we can snapshot the EI grid between picks for the figure.

library(patchwork)
source("rsrc/_setup.R")

set.seed(42)

instance = OptimInstanceSingleCrit$new(objective = branin_obj_rfundt, terminator = trm("none"))
design = generate_design_lhs(instance$search_space, n = 8)$data
instance$eval_batch(design)
design_plot = copy(instance$archive$data[, .(x1, x2)])  # keep real design for plotting

surrogate = my_gp_surrogate(instance$archive)
acq = acqf("ei", surrogate = surrogate)

res = 150L
gx1 = seq(-5, 10, length.out = res)
gx2 = seq( 0, 15, length.out = res)
grid = as.data.table(expand.grid(x1 = gx1, x2 = gx2))

q = 4
panels = vector("list", q)
picks = data.table(x1 = numeric(), x2 = numeric(), iter = integer())

for (k in seq_len(q)) {
  acq$surrogate$update()
  acq$update()
  ei_grid = copy(grid)
  ei_grid[, ei := acq$eval_dt(grid)$acq_ei]
  panels[[k]] = ei_grid

  idx = which.max(ei_grid$ei)
  new_x = ei_grid[idx, .(x1, x2)]
  fantasy_y = acq$surrogate$predict(new_x)$mean  # Kriging Believer: posterior mean

  picks = rbind(picks, cbind(new_x, iter = k))
  instance$archive$add_evals(
    xdt         = new_x,
    xss_trafoed = list(as.list(new_x)),
    ydt         = data.table(y = fantasy_y)
  )
}

# ------------------------------------------------------------------------------
plot_iter = function(panel_dt, earlier, current, k) {
  p = acqf_base_plot(panel_dt, design_plot)
  if (nrow(earlier) > 0) {
    p = p + geom_point(aes(x = x1, y = x2), data = earlier,
                       shape = 21, colour = "black", fill = "#e41a1c",
                       size = 3, alpha = 0.9)
  }
  p +
    geom_point(aes(x = x1, y = x2), data = current,
               shape = 23, colour = "black", fill = "#4daf4a",
               size = 4.2, stroke = 1.2) +
    labs(subtitle = sprintf("iter %d", k),
         x = expression(lambda[1]), y = expression(lambda[2]))
}

plts = lapply(seq_len(q), function(k) {
  plot_iter(panels[[k]], picks[iter < k], picks[iter == k], k)
})

p = wrap_plots(plts, nrow = 1)
myggsave("08_batch_bo_kriging_believer", plot = p, width = 10, height = 2.8)
