# Used in: 04_BO/01_big_picture.tex
#
# BO vs random search on Branin: point placement in 2D + best-so-far convergence.

library(patchwork)
source("rsrc/_setup.R")

set.seed(42)

# ------------------------------------------------------------------------------
# Branin landscape grid (log-scaled for a usable color range -- 3 minima visible)

res = 160L
x1 = seq(-5, 10, length.out = res)
x2 = seq( 0, 15, length.out = res)
grid = as.data.table(expand.grid(x1 = x1, x2 = x2))
grid[, y := branin(x1, x2)]
grid[, logy := log10(y + 0.5)]

branin_minima = data.table(
  x1 = c(-pi, pi, 9.42478),
  x2 = c(12.275, 2.275, 2.475)
)

# ------------------------------------------------------------------------------
# Run once: RS and BO, same total budget.

budget = 30L
n_init = 6L

run_rs = function() {
  instance = OptimInstanceSingleCrit$new(objective = branin_obj_rfundt,
                                         terminator = trm("evals", n_evals = budget))
  opt = opt("random_search", batch_size = 1L)
  opt$optimize(instance)
  instance$archive$data[, .(x1, x2, y, iter = seq_len(.N))]
}

run_bo = function() {
  instance = OptimInstanceSingleCrit$new(objective = branin_obj_rfundt,
                                         terminator = trm("evals", n_evals = budget))
  design = generate_design_lhs(instance$search_space, n = n_init)$data
  instance$eval_batch(design)
  surrogate = my_gp_surrogate(instance$archive)
  acq_f = acqf("ei", surrogate = surrogate)
  acq_opt = acqo(opt("random_search", batch_size = 1000L),
                 terminator = trm("evals", n_evals = 1000L))
  opt = opt("mbo", loop_function = bayesopt_ego,
            surrogate = surrogate, acq_function = acq_f, acq_optimizer = acq_opt)
  opt$optimize(instance)
  instance$archive$data[, .(x1, x2, y, iter = seq_len(.N))]
}

set.seed(42); rs_pts = run_rs()
set.seed(42); bo_pts = run_bo()

rs_pts[, method := "Random Search"]
bo_pts[, method := "Bayesian Optimization"]

# Best so far
best_so_far = function(dt) dt[, .(iter, best = cummin(y))]
rs_conv = best_so_far(rs_pts); rs_conv[, method := "Random Search"]
bo_conv = best_so_far(bo_pts); bo_conv[, method := "Bayesian Optimization"]
conv = rbind(rs_conv, bo_conv)

# ------------------------------------------------------------------------------
# Plots.

landscape_plot = function(pts, title) {
  ggplot(grid, aes(x = x1, y = x2)) +
    geom_raster(aes(fill = logy)) +
    geom_contour(aes(z = logy), colour = "white", alpha = 0.35,
                 linewidth = 0.25, bins = 8) +
    geom_point(aes(x = x1, y = x2), data = branin_minima,
               shape = 8, colour = "#ff1493", size = 3.2, stroke = 1.3) +
    geom_point(aes(x = x1, y = x2, size = iter), data = pts,
               shape = 21, colour = "black", fill = "#e41a1c", alpha = 0.85) +
    scale_size_continuous(range = c(1.2, 3.2), guide = "none") +
    coord_cartesian(xlim = c(-5, 10), ylim = c(0, 15), expand = FALSE) +
    labs(subtitle = title, x = expression(lambda[1]), y = expression(lambda[2])) +
    landscape_style()
}

conv_plot = ggplot(conv, aes(x = iter, y = best, colour = method)) +
  geom_step(linewidth = 0.9) +
  geom_point(size = 1.6) +
  scale_colour_manual(values = c("Random Search" = "#7f7f7f",
                                 "Bayesian Optimization" = "#e41a1c")) +
  scale_y_log10() +
  labs(subtitle = "Best-so-far cost (log scale)",
       x = "Evaluation", y = NULL, colour = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = c(0.7, 0.85),
        legend.background = element_rect(fill = alpha("white", 0.7), colour = NA),
        plot.subtitle = element_text(face = "bold"),
        panel.grid.minor = element_blank())

p = landscape_plot(rs_pts, "Random search (30 evals)") +
    landscape_plot(bo_pts, "BO with EI (6 LHS + 24 iters)") +
    conv_plot +
    plot_layout(widths = c(1, 1, 1))

myggsave("01_bo_vs_rs_branin", plot = p, width = 11, height = 3.2)
