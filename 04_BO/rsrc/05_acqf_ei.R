# Used in: 04_BO/03_gps.tex, 04_BO/05_acqf.tex, 04_BO/06_acqf_opt.tex
#
# Three EI illustrations on the 1D toy:
#   1) bayesian_loop_ee:               7-point archive + well/insufficiently
#                                      explored regions
#   2) bayesian_loop_sm_normal_fmin:   GP fit, current best, normal density
#                                      at the EI argmax
#   3) bayesian_loop_6_{obj,acq}:      GP fit and EI curve after 5 BO steps
#                                      starting from a 5-point initial design

source("rsrc/_setup.R")

set.seed(123)

objective = toy1d_obj_rfundt

# ===== Plot 1: well / insufficiently explored regions =======================

instance = OptimInstanceSingleCrit$new(objective = objective, terminator = trm("none"))
instance$eval_batch(data.table(x = c(0.100, 0.300, 0.650, 1.000, 0.348, 0.400, 0.349)))

grid = generate_design_grid(instance$search_space, resolution = 1001L)$data
set(grid, j = "y", value = objective$eval_dt(grid)$y)

g = ggplot() +
  geom_line(aes(x = x, y = y), data = grid) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  geom_rect(aes(xmin = 0.25, xmax = 0.45, ymin = -Inf, ymax = Inf),
            data = data.table(), alpha = 0.2, fill = "lightblue") +
  geom_rect(aes(xmin = 0.70, xmax = 0.90, ymin = -Inf, ymax = Inf),
            data = data.table(), alpha = 0.2, fill = "red") +
  geom_text(aes(x = 0.35, y = -1.8, label = "well explored"),
            data = data.table(), color = "lightblue4") +
  geom_text(aes(x = 0.80, y = -1.8, label = "insufficiently explored"),
            data = data.table(), color = "indianred4") +
  xlim(c(0, 1)) + ylim(c(-2, 2.2)) +
  labs(x = expression(lambda), y = expression(c)) + theme_minimal()

myggsave("05_bayesian_loop_ee", plot = g, width = 5, height = 4)

# ===== Plot 2: GP fit + best + normal density at EI argmax ==================

surrogate = my_gp_surrogate(instance$archive)
acq_function = acqf("ei", surrogate = surrogate)

update_surrogate_and_grid(surrogate, grid)
acq_function$update()
set(grid, j = "ei", value = acq_function$eval_dt(grid[, "x"])$acq_ei)
ei_argmax = grid[which.max(ei), ]

# Vertical normal density at ei_argmax, scaled to fit a thin horizontal slot
norm_curve = data.table(y = seq(-2, 2.2, by = 0.01))
norm_curve[, d := dnorm(y, mean = ei_argmax$y_hat, sd = ei_argmax$y_max - ei_argmax$y_hat)]
norm_curve[, x := (d / max(d)) / 10]

g = ggplot(grid, aes(x = x, y = y)) +
  geom_line() +
  geom_line(aes(y = y_hat), colour = "steelblue", linetype = 2) +
  geom_ribbon(aes(ymin = y_min, ymax = y_max), fill = "steelblue", alpha = 0.1) +
  geom_point(aes(y = y), size = 3L, colour = "black", data = instance$archive$data) +
  geom_point(aes(y = y), size = 3L, colour = "#00A64F", data = instance$archive$best()) +
  geom_hline(yintercept = instance$archive$best()$y, colour = "#00A64F", linetype = 2) +
  geom_path(aes(y = y), data = norm_curve, colour = "darkgrey") +
  geom_point(aes(y = y_hat), data = ei_argmax, size = 3L, colour = "darkgrey") +
  geom_segment(aes(x = 0, xend = 0.1, y = ei_argmax$y_hat, yend = ei_argmax$y_hat),
               colour = "darkgrey") +
  xlim(c(0, 1)) + ylim(c(-2, 2.2)) +
  labs(x = expression(lambda), y = expression(c)) + theme_minimal()

myggsave("05_bayesian_loop_sm_normal_fmin", plot = g, width = 5, height = 4)

# ===== Plot 3: BO state after 5 EI proposals from a 5-point initial design ==

instance$archive$clear()
instance$eval_batch(data.table(x = c(0.100, 0.300, 0.370, 0.650, 1.000)))

# Six BO steps; only the last one is plotted. The previous step's argmax is
# kept around as the grey "old pick" decoration on the obj plot.
old_ei_argmax = NULL
for (i in seq_len(6)) {
  update_surrogate_and_grid(surrogate, grid)
  acq_function$update()
  set(grid, j = "ei", value = acq_function$eval_dt(grid[, "x"])$acq_ei)
  ei_argmax = grid[which.max(ei), ]
  if (i < 6) {
    old_ei_argmax = ei_argmax
    instance$eval_batch(ei_argmax[, "x", with = FALSE])
  }
}

obj = ggplot(grid, aes(x = x, y = y)) +
  geom_line() +
  geom_line(aes(y = y_hat), colour = "steelblue", linetype = 2) +
  geom_ribbon(aes(ymin = y_min, ymax = y_max), fill = "steelblue", alpha = 0.1) +
  geom_point(aes(y = y), size = 3L, colour = "black", data = instance$archive$data) +
  geom_point(aes(y = y), size = 3L, colour = "grey", data = old_ei_argmax) +
  xlim(c(0, 1)) + ylim(c(-2, 2.2)) +
  labs(x = expression(lambda), y = expression(c)) + theme_minimal()

acq = ggplot(grid, aes(x = x, y = ei)) +
  geom_line(colour = "darkred") +
  geom_point(size = 3L, colour = "darkred", data = ei_argmax) +
  xlim(c(0, 1)) +
  labs(x = expression(lambda), y = "EI") + theme_minimal()

myggsave("05_bayesian_loop_6_obj", plot = obj, width = 5, height = 2)
myggsave("05_bayesian_loop_6_acq", plot = acq, width = 5, height = 2)
