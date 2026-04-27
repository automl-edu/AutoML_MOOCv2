# Used in: 04_BO/05_acqf.tex
#
# Lower Confidence Bound at tau = 3 on the 1D toy. Upper plot shows the GP
# fit with a +- tau * sd band whose lower edge is the LCB curve in the lower
# plot.

source("rsrc/_setup.R")

set.seed(123)

instance = OptimInstanceSingleCrit$new(objective = toy1d_obj_rfundt, terminator = trm("none"))
instance$eval_batch(data.table(x = c(0.100, 0.300, 0.370, 0.650, 1.000)))

grid = generate_design_grid(instance$search_space, resolution = 1001L)$data
set(grid, j = "y", value = toy1d_obj_rfundt$eval_dt(grid)$y)

tau = 3
surrogate = my_gp_surrogate(instance$archive)
acq_function = acqf("cb", surrogate = surrogate, lambda = tau)

prediction = update_surrogate_and_grid(surrogate, grid)
# Widen band to +- tau * sd so the lower edge matches LCB at tau
set(grid, j = "y_min", value = prediction$mean - tau * prediction$se)
set(grid, j = "y_max", value = prediction$mean + tau * prediction$se)

acq_function$update()
set(grid, j = "cb", value = acq_function$eval_dt(grid[, "x"])$acq_cb)
cb_argmin = grid[which.min(cb), ]

obj = ggplot(grid, aes(x = x, y = y)) +
  geom_line() +
  geom_line(aes(y = y_hat), colour = "steelblue", linetype = 2) +
  geom_ribbon(aes(ymin = y_min, ymax = y_max), fill = "steelblue", alpha = 0.25) +
  geom_line(aes(y = y_min), colour = "darkred") +
  geom_point(aes(y = y), size = 3L, colour = "black", data = instance$archive$data) +
  xlim(c(0, 1)) + ylim(c(-2, 3.8)) +
  labs(x = expression(lambda), y = expression(c)) + theme_minimal(base_size = 13)

acq = ggplot(grid, aes(x = x, y = cb)) +
  geom_line(colour = "darkred") +
  geom_point(size = 3L, colour = "darkred", data = cb_argmin) +
  xlim(c(0, 1)) + ylim(c(-2, 3.8)) +
  labs(x = expression(lambda), y = bquote("LCB, " * tau == .(tau))) + theme_minimal(base_size = 13)

myggsave("05_acqf_lcb_obj", plot = obj, width = 5, height = 2)
myggsave("05_acqf_lcb_acq", plot = acq, width = 5, height = 2)
