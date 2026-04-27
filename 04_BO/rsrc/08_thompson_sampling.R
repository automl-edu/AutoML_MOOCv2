# Used in: 04_BO/08_parallel_bo.tex
#
# Thompson sampling as a parallel BO mechanism: draw q sample paths from the
# GP posterior on a 1D toy; each path's argmin is one batch proposal. The plot
# illustrates that the batch is diverse "by construction" because the q paths
# disagree where the GP is uncertain.

source("rsrc/_setup.R")

set.seed(42)

# Fit GP on small archive
objective = toy1d_obj_rfundt
instance = OptimInstanceSingleCrit$new(objective = objective, terminator = trm("none"))
instance$eval_batch(data.table(x = c(0.05, 0.50, 0.95)))

surrogate = my_gp_surrogate(instance$archive)
surrogate$update()

# Joint posterior on a dense grid: mean + Cholesky of covariance.
# We pull the underlying DiceKriging model out of the mlr3 wrapper to access
# the full predictive covariance (mlr3 wrappers only expose the marginals).
gx = seq(0, 1, length.out = 401)
grid = data.table(x = gx)
grid[, y_truth := objective$eval_dt(grid)$y]
pred_marg = surrogate$predict(grid)
grid[, mean := pred_marg$mean]
grid[, lo := mean - pred_marg$se]
grid[, hi := mean + pred_marg$se]

# DiceKriging joint predictive covariance matrix on the grid
km_model = surrogate$learner$model
pred_full = DiceKriging::predict.km(km_model, newdata = data.frame(x = gx),
                                    type = "UK", cov.compute = TRUE)
mu_grid = pred_full$mean
Sigma   = pred_full$cov + diag(1e-8, length(gx))   # jitter for numerical PSD
L = chol(Sigma)

# Draw q sample paths from the joint posterior
q = 4
samples = vapply(seq_len(q),
                 function(i) as.numeric(mu_grid + t(L) %*% rnorm(length(gx))),
                 numeric(length(gx)))
colnames(samples) = paste0("s", seq_len(q))

samples_long = melt(as.data.table(cbind(x = gx, samples)),
                    id.vars = "x", variable.name = "path", value.name = "y")

# Each path's argmin = one Thompson-sampling proposal
proposals = samples_long[, .(x = x[which.min(y)], y = min(y)), by = path]

# ---- plot --------------------------------------------------------------------
g = ggplot() +
  # GP posterior mean and 1-sigma band
  geom_ribbon(aes(x = x, ymin = lo, ymax = hi),
              data = grid, fill = "steelblue", alpha = 0.12) +
  geom_line(aes(x = x, y = mean), data = grid,
            colour = "steelblue", linetype = 2, linewidth = 0.5) +
  # sample paths
  geom_line(aes(x = x, y = y, colour = path),
            data = samples_long, linewidth = 0.55, alpha = 0.85) +
  # truth (very thin)
  geom_line(aes(x = x, y = y_truth), data = grid,
            colour = "black", linewidth = 0.3, alpha = 0.6) +
  # archive
  geom_point(aes(x = x, y = y), data = instance$archive$data,
             size = 2.5, colour = "black") +
  # per-path argmin -> Thompson proposal
  geom_point(aes(x = x, y = y, colour = path),
             data = proposals, size = 3.2, shape = 18) +
  scale_colour_brewer(palette = "Set1", guide = "none") +
  coord_cartesian(ylim = c(-2.2, 2.2)) +
  labs(x = expression(lambda), y = expression(c)) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

myggsave("08_thompson_sampling", plot = g, width = 5, height = 2.6)
