# Used in: 04_BO/06_acqf_opt.tex
#
# Visualize CMA-ES sampling: sample lambda candidates from N(m, sigma^2 C),
# rank by acquisition, update mean and covariance from the best mu, repeat.
# Shows three generations on a rotated narrow-valley quadratic so the learned
# covariance ellipse visibly rotates to follow the local curvature.

library(bbotk)
library(data.table)
library(ggplot2)
source("rsrc/_setup.R")

set.seed(7)

# ------------------------------------------------------------------------------
# Test landscape: rotated narrow valley.
#
# Pretend `f` is the (negative) acquisition we minimise; CMA-ES does not care
# about its analytical form -- only ranks of evaluated samples matter.
f = function(x1, x2) {
  u = (x1 + x2) / sqrt(2)
  v = (x1 - x2) / sqrt(2)
  6 * u^2 + 0.4 * v^2
}

gx = seq(-3, 3, length.out = 200)
grid = CJ(x1 = gx, x2 = gx)
grid[, z := f(x1, x2)]

# ------------------------------------------------------------------------------
# Minimal rank-mu CMA-ES (no step-size adaptation: we shrink sigma manually
# for a clean visual; the *mechanism* shown is the population sample +
# covariance update from the best mu, which is the slide's point).

n_gen  = 2
lambda = 10
mu     = 4
weights = log(mu + 1) - log(seq_len(mu))
weights = weights / sum(weights)

m     = c(-2.2, 2.2)
sigma = 0.6
C     = diag(2)

pop_rows  = list()
ell_rows  = list()
mean_rows = list()

# 2-sigma confidence ellipse points for current N(m, sigma^2 C)
ellipse_pts = function(m, sigma, C, npts = 80) {
  th   = seq(0, 2 * pi, length.out = npts)
  L    = chol(sigma^2 * C)
  pts  = (cbind(cos(th), sin(th)) * 2) %*% L
  data.table(x1 = pts[, 1] + m[1], x2 = pts[, 2] + m[2])
}

for (gen in seq_len(n_gen)) {
  # 1. sample lambda candidates from N(m, sigma^2 C)
  L = chol(sigma^2 * C)
  Z = matrix(rnorm(lambda * 2), ncol = 2)
  X = sweep(Z %*% L, 2, m, "+")
  vals = f(X[, 1], X[, 2])

  # 2. rank, take top mu
  ord = order(vals)
  best_idx = ord[seq_len(mu)]
  is_best  = seq_len(lambda) %in% best_idx

  # collect plot data for this generation
  pop_rows[[gen]]  = data.table(gen = gen, x1 = X[, 1], x2 = X[, 2], best = is_best)
  ell_rows[[gen]]  = cbind(gen = gen, ellipse_pts(m, sigma, C))
  mean_rows[[gen]] = data.table(gen = gen, x1 = m[1], x2 = m[2])

  # 3. update mean (weighted recombination of top mu)
  X_best = X[best_idx, , drop = FALSE]
  m_new = as.numeric(t(X_best) %*% weights)

  # 4. update covariance (rank-mu only, simplified)
  d = sweep(X_best, 2, m, "-") / sigma
  C_new = t(d) %*% (d * weights)

  m = m_new
  C = C_new
  sigma = sigma * 0.95  # gentle shrink so later-gen points don't pile up
}

pop  = rbindlist(pop_rows)
ell  = rbindlist(ell_rows)
mns  = rbindlist(mean_rows)

# ------------------------------------------------------------------------------
g = ggplot() +
  geom_contour(aes(x = x1, y = x2, z = z), data = grid,
               colour = "grey75", bins = 12, linewidth = 0.3) +
  geom_path(aes(x = x1, y = x2, colour = factor(gen), group = gen),
            data = ell, linetype = 2, linewidth = 0.6) +
  geom_point(aes(x = x1, y = x2, colour = factor(gen),
                 shape = best, size = best),
             data = pop, stroke = 0.7) +
  geom_point(aes(x = x1, y = x2, colour = factor(gen)),
             data = mns, shape = 4, size = 3.5, stroke = 1.2) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), guide = "none") +
  scale_size_manual(values = c(`TRUE` = 1.8, `FALSE` = 1.0), guide = "none") +
  scale_colour_brewer(palette = "Set1", name = "Gen.") +
  coord_equal(xlim = c(-3.6, 0.2), ylim = c(0.2, 4), expand = FALSE) +
  labs(x = expression(lambda[1]), y = expression(lambda[2])) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = c(0.97, 0.03),
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = alpha("white", 0.7), colour = NA),
    panel.grid.minor = element_blank()
  )

myggsave("06_cmaes_sampling", plot = g, width = 5, height = 5)
