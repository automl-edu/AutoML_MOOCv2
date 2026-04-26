# Used in: 04_BO/03_gps.tex
#
# Draw 1D GP prior sample paths to show how the kernel choice shapes the
# typical landscape: RBF priors are very smooth, Matérn priors get rougher
# as nu shrinks. Two figures, one per slide, colored to match the
# correlation-vs-distance pgfplots above each slide.

library(bbotk)
library(data.table)
library(ggplot2)
source("rsrc/_setup.R")

set.seed(31)

# ------------------------------------------------------------------------------
# Kernel definitions on scalar distance d, length scale l.
rbf_k    = function(d, l)             exp(-d^2 / (2 * l^2))
matern_k = function(d, nu, l = 1) {
  r = d / l
  if (nu == 0.5) return(exp(-r))
  if (nu == 1.5) return((1 + sqrt(3) * r) * exp(-sqrt(3) * r))
  if (nu == 2.5) return((1 + sqrt(5) * r + 5 * r^2 / 3) * exp(-sqrt(5) * r))
  stop("unsupported nu")
}

# Sample one path from N(0, K) on a 1D grid `x` for kernel k(d, ...).
sample_path = function(x, kfun, ...) {
  D = abs(outer(x, x, "-"))
  K = kfun(D, ...) + diag(1e-6, length(x))   # jitter for numerical PSD
  L = chol(K)
  as.numeric(t(L) %*% rnorm(length(x)))
}

x = seq(0, 3, length.out = 401)

# ------------------------------------------------------------------------------
# RBF: three length scales to match the top plot's legend (0.3, 0.7, 1.5).
rbf_paths = rbindlist(lapply(c(0.3, 0.7, 1.5), function(l) {
  data.table(x = x, y = sample_path(x, rbf_k, l = l), l = sprintf("l=%g", l))
}))
rbf_paths[, l := factor(l, levels = c("l=0.3", "l=0.7", "l=1.5"))]

g_rbf = ggplot(rbf_paths, aes(x = x, y = y, colour = l)) +
  geom_line(linewidth = 0.5) +
  scale_colour_manual(
    values = c("l=0.3" = "blue", "l=0.7" = "darkgreen", "l=1.5" = "red"),
    name = NULL
  ) +
  labs(x = expression(x), y = expression(f(x))) +
  theme_minimal(base_size = 9) +
  theme(
    legend.position = c(0.99, 0.99),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = alpha("white", 0.7), colour = NA),
    legend.key.height = unit(0.35, "cm"),
    panel.grid.minor = element_blank()
  )

myggsave("rbf_samples", plot = g_rbf, width = 4.5, height = 1.8)

# ------------------------------------------------------------------------------
# Matérn: nu in {1/2, 3/2, 5/2} at l=1, matching the top plot's legend.
matern_paths = rbindlist(lapply(c(0.5, 1.5, 2.5), function(nu) {
  data.table(
    x = x,
    y = sample_path(x, matern_k, nu = nu, l = 1),
    nu = sprintf("nu=%s", c(`0.5` = "1/2", `1.5` = "3/2", `2.5` = "5/2")[as.character(nu)])
  )
}))
matern_paths[, nu := factor(nu, levels = c("nu=1/2", "nu=3/2", "nu=5/2"))]

g_matern = ggplot(matern_paths, aes(x = x, y = y, colour = nu)) +
  geom_line(linewidth = 0.5) +
  scale_colour_manual(
    values = c("nu=1/2" = "orange", "nu=3/2" = "darkgreen", "nu=5/2" = "blue"),
    name = NULL
  ) +
  labs(x = expression(x), y = expression(f(x))) +
  theme_minimal(base_size = 9) +
  theme(
    legend.position = c(0.99, 0.99),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = alpha("white", 0.7), colour = NA),
    legend.key.height = unit(0.35, "cm"),
    panel.grid.minor = element_blank()
  )

myggsave("matern_samples", plot = g_matern, width = 4.5, height = 1.8)
