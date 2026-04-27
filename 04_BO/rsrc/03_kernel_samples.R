# Used in: 04_BO/03_gps.tex
#
# Draw 1D GP prior sample paths to show how the kernel choice shapes the
# typical landscape: RBF priors are very smooth, Matérn priors get rougher
# as nu shrinks. Two figures, one per slide, colored to match the
# correlation-vs-distance pgfplots above each slide.

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
# Shared style for the two correlation-vs-distance panels (k vs d).
corr_theme = function() list(
  labs(x = expression(d(x, x*minute)), y = expression(k)),
  scale_y_continuous(limits = c(0, 1.05)),
  scale_x_continuous(limits = c(0, 3)),
  theme_minimal(base_size = 13),
  theme(
    legend.position = c(0.99, 0.99),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = alpha("white", 0.7), colour = NA),
    legend.key.height = unit(0.35, "cm"),
    panel.grid.minor = element_blank()
  )
)

# RBF correlation curves at the same length scales as the sample-path plot.
d = seq(0, 3, length.out = 401)
rbf_corr = rbindlist(lapply(c(0.3, 0.7, 1.5), function(l) {
  data.table(d = d, k = rbf_k(d, l), l = sprintf("l=%g", l))
}))
rbf_corr[, l := factor(l, levels = c("l=0.3", "l=0.7", "l=1.5"))]

g_rbf_corr = ggplot(rbf_corr, aes(x = d, y = k, colour = l)) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(
    values = c("l=0.3" = "#0072B2", "l=0.7" = "#009E73", "l=1.5" = "#D55E00"),
    name = NULL
  ) +
  corr_theme()

myggsave("03_kernel_rbf_corr", plot = g_rbf_corr, width = 4.5, height = 2.4)

# Matérn correlation curves at l=1, three smoothness levels.
matern_corr = rbindlist(lapply(c(0.5, 1.5, 2.5), function(nu) {
  data.table(
    d = d,
    k = matern_k(d, nu, l = 1),
    nu = sprintf("nu=%s", c(`0.5` = "1/2", `1.5` = "3/2", `2.5` = "5/2")[as.character(nu)])
  )
}))
matern_corr[, nu := factor(nu, levels = c("nu=1/2", "nu=3/2", "nu=5/2"))]

g_matern_corr = ggplot(matern_corr, aes(x = d, y = k, colour = nu)) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(
    values = c("nu=1/2" = "#E69F00", "nu=3/2" = "#009E73", "nu=5/2" = "#0072B2"),
    name = NULL
  ) +
  corr_theme()

myggsave("03_kernel_matern_corr", plot = g_matern_corr, width = 4.5, height = 2.4)

# ------------------------------------------------------------------------------
# RBF: three length scales to match the top plot's legend (0.3, 0.7, 1.5).
rbf_paths = rbindlist(lapply(c(0.3, 0.7, 1.5), function(l) {
  data.table(x = x, y = sample_path(x, rbf_k, l = l), l = sprintf("l=%g", l))
}))
rbf_paths[, l := factor(l, levels = c("l=0.3", "l=0.7", "l=1.5"))]

g_rbf = ggplot(rbf_paths, aes(x = x, y = y, colour = l)) +
  geom_line(linewidth = 0.5) +
  scale_colour_manual(
    values = c("l=0.3" = "#0072B2", "l=0.7" = "#009E73", "l=1.5" = "#D55E00"),
    name = NULL
  ) +
  labs(x = expression(x), y = expression(f(x))) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = c(0.99, 0.99),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = alpha("white", 0.7), colour = NA),
    legend.key.height = unit(0.35, "cm"),
    panel.grid.minor = element_blank()
  )

myggsave("03_kernel_samples_rbf", plot = g_rbf, width = 4.5, height = 1.8)

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
    values = c("nu=1/2" = "#E69F00", "nu=3/2" = "#009E73", "nu=5/2" = "#0072B2"),
    name = NULL
  ) +
  labs(x = expression(x), y = expression(f(x))) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = c(0.99, 0.99),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = alpha("white", 0.7), colour = NA),
    legend.key.height = unit(0.35, "cm"),
    panel.grid.minor = element_blank()
  )

myggsave("03_kernel_samples_matern", plot = g_matern, width = 4.5, height = 1.8)
