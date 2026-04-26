# Used in: 04_BO/03_gps.tex
#
# 2D GP prior samples under different length-scale regimes, to show how the
# length scales shape the typical landscape:
#   1) small isotropic l                 -> wiggly in both dims
#   2) large isotropic l                 -> very smooth
#   3) ARD with l1 << l2                 -> ridge-like (anisotropic)

library(bbotk)
library(data.table)
library(ggplot2)
source("rsrc/_setup.R")

set.seed(11)

# ------------------------------------------------------------------------------
# RBF kernel with per-dim length scales l = (l1, l2). Vector x, x' in R^2.
# Squared ARD distance: sum_j (x_j - x'_j)^2 / l_j^2
make_rbf_ard = function(l) {
  function(X1, X2) {
    # X1: n1 x 2, X2: n2 x 2; return n1 x n2 kernel matrix
    sq = matrix(0, nrow(X1), nrow(X2))
    for (j in seq_len(2)) {
      d = outer(X1[, j], X2[, j], "-") / l[j]
      sq = sq + d^2
    }
    exp(-sq / 2)
  }
}

# Sample one GP prior path on grid X (n x 2) for given length scales.
sample_2d = function(X, l) {
  k = make_rbf_ard(l)
  K = k(X, X) + diag(1e-6, nrow(X))   # jitter for numerical PSD
  L = chol(K)
  as.numeric(t(L) %*% rnorm(nrow(X)))
}

# ------------------------------------------------------------------------------
res = 50L
gx  = seq(-3, 3, length.out = res)
X   = as.matrix(CJ(x1 = gx, x2 = gx))

cases = list(
  list(label = "isotropic, small l",       l = c(0.5, 0.5)),
  list(label = "isotropic, large l",       l = c(2.0, 2.0)),
  list(label = "ARD: small l1, large l2",  l = c(0.4, 2.5))
)

panels = rbindlist(lapply(cases, function(cc) {
  z = sample_2d(X, cc$l)
  data.table(x1 = X[, 1], x2 = X[, 2], z = z, panel = cc$label)
}))
panels[, panel := factor(panel, levels = vapply(cases, `[[`, "", "label"))]

g = ggplot(panels, aes(x = x1, y = x2, fill = z)) +
  geom_raster() +
  geom_contour(aes(z = z), colour = "white", alpha = 0.4,
               linewidth = 0.25, bins = 8) +
  facet_wrap(~ panel, nrow = 1) +
  scale_fill_viridis_c(option = "mako", direction = -1) +
  coord_equal(expand = FALSE) +
  labs(x = expression(lambda[1]), y = expression(lambda[2])) +
  theme_minimal(base_size = 9) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold")
  )

myggsave("gp_lengthscale_2d", plot = g, width = 9, height = 3)
