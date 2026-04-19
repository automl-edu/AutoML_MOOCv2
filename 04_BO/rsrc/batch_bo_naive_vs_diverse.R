# Used in: 04_BO/07_parallel_bo.tex

#  top-q maximizers oof EI vs diverse batch

library(bbotk)
library(data.table)
library(mlr3mbo)
library(mlr3learners)
library(ggplot2)
library(patchwork)
source("rsrc/_setup.R")

set.seed(42)

# Fit GP on a small LHS design of Branin and compute EI on a grid.

instance = OptimInstanceSingleCrit$new(objective = branin_obj_rfundt, terminator = trm("none"))
design = generate_design_lhs(instance$search_space, n = 8)$data
instance$eval_batch(design)

surrogate = srlrn(
  lrn("regr.km", covtype = "matern5_2", optim.method = "BFGS",
      nugget.stability = 1e-8, control = list(trace = FALSE)),
  archive = instance$archive
)
acq = acqf("ei", surrogate = surrogate)
acq$surrogate$update()
acq$update()

res = 150L
x1 = seq(-5, 10, length.out = res)
x2 = seq( 0, 15, length.out = res)
grid = as.data.table(expand.grid(x1 = x1, x2 = x2))
grid[, ei := acq$eval_dt(grid)$acq_ei]

# ------------------------------------------------------------------------------
# Batches.

q = 8

# Naive: the q grid cells with highest EI (they cluster).
naive = grid[order(-ei)][seq_len(q)]

# Diverse: greedy distance-filtered top-q (cheap stand-in for local penalization /
#   Kriging Believer).  After each pick, exclude a ball of radius r around it so
#   the next pick has to come from a different EI peak.
greedy_diverse = function(grid, q, r) {
  cand = grid[order(-ei)]
  out = cand[1]
  for (i in 2:nrow(cand)) {
    if (nrow(out) >= q) break
    d = sqrt((cand$x1[i] - out$x1)^2 + (cand$x2[i] - out$x2)^2)
    if (min(d) >= r) out = rbind(out, cand[i])
  }
  out
}
diverse = greedy_diverse(grid, q = q, r = 2.5)

# ------------------------------------------------------------------------------
# Plot.

plot_ei = function(batch, subtitle, annotate_cluster = FALSE) {
  p = ggplot(grid, aes(x = x1, y = x2)) +
    geom_raster(aes(fill = ei)) +
    geom_contour(aes(z = ei), colour = "white", alpha = 0.4, linewidth = 0.3, bins = 8) +
    geom_point(aes(x = x1, y = x2), data = instance$archive$data,
               shape = 4, colour = "#ffcc00", size = 3, stroke = 1.5) +
    geom_jitter(aes(x = x1, y = x2), data = batch,
                shape = 21, colour = "black", fill = "#e41a1c", size = 3.6,
                alpha = 0.8, width = 0.25, height = 0.25) +
    scale_fill_viridis_c(option = "mako", direction = -1, name = "EI") +
    coord_cartesian(xlim = c(-5, 10), ylim = c(0, 15), expand = FALSE) +
    labs(subtitle = subtitle, x = expression(lambda[1]), y = expression(lambda[2])) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      plot.subtitle = element_text(face = "bold")
    )
  if (annotate_cluster) {
    cx = mean(batch$x1); cy = mean(batch$x2)
    p = p +
      annotate("segment", x = cx + 2.5, y = cy - 1.5, xend = cx + 0.7, yend = cy - 0.3,
               arrow = arrow(length = unit(0.15, "cm")), colour = "black") +
      annotate("label", x = cx + 2.8, y = cy - 1.8,
               label = sprintf("all %d points\nhere", nrow(batch)),
               size = 3, hjust = 0, label.size = 0, fill = alpha("white", 0.85))
  }
  p
}

p = plot_ei(naive,   "Naive top-q maximizers of EI -- cluster at one peak",
            annotate_cluster = TRUE) +
    plot_ei(diverse, "Diverse batch -- spreads across several EI peaks")

myggsave("batch_bo_naive_vs_diverse", plot = p, width = 8, height = 3.2)
