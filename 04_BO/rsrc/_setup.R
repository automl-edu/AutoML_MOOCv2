# ------------------------------------------------------------------------------
# Shared setup for 04_BO figure scripts.
# Source this from every figure-generating R script (assuming WD = 04_BO/).
# ------------------------------------------------------------------------------

# myggsave
#
#   Convenience wrapper around ggplot2::ggsave that:
#     - saves into the 04_BO/images/ directory (WD must be 04_BO/)
#     - writes PDF via cairo_pdf (proper font metrics => clean Greek subscripts)
#
# Arguments
#   name    file stem without extension or directory, e.g. "initdes_random"
#   plot    ggplot object to save
#   width   in inches (default 5)
#   height  in inches (default 4)
#   ...     any other argument passed on to ggplot2::ggsave
myggsave = function(name, plot, width = 5, height = 4, ...) {
  ggplot2::ggsave(
    filename = file.path("images", paste0(name, ".pdf")),
    plot     = plot,
    width    = width,
    height   = height,
    device   = grDevices::cairo_pdf,
    ...
  )
}
