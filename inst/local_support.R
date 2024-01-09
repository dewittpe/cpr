################################################################################
# file: local_support.R
#
# Show the local support of B-splines
library(cpr)
library(data.table)

x <- runif(n = 1e5)
ORDER <- 4

bmat <- bsplines(x = x, df = 20, bknots = c(0, 1), order = ORDER)

theta <- 2 * sin(seq(0, 2 * pi, length.out = 20)) + 3 * seq(0, 2 * pi, length.out = 20) * cos(3.5 * seq(0, 2 * pi, length.out = 20)) + rnorm(20, mean = 2)

acp <- cp(bmat, theta)

plot(acp, show_spline = TRUE)

influence_of_iknots(acp) |> str(max.level = 1)

restored_splines <-
  influence_of_iknots(acp) |>
  getElement(object = _, name = "restored_cps") |>
  lapply(get_spline, n = 2000) |>
  lapply(as.data.table) |>
  rbindlist(idcol = "knot")
restored_splines[, knot := factor(knot, labels = paste0("xi[", unique(knot) + ORDER, "]"))]

original_spline <-
  get_spline(acp, n = 2000)|>
  as.data.table()
original_spline[, knot := "original"]

splines <-
  rbind(original_spline, restored_splines) |>
  dcast(x ~ knot, value.var = "y") |>
  melt(id.vars = c("x", "original"))

splines[, delta := value - original]
#splines[, delta_color := all.equal(delta, 0), by = .(x, variable)]
splines[, delta_color := factor(isTRUE(all.equal(delta, 0)), c(TRUE, FALSE), c("Equal", "Different")), by = .(x, variable)]
splines[, xmax := shift(x, type = "lead")]

splines[variable == "xi[19]"] |> print(n = Inf)

foo <- function(v) {
  plot(acp, show_spline = TRUE) +
    ggplot2::geom_path(data = splines[variable == v]
                       , mapping = ggplot2::aes(x = x, y = value, color = "restored spline")
                       , inherit.aes = FALSE
                       ) +
    ggplot2::geom_rect(data = splines[variable == v & x > 0 & delta_color == "Different"]
                         , mapping = ggplot2::aes(xmin = x, xmax = xmax, ymin = -Inf, ymax = Inf, fill = delta_color)
                         , inherit.aes = FALSE
                         , alpha = 0.2
                         ) +
    ggplot2::scale_fill_manual(name = "", values = c("Equal" = "green", "Different" = "red")) +
    ggplot2::ggtitle(parse(text = v))
}

plots <- lapply(levels(splines$variable), foo)
ggpubr::ggarrange(plotlist = plots, common.legend = TRUE)

################################################################################
#                                 End of File                                  #
################################################################################
