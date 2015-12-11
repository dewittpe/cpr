#' Control Polygon Reduction Diagnoses
#'
#' A collection of function for the inspection and evaluation of the control
#' polygon reduction.
#'
#' 
#' @export
#' @rdname cpr_diagnostics
#' @param obj a cpr_cpr object
#' @param tol max difference/error between the vertices of cp1 to cp2
cpr_select <- function(obj, tol = 3) { 
  diffs <- mapply(function(x1, x2) { cp_diff(x1, x2) }, 
                  x1 = obj[-length(obj)],
                  x2 = obj[-1])
  # mxdif <- sapply(diffs, max)
  # mndif <- sapply(diffs, mean)
  mwt <- sapply(obj, function(x) { unname(min(attr(x, "weights"))) })


  dat <- 
    dplyr::data_frame(n_iknots      = seq(0, length(obj) - 1L, by = 1), 
                      min_weight    = mwt, 
                      # max_diff      = c(mxdif, NA),
                      # mean_diff     = c(mndif, NA), 
                      diffs         = c(diffs, NA)) 
  best_cp_index <- max(which(mwt > median(mwt, na.rm = TRUE) + tol * IQR(mwt, na.rm = TRUE)))

  out <- list(best_cp = obj[[best_cp_index]], all_cps = dat)
  attr(out, "best_cp_index") <- best_cp_index 
  attr(out, "tol") <- tol
  class(out) <- c("cpr_selected", class(out))
  return(out)
}

#' @method print cpr_selected
#' @export
print.cpr_selected <- function(x, ...) { 
  cat("Selected model with", length(attr(x$best_cp, "iknots")), "internal knots:\n")
  print(attr(x$best_cp, "iknots")) 
  cat("\nThe selected CP is:\n")
  print(x$best_cp)
}

# @method plot cpr_cp
# @export 
# plot.cpr_cp <- function(x, ...) { 
# 
#   .data <- bind_rows(c(x, ...), .id = "row") 
# 
#   ggplot(.data) +
#   theme_bw() + 
#   aes(x = xi_star, y = theta, linetype = factor(row)) + 
#   geom_point() + geom_line() 
# }

#' @method plot cpr_cpr
#' @export 
plot.cpr_cpr <- function(x, a = 0, b = 9) { 

  cp_data <-
    bind_rows(x[seq(a, b, by = 1L) + 1L], .id = "row") %>%
    mutate(`row` = as.integer(`row`))

  foo <- function(.data) { 
    ggplot(.data) +
    theme_bw() + 
    aes(x = xi_star, y = theta, linetype = factor(`row`)) + 
    geom_point() + geom_line()  + 
    theme(legend.position = "none") 
  }

  mapply(function(aa, bb) { cp_data %>% filter(`row` %in% aa:bb) %>% 
         {foo(.) + ggtitle(paste("Internal knots:\n", aa - 1, "(solid) to", 
                                 bb - 1, "(dashed)", collapse = " "))}},
         aa = seq(a, b - 1, by = 1) + 1, 
         bb = seq(a + 1, b, by = 1) + 1, 
         SIMPLIFY = FALSE)  %>%
  lapply(., ggplotGrob) %>%
  grid.arrange(grobs = .)
}

#' @export 
#' @rdname cpr_diagnostics
#' @param start index of the first control polygon to plot
#' @param end index of the last control polygon to plot
cpr_layers <- function(obj, start = 1L, end = 6L) { 
  dat <- lapply(seq(start, end, by = 1L), function(i) { dplyr::mutate(obj[[i]]$cp, index = as.character(i)) })
  dat <- do.call(rbind, dat)

  list(ggplot2::geom_point(data = dat, mapping = ggplot2::aes_string(x = "xi_star", y = "theta", color = "index")),
       ggplot2::geom_line(data = dat, mapping = ggplot2::aes_string(x = "xi_star", y = "theta", color = "index")))
}


