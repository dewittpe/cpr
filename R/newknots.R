newknots <- function(form, nk) { 
  rr <- function(x, nk) {
      if(is.call(x) && grepl("bsplines|btensor", deparse(x[[1]]))) {
        x$df <- NULL
        x$iknots <- nk
        x
      } else if (is.recursive(x)) {
        as.call(lapply(as.list(x), rr, nk))
      } else {
        x
      }
  }

  z <- lapply(as.list(form), rr, nk)   
  z <- eval(as.call(z))
  environment(z) <- environment(form)
  z
}
