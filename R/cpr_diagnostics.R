#' @export
#' @rdname cp
#' @param obj a cpr_cpr object
#' @param err max difference/error between the vertices of cp1 to cp2
cpr_select <- function(obj, err = 0.01) { 
  diffs <- mapply(function(x1, x2) { cp_diff(x1$cp, x2$cp) }, 
                  x1 = obj[-length(obj)],
                  x2 = obj[-1])
  min(which(!sapply(diffs, function(x) { all(x < err) })))
}

