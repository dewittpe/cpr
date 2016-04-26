/******************************************************************************/
/* file: deboor.cpp                                                           */
/* author: Peter DeWitt, dewittpe at gmail.com                                */
/*                                                                            */
/* Implementation of de Boor's recursive algorithm for generating B-splines   */
/* See de Boor (2001) page 90 for more details                                */
/******************************************************************************/

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// greville sites
//' @export
//' @param xi full knot vector
//' @rdname bsplines
// [[Rcpp::export]]
arma::vec greville_sites(arma::vec xi, unsigned int order) {
  arma::vec xi_star(xi.n_elem - order);

  for (int i=0; i < xi_star.n_elem; ++i) {
    xi_star(i) = arma::sum(xi(arma::span(i + 1, i + order - 1))) / double (order - 1);
  }

  return(xi_star);
}


// class definition for bsplines
class bsplines { 
  public:
    // constructor and deconstructor
    // x is the x, xi the knot vector, order of the polynomial
    bsplines(arma::vec x_, arma::vec iknots_, arma::vec bknots_, unsigned int order_); 

    //implicit deconstructor will be sufficient ?
    //~bsplines();  

    unsigned int get_order() { return order; }
    void         set_order(unsigned int order_) { order = order_; }

    arma::mat get_Bmat() { return Bmat; }
    void      set_Bmat(arma::mat Bmat_) { Bmat = Bmat_; }

    arma::vec get_xi() { return xi; }
    void      set_xi(arma::vec xi_) { xi = xi_; }

    arma::vec get_xi_star() { return xi_star; }
    void      set_xi_star(arma::vec xi_star_) { xi_star = xi_star_; }

  private:
    unsigned int order; // order of the polynomials
    arma::vec x; // x and unique x values (x is a synonym data)
    arma::mat Bmat; // Bspline basis matrix and unique rows
    arma::vec xi;   // knot vector 
    arma::vec xi_star; // knot averages

    // B spline recusance relationship.  B will call w
    // See page 90 of de Boor (2001)
    double B(double x, unsigned int j, unsigned int k);  
    double w(double x, unsigned int j, unsigned int k);
};

/******************************************************************************/
/* Function Definitions                                                       */
/******************************************************************************/
bsplines::bsplines(arma::vec x_, arma::vec iknots, arma::vec bknots, unsigned int order_) {
  // counters
  unsigned int i, j; 

  // set the order of the B-splines
  order = order_;

  // set the x values
  x = x_;

  // create the full knot vector
  xi.set_size(order * 2 + iknots.n_elem);
  for (i = 0; i < order; ++i) {
    xi(i) = bknots(0);
    xi(order + iknots.n_elem + i) = bknots(1);
  }
  for (i = 0; i < iknots.n_elem; ++i) {
    xi(order + i) = iknots(i);
  }

  // set the size of the basis matrix
  Bmat.set_size(x.n_elem, order + iknots.n_elem); 

  Bmat.fill(0.0);

  for (i = 0; i < x.n_elem; ++ i) { 
    if (x(i) == bknots(1)) {
      Bmat(i, order - 1 + iknots.n_elem) = 1.0;
    }
    else { 
      for (j = 0; j < order + iknots.n_elem; ++j) { 
        Bmat(i, j) = B(x(i), j, order); 
      }
    }
  }

  // set the knot averages
  xi_star.set_size(order + iknots.n_elem);
  for(j = 0; j < xi_star.n_elem; ++j) { 
    xi_star(j) = arma::sum(xi.subvec(j + 1, j + order - 1)) / (order - 1);
  }
}


double bsplines::w(double x, unsigned int j, unsigned int k) { 

  double w = 0.0;
  if (xi(j + k - 1) != xi(j)) { 
    w = (x - xi(j)) / (xi(j + k - 1) - xi(j));
  }

  return(w);
}

double bsplines::B(double x, unsigned int j, unsigned int k) {
  double rtn;

  if (k == 1) { 
    if ((xi(j) <= x) && (x < xi(j + 1))) {
      rtn = 1.0;
    } else { 
      rtn = 0.0;
    }
  } 
  else { 
    rtn = w(x, j, k) * B(x, j, k - 1) + (1.0 - w(x, j + 1, k)) * B(x, j + 1, k - 1);
  }

  return(rtn);
}

/******************************************************************************/
/* Exported functions                                                         */
/******************************************************************************/ 

// [[Rcpp::export]]
Rcpp::List bsplines__impl(arma::vec x, arma::vec iknots, arma::vec bknots, unsigned int order) { 

  if (!iknots.is_sorted()) { 
    Rcpp::warning("Sorting iknots");
    iknots = arma::sort(iknots);
  }

  bsplines Bspline(x, iknots, bknots, order);

  return Rcpp::List::create(
      Rcpp::Named("Bmat")    = Bspline.get_Bmat(),
      Rcpp::Named("order")   = order,
      Rcpp::Named("iknots")  = iknots,
      Rcpp::Named("bknots")  = bknots,
      Rcpp::Named("xi")      = Bspline.get_xi(),
      Rcpp::Named("xi_star") = Bspline.get_xi_star());
}




/******************************************************************************/
/* end of file                                                                */
/******************************************************************************/ 
