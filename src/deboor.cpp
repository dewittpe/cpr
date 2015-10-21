/******************************************************************************/
/* file: deboor.cpp                                                           */
/* author: Peter DeWitt, dewittpe at gmail.com                                */
/*                                                                            */
/* Implementation of de Boor's recursive algorithm for generating B-splines   */
/* See de Boor (2001) page 90 for more details                                */
/******************************************************************************/

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>


// class definition for bsplines
class bsplines { 
  public:
    // constructor and deconstructor
    // x is the data, xi the knot vector, order of the polynomial
    bsplines(arma::vec x, 
             arma::vec iknots, 
             arma::vec bknots, 
             unsigned int ord); 

    //implicit deconstructor will be sufficient ?
    //~bsplines();  

    arma::mat get_Bmat();
    arma::vec get_xi();

  private:
    unsigned int order;          // order of the ploynomials
    arma::vec data;       // data and unique data values
    arma::mat Bmat; // Bspline basis matrix and unique rows
    arma::vec xi;   // knot vector 

    // B spline recurance relationship.  B will call w
    double B(double x, unsigned int j, unsigned int k);  
    double w(double x, unsigned int j, unsigned int k);
};

/******************************************************************************/
/* Function Definitions                                                       */
/******************************************************************************/
bsplines::bsplines(arma::vec x, arma::vec iknots, arma::vec bknots, unsigned int ord) {
  // counters
  unsigned int i, j; 

  // set the order of the B-splines
  order = ord;

  // set the data values
  data  = x;

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
}

arma::mat bsplines::get_Bmat() { 
  return Bmat;
}

arma::vec bsplines::get_xi() { 
  return xi;
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
arma::mat generate_bsplines(arma::vec x, 
                            arma::vec iknots, 
                            arma::vec bknots, 
                            unsigned int k) { 
  bsplines B(x, iknots, bknots, k);

  return B.get_Bmat();
}

// Rcpp::List generate_bsplines(arma::vec x, 
//                             arma::vec iknots, 
//                             arma::vec bknots, 
//                             unsigned int k) { 
//   bsplines B(x, iknots, bknots, k);
// 
//   return Rcpp::List::create(
//       Rcpp::Named("Bmat") = B.get_Bmat(),
//       Rcpp::Named("xi")   = B.get_xi());
// }

/******************************************************************************/
/* end of file                                                                */
/******************************************************************************/ 
