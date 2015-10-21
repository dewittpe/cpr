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
    bsplines(arma::vec x, arma::vec iknots, arma::vec bknots, unsigned int ord); 

    //implicit deconstructor will be sufficient ?
    //~bsplines();  

    arma::mat get_Bmat();

  private:
    unsigned int order;          // order of the ploynomials
    arma::vec data;       // data and unique data values
    arma::mat Bmat; // Bspline basis matrix and unique rows
   // arma::vec xi;                // knot vector 
};

/******************************************************************************/
/* Function Definitions                                                       */
/******************************************************************************/
bsplines::bsplines(arma::vec x, arma::vec iknots, arma::vec bknots, unsigned int ord) {
  order = ord;
  data  = x;
  //xi    = knots;

  Bmat.set_size(4, 4);
  Bmat.eye();

}

arma::mat bsplines::get_Bmat() { 
  return Bmat;
}

/******************************************************************************/
/* Exported functions                                                         */
/******************************************************************************/ 

// [[Rcpp::export]]
arma::mat generate_bsplines(arma::vec x, arma::vec iknots, arma::vec bknots, unsigned int k) { 
  bsplines B(x, iknots, bknots, k);
  return B.get_Bmat();
}

/******************************************************************************/
/* end of file                                                                */
/******************************************************************************/ 
