// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

class bspline { 
  public:
    // constructors
    bspline();
    bspline(arma::vec x_, unsigned int j_, unsigned int order_, arma::vec knots_); 

    //implicit deconstructor will be sufficient ?

    unsigned int get_order() { return order; }
    void         set_order(unsigned int order_) { order = order_; }

    arma::vec get_data() { return data; }
    void      set_data(arma::vec data_) { data = data_; }

    arma::vec get_xi() { return xi; }
    void      set_xi(arma::vec xi_) { xi = xi_; }

    unsigned int get_j() { return j; }
    void      set_j(unsigned int j_) { j = j_; }

    arma::vec get_Bj() { return Bj; }
    void      set_Bj();

  private:
    unsigned int order; // order of the polynomials
    unsigned int j;
    arma::vec data;     // data
    arma::vec xi;       // knot vector 
    arma::vec Bj;       // the jth bspline


    // B spline recusance relationship.  B will call w
    // See page 90 of de Boor (2001)
    double B(double x_, unsigned int j_, unsigned int k_);  
    double w(double x_, unsigned int j_, unsigned int k_);
};

/******************************************************************************/
/* Function Definitions                                                       */
/******************************************************************************/
bspline::bspline() { 
}
bspline::bspline(arma::vec x_, unsigned int j_, unsigned int order_, arma::vec knots_) {
  set_xi(knots_);
  set_order(order_);
  set_data(x_);
  set_j(j_);
  set_Bj();
}
 
void bspline::set_Bj() {
  // declare a vector for the jth bspline, fill with zeros
  Bj.zeros(data.n_elem); 

  for (unsigned int i = 0; i < data.n_elem; ++i) {
    if (data(i) >= xi(j) && data(i) <= xi(j + order)) {
      Bj(i) = B(data(i), j, order); 
    }
  }
}


double bspline::w(double x_, unsigned int j_, unsigned int k_) { 
  double w = 0.0;
  if (xi(j_ + k_ - 1) != xi(j_)) { 
    w = (x_ - xi(j_)) / (xi(j_ + k_ - 1) - xi(j_));
  }

  return(w);
}

double bspline::B(double x_, unsigned int j_, unsigned int k_) {
  double rtn;

  if (k_ == 1) { 
    if ((xi(j_) <= x_) && (x_ < xi(j_ + 1))) { 
      rtn = 1.0; 
    } else { 
      rtn = 0.0;
    } 
  } else { 
    rtn = w(x_, j_, k_) * B(x_, j_, k_ - 1) + (1.0 - w(x_, j_ + 1, k_)) * B(x_, j_ + 1, k_ - 1);
  }

  return(rtn);
}

template <typename T>
Rcpp::NumericVector arma2vec(const T& x) {
    return Rcpp::NumericVector(x.begin(), x.end());
}

/******************************************************************************/
/* Exported functions                                                         */
/******************************************************************************/ 

// [[Rcpp::export]]
Rcpp::NumericVector greville_sites(arma::vec xi, unsigned int order) {
  arma::vec xi_star(xi.n_elem - order);

  for (int i=0; i < xi_star.n_elem; ++i) {
    xi_star(i) = arma::sum(xi(arma::span(i + 1, i + order - 1))) / double (order - 1);
  }

  return arma2vec(xi_star);
}

// [[Rcpp::export]]
Rcpp::NumericVector bspline__impl(arma::vec x, unsigned int j, unsigned int order, arma::vec knots) { 

  bspline BJ(x, j, order, knots);

  Rcpp::NumericVector out = arma2vec(BJ.get_Bj());
  out.attr("order")   = order;
  out.attr("xi") = arma2vec(BJ.get_xi());

  return out;
}

// [[Rcpp::export]]
Rcpp::NumericVector bsplineD1__impl(arma::vec x, unsigned int j, unsigned int order, arma::vec knots) { 

  double a = 0.0;
  double b = 0.0;
  arma::vec A1;
  arma::vec A2;
  arma::vec A3;
  A1.zeros(x.n_elem);
  A2.zeros(x.n_elem);
  A3.zeros(x.n_elem);

  if (knots(j + order - 1) - knots(j) != 0) { 
    bspline A(x, j, order - 1, knots); 
    A1 = A.get_Bj()/(knots(j + order - 1) - knots(j));
  }

  if (knots(j + order) - knots(j + 1) != 0) { 
    bspline B(x, j + 1, order - 1, knots);
    A2 = B.get_Bj() / (knots(j + order) - knots(j + 1));
  }
    
  A3 = double(order-1) * (A1 - A2);
  Rcpp::NumericVector out = arma2vec(A3);

  return out;
}

//[[Rcpp::export]]
Rcpp::NumericVector bsplineD2__impl(arma::vec x, unsigned int j, unsigned int order, arma::vec knots) { 

  double a = 0.0;
  double b = 0.0;
  arma::vec A1;
  arma::vec A2;
  arma::vec A3;
  arma::vec A4;
  A1.zeros(x.n_elem);
  A2.zeros(x.n_elem);
  A3.zeros(x.n_elem);
  A4.zeros(x.n_elem);

  a = knots(j + order) - knots(j + 2);
  if (a != 0.0) { 
    bspline Aj(x, j + 2, order - 2, knots);
    A1 = -1.0 * Aj.get_Bj() / a;
  }

  a = knots(j + order - 1) - knots(j + 1);
  if (a != 0.0) { 
    bspline Bj(x, j + 1, order - 2, knots);
    A2 = Bj.get_Bj() / a;
  }
  
  a = knots(j + order - 2) - knots(j);
  if (a != 0.0) {
    bspline Cj(x, j, order - 2, knots);
    A3 = Cj.get_Bj() / a;
  }

  a = knots(j + order) - knots(j + 1);
  if (a != 0.0) { 
    a = -1.0 / a;
  }

  b = knots(j + order - 1) - knots(j);
  if (b != 0.0) { 
    b = 1.0 / b;
  }

  A4 = double(order - 1) * double(order - 2) * (a * (A1 + A2) + b * (A3 - A2));

  Rcpp::NumericVector out = arma2vec(A4); 
  return out;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix bbasis__impl(arma::vec x, arma::vec iknots, arma::vec bknots, unsigned int order) { 

  unsigned int i,j;
  arma::mat bmat(x.n_elem, iknots.n_elem + order);

  arma::vec knots(iknots.n_elem + 2 * order);
  for (i = 0; i < order; ++i) {
    knots(i) = bknots(0);
    knots(order + iknots.n_elem + i) = bknots(1);
  }
  for (i = 0; i < iknots.n_elem; ++i) {
    knots(order + i) = iknots(i);
  }

  if (!knots.is_sorted()) { 
    Rcpp::warning("Sorting knots");
    knots = arma::sort(knots);
  }

  bspline BJ(x, 0, order, knots);
  bmat.col(0) = BJ.get_Bj();

  for(j = 1; j < bmat.n_cols; ++j) {
    BJ.set_j(j);
    BJ.set_Bj();
    bmat.col(j) = BJ.get_Bj();
  }

  arma::uvec bx = arma::find(x == bknots(1));
  arma::uvec jx(bx.n_elem); jx.fill(j - 1);
  bmat(bx, jx).fill(1.0);

  Rcpp::NumericMatrix out = Rcpp::wrap(bmat);
  out.attr("order")   = order;
  out.attr("iknots")  = arma2vec(iknots);
  out.attr("bknots")  = arma2vec(bknots);
  out.attr("xi")      = arma2vec(knots);
  out.attr("xi_star") = greville_sites(knots, order);
  out.attr("class")   = "cpr_bs";

  return out; 
}


/******************************************************************************/
/* end of file                                                                */
/******************************************************************************/ 
