#include <Rcpp.h>
using namespace Rcpp;

//' @title Convert -32767 (missing info) to NA
//' @description Converts all -32767 (smallest INT16  value indicating missing info) to NA.
//' You don't need to call this function directly, as it is automatically envoked within
//' \code{\link{read_edf}} function.
//' @param List original_frame, data.frame to be processed
//' @return processed data.frame
//' @examples
//' \donttest{
//'   gaze <- read_edf(system.file("extdata", "example.edf", package = "edfR"))
//'   gaze$samples <- convert_NAs(GazeData$samples)
//' }

//[[Rcpp::export]]
List convert_NAs(List original_frame){
  List target_frame = clone(original_frame);
  for( List::iterator it = target_frame.begin(); it != target_frame.end(); ++it ) {
    switch( TYPEOF(*it) ) {
      case REALSXP: {
        NumericVector tmp = as<NumericVector>(*it);
        for(unsigned int iRow= 0; iRow<tmp.size(); iRow++){
          if ((tmp[iRow]<=-32767) || (tmp[iRow] >= 1e8)){
            tmp(iRow)= NA_REAL;
          }
        }
        break;
      }
      case INTSXP: {
        if( Rf_isFactor(*it) ) break; // factors have internal type INTSXP too
        IntegerVector tmp = as<IntegerVector>(*it);
        for(unsigned int iRow= 0; iRow<tmp.size(); iRow++){
          if (tmp[iRow]<=-32767 || (tmp[iRow] >= 1e8)){
            tmp(iRow)= NA_INTEGER;
          }
        }
        break;
      }
    }
  }

  // target_frame.attr("class") = "data.frame";
  return target_frame;
}
