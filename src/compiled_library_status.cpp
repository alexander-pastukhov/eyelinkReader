#include <Rcpp.h>
using namespace Rcpp;

//' @title Status of compiled library
//' @description Return status of compiled library
//' @return logical
//' @export
//' @examples
//' compiled_library_status()
//[[Rcpp::export]]
bool compiled_library_status(){
  // the other version, compiled alongside the library returns true
  return(false);
}
