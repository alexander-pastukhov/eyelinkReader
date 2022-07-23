#include <Rcpp.h>
using namespace Rcpp;


//' @title Reads preamble of the EDF file as a single string.
//' @description Reads preamble of the EDF file as a single string.
//' Please, do not use this function directly. Instead, call \code{\link{read_preamble}} function
//' that provides a more consistent interface.
//' @return string with the preamble
//' @export
//' @keywords internal
//' @examples
//' \donttest{
//' if (eyelinkReader::compiled_library_status()) {
//'   read_preamble(system.file("extdata", "example.edf", package = "eyelinkReader"))
//' }
//' }
//[[Rcpp::export]]
std::string read_preamble_str(std::string filename){
  return("");
}
