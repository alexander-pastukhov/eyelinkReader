#include <Rcpp.h>
using namespace Rcpp;

#include "SRResearch/edf.h"

//' @title Version of the EDF API library
//' @description Returns version of the EDF API library used to interface an EDF file.
//' @export
//' @examples
//' eyelinkReader::library_version()
//[[Rcpp::export]]
CharacterVector library_version(){
  Rcpp::StringVector version_info(1);
  version_info[0]= edf_get_version();
  return version_info;
}
