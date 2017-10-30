#include <Rcpp.h>
using namespace Rcpp;

#include "SRResearch/edf.h"

//' Returns version of the EDF API library
//'
//' @title Version of the EDF API library
//'
//' @description Returns version of the EDF API library used to interface an EDF file.
//'
//' @export
//[[Rcpp::export]]
CharacterVector library_version(){
  Rcpp::StringVector version_info(1);
  version_info[0]= edf_get_version();
  return version_info;
}
