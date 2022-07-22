#include <Rcpp.h>
using namespace Rcpp;


//' @title Internal function that reads EDF file
//' @description Reads EDF file into a list that contains events, samples, and recordings.
//' DO NOT call this function directly. Instead, use read_edf function that implements
//' parameter checks and additional postprocessing.
//' @param std::string filename, full name of the EDF file
//' @param int consistency, consistency check control (for the time stamps of the start
//' and end events, etc). 0, no consistency check. 1, check consistency and report.
//' 2, check consistency and fix.
//' @param bool import_events, load/skip loading events.
//' @param bool import_recordings, load/skip loading recordings.
//' @param bool import_samples, load/skip loading of samples.
//' @param LogicalVector sample_attr_flag, boolean vector that indicates which sample fields are to be stored
//' @param std::string start_marker_string, event that marks trial start. Defaults to "TRIALID", if empty.
//' @param std::string end_marker_string, event that marks trial end
//' @param verbose, whether to show progressbar and report number of trials
//' @export
//' @keywords internal
//' @return List, contents of the EDF file. Please see read_edf for details.
//[[Rcpp::export]]
List read_edf_file(std::string filename,
                   int consistency,
                   bool import_events,
                   bool import_recordings,
                   bool import_samples,
                   LogicalVector sample_attr_flag,
                   std::string start_marker_string,
                   std::string end_marker_string,
                   bool verbose){
  return(List::create());
}
