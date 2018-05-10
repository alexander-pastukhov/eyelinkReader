#include <sstream>
#include <math.h>

#include <Rcpp.h>
using namespace Rcpp;

#include "SRResearch/edf.h"

#include "edf_structures.h"

//' @title Opens EDF file, throws exception on error
//' @description Opens EDF file for reading, throws exception and prints error message if fails.
//' @param std::string filename, name of the EDF file
//' @param int consistency, consistency check control (for the time stamps of the start
//' and end events, etc). 0, no consistency check. 1, check consistency and report.
//' 2, check consistency and fix.
//' @param int loadevents, load/skip loading events 0, do not load events. 1, load events.
//' @param int loadsamples, load/skip loading of samples 0, do not load samples. 1, load samples.
//' @return pointer to the EDF file
//' @keywords internal
EDFFILE* safely_open_edf_file(std::string filename, int consistency, int loadevents, int loadsamples){
  // opening the edf file
  int ReturnValue;
  EDFFILE* edfFile= edf_open_file(filename.c_str(), consistency, loadevents, loadsamples, &ReturnValue);

  // throwing an exception, if things go pear shaped
  if (ReturnValue != 0){
    std::stringstream error_message_stream;
    error_message_stream << "Error opening file '" << filename << "', error code: " << ReturnValue;
    ::Rf_error(error_message_stream.str().c_str());
  }

  return edfFile;
}

//' @title Reads preamble of the EDF file.
//' @description Reads preamble of the EDF file.
//' @return string with the preamble
//' @export
//' @examples
//' read_preamble(system.file("extdata", "example.edf", package = "edfR"))
//[[Rcpp::export]]
std::string read_preamble(std::string filename){
  EDFFILE* edfFile= safely_open_edf_file(filename, 2, 0, 0);

  // getting preable
  int ReturnValue;
  char preamble_buffer[2048];
  ReturnValue= edf_get_preamble_text(edfFile, preamble_buffer, 2048);
  if (ReturnValue!=0)
  {
    std::stringstream error_message_stream;
    error_message_stream << "Error reading preable for file '" << filename << "', error code: " << ReturnValue;
    ::Rf_error(error_message_stream.str().c_str());
  }
  std::string preamble(preamble_buffer);

  // closing file
  edf_close_file(edfFile);

  return preamble;
}


//' @title Sets trial navigation for EDF API
//' @description Sets trial navigation via the markers for the start and the end of the trial.
//' @param EDFFILE* edfFile, pointer to the EDF file
//' @param std::string start_marker_string, event that marks trial start. Defaults to "TRIALID", if empty.
//' @param std::string end_marker_string, event that marks trial end
//' @seealso safely_open_edf_file
//' @keywords internal
void set_trial_navigation_up(EDFFILE* edfFile, std::string start_marker_string, std::string end_marker_string){
  // converting strings to char buffers
  char * start_marker_char = new char[start_marker_string.size() + 1];
  std::copy(start_marker_string.begin(), start_marker_string.end(), start_marker_char);
  start_marker_char[start_marker_string.size()] = '\0';

  char * end_marker_char = new char[end_marker_string.size() + 1];
  std::copy(end_marker_string.begin(), end_marker_string.end(), end_marker_char);
  end_marker_char[end_marker_string.size()] = '\0';

  // setting the trial identifier
  if (edf_set_trial_identifier(edfFile, start_marker_char, end_marker_char)){
    ::Rf_error("Error while setting up trial navigation identifier");
  }

  // cleaning up
  delete[] start_marker_char;
  delete[] end_marker_char;
}

//' @title Jumps to the i-th trial
//' @description Jumps to the i-th trial, throws an exception and prints an error message, if fails.
//' @param EDFFILE* edfFile, pointer to the EDF file
//' @param int iTrial, index of the desired trial
//' @seealso safely_open_edf_file, set_trial_navigation_up
//' @keywords internal
void jump_to_trial(EDFFILE* edfFile, int iTrial){
  if (edf_jump_to_trial(edfFile, iTrial) != 0){
    std::stringstream error_message_stream;
    error_message_stream << "Error jumping to trial " << iTrial+1;
    ::Rf_error(error_message_stream.str().c_str());
  }
}

//' @title Prepare matrix for trial headers
//' @description Prepare matrix for trial headers.
//' @param int total_trials, total number of trials, i.e. number of rows in the matrix
//' @return NumericMatrix total_trials (rows) x 15 (columns)
//' @keywords internal
NumericMatrix prepare_trial_headers(int total_trials){
  // row names
  NumericVector row_index(total_trials);
  for(unsigned int iTrial= 0; iTrial< total_trials; iTrial++)
  {
    row_index[iTrial]= iTrial+1;
  };

  // column names
  CharacterVector col_names= CharacterVector::create("trial", "duration", "starttime", "endtime",
                                                     "rec.time", "rec.sample_rate", "rec.eflags",
                                                     "rec.sflags", "rec.state", "rec.record_type",
                                                     "rec.pupil_type", "rec.recording_mode", "rec.filter_type",
                                                     "rec.pos_type", "rec.eye");

  // create the matrix
  NumericMatrix trial_headers= NumericMatrix(total_trials, col_names.size());
  trial_headers.attr("dimnames")= List::create(row_index, col_names);
  return (trial_headers);
}
