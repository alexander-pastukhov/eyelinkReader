#include <sstream>
#include <math.h>

#include <Rcpp.h>
using namespace Rcpp;

#include "SRResearch/edf.h"

#include "edf_structures.h"

//' Opens EDF file, throws exception on error
//'
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

//' Reads preamble of the EDF file.
//'
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


//' Jumps to the i-th trial
//'
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

//' Prepare matrix for trial headers
//'
//' @title Prepare matrix for trial headers
//' @description Prepare matrix for trial headers.
//' @param int total_trials, total number of trials, i.e. number of rows in the matrix
//' @return NumericMatrix total_trials (rows) x 15 (columns)
//' @keywords internal
NumericMatrix prepare_trial_headers(int total_trials){
  // row names
  NumericVector row_index(total_trials);
  for(int iTrial= 0; iTrial< total_trials; iTrial++)
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

//' @title Read header for the i-th trial
//' @description Read head and store it in the i-th row of the headers matrix
//' @param EDFFILE* edfFile, pointer to the EDF file
//' @param NumericMatrix &trial_headers, reference to the trial header matrix
//' @param int iTrial, the row in which the header will be stored.
//' Functions assumes that the correct trial within the EDF file was already navigated to.
//' @return modifes trial_headers i-th row in place
//' @keywords internal
void read_trial_header(EDFFILE* edfFile, NumericMatrix &trial_headers, int iTrial){

  // obtaining the trial header
  TRIAL current_header;
  if (edf_get_trial_header(edfFile, &current_header) != 0){
    std::stringstream error_message_stream;
    error_message_stream << "Error obtaining the header for the trial " << iTrial+1;
    ::Rf_error(error_message_stream.str().c_str());
  }

  // copying it over
  trial_headers(iTrial, 0)= iTrial+1;
  trial_headers(iTrial, 1)= current_header.duration;
  trial_headers(iTrial, 2)= current_header.starttime;
  trial_headers(iTrial, 3)= current_header.endtime;
  trial_headers(iTrial, 4)= current_header.rec->time;
  trial_headers(iTrial, 5)= current_header.rec->sample_rate ;
  trial_headers(iTrial, 6)= current_header.rec->eflags;
  trial_headers(iTrial, 7)= current_header.rec->sflags;
  trial_headers(iTrial, 8)= current_header.rec->state;
  trial_headers(iTrial, 9)= current_header.rec->record_type;
  trial_headers(iTrial,10)= current_header.rec->pupil_type;
  trial_headers(iTrial,11)= current_header.rec->recording_mode;
  trial_headers(iTrial,12)= current_header.rec->filter_type;
  trial_headers(iTrial,13)= current_header.rec->pos_type;
  trial_headers(iTrial,14)= current_header.rec->eye;
}

//' @title Appends event to the even structure
//' @description Appends a new event to the even structure and copies all the data
//' @param TRIAL_EVENTS &events, reference to the trial events structure
//' @param FEVENT new_event, structure with event info, as described in the EDF API manual
//' @param int iTrial, the index of the trial the event belongs to
//' @param UINT32 trial_start, the timestamp of the trial start.
//' Is used to compute event time relative to it.
//' @return modifies events structure
//' @keywords internal
void append_event(TRIAL_EVENTS &events, FEVENT new_event, unsigned int iTrial, UINT32 trial_start){
  events.trial_index.push_back(iTrial+1);
  events.time.push_back(new_event.time);
  events.type.push_back(new_event.type);
  events.read.push_back(new_event.read);
  events.sttime.push_back(new_event.sttime);
  events.sttime_rel.push_back(new_event.sttime-trial_start);
  events.entime.push_back(new_event.entime);
  if (new_event.entime>0){
    events.entime_rel.push_back(new_event.entime-trial_start);
  }
  else
  {
    events.entime_rel.push_back(new_event.entime);
  }
  events.hstx.push_back(new_event.hstx);
  events.hsty.push_back(new_event.hsty);
  events.gstx.push_back(new_event.gstx);
  events.gsty.push_back(new_event.gsty);
  events.sta.push_back(new_event.sta);
  events.henx.push_back(new_event.henx);
  events.heny.push_back(new_event.heny);
  events.genx.push_back(new_event.genx);
  events.geny.push_back(new_event.geny);
  events.ena.push_back(new_event.ena);
  events.havx.push_back(new_event.havx);
  events.havy.push_back(new_event.havy);
  events.gavx.push_back(new_event.gavx);
  events.gavy.push_back(new_event.gavy);
  events.ava.push_back(new_event.ava);
  events.avel.push_back(new_event.avel);
  events.pvel.push_back(new_event.pvel);
  events.svel.push_back(new_event.svel);
  events.evel.push_back(new_event.evel);
  events.supd_x.push_back(new_event.supd_x);
  events.eupd_x.push_back(new_event.eupd_x);
  events.supd_y.push_back(new_event.supd_y);
  events.eupd_y.push_back(new_event.eupd_y);
  events.eye.push_back(new_event.eye);
  events.status.push_back(new_event.status);
  events.flags.push_back(new_event.flags);
  events.input.push_back(new_event.input);
  events.buttons.push_back(new_event.buttons);
  events.parsedby.push_back(new_event.parsedby);

  // special case: LSTRING message
  LSTRING* message_ptr= ((LSTRING*)new_event.message);
  if (message_ptr==0 || message_ptr==NULL){
    events.message.push_back("");
  }
  else{
    char* message_char= new char[message_ptr->len];
    strncpy(message_char, &(message_ptr->c), message_ptr->len);
    events.message.push_back(message_char);
    delete[] message_char;
  }
}

