#include <sstream>
#include <math.h>

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>

namespace edfapi {
#include "edf.h"
}

const float FLOAT_NAN = nanf("");


//' @title Status of compiled library
//' @description Return status of compiled library
//' @return logical
//' @export
//' @examples
//' compiled_library_status()
//[[Rcpp::export]]
bool compiled_library_status(){
  // the other version, compiled at installation returns false
  return(true);
}


// ------------------ data structures, as defined in EDF C API user manual ------------------
typedef struct TRIAL_EVENTS {
  std::vector <unsigned int> trial_index;
  std::vector <edfapi::UINT32> time;
  std::vector <edfapi::INT16> type;
  std::vector <edfapi::UINT16> read;
  std::vector <edfapi::UINT32> sttime;
  std::vector <edfapi::UINT32> sttime_rel;
  std::vector <edfapi::UINT32> entime;
  std::vector <edfapi::UINT32> entime_rel;
  std::vector <float> hstx;
  std::vector <float> hsty;
  std::vector <float> gstx;
  std::vector <float> gsty;
  std::vector <float> sta;
  std::vector <float> henx;
  std::vector <float> heny;
  std::vector <float> genx;
  std::vector <float> geny;
  std::vector <float> ena;
  std::vector <float> havx;
  std::vector <float> havy;
  std::vector <float> gavx;
  std::vector <float> gavy;
  std::vector <float> ava;
  std::vector <float> avel;
  std::vector <float> pvel;
  std::vector <float> svel;
  std::vector <float> evel;
  std::vector <float> supd_x;
  std::vector <float> eupd_x;
  std::vector <float> supd_y;
  std::vector <float> eupd_y;
  std::vector <edfapi::INT16> eye;
  std::vector <edfapi::UINT16> status;
  std::vector <edfapi::UINT16> flags;
  std::vector <edfapi::UINT16> input;
  std::vector <edfapi::UINT16> buttons;
  std::vector <edfapi::UINT16> parsedby;
  std::vector <std::string> message;
} TRIAL_EVENTS;


// please note that byte type were replaced with UINT16 for compatibility reasons
typedef struct TRIAL_RECORDINGS{
  std::vector <unsigned int> trial_index;
  std::vector <edfapi::UINT32> time;
  std::vector <edfapi::UINT32> time_rel;
  std::vector <float> sample_rate;
  std::vector <edfapi::UINT16> eflags;
  std::vector <edfapi::UINT16> sflags;
  std::vector <edfapi::UINT16> state;
  std::vector <edfapi::UINT16> record_type;
  std::vector <edfapi::UINT16> pupil_type;
  std::vector <edfapi::UINT16> recording_mode;
  std::vector <edfapi::UINT16> filter_type;
  std::vector <edfapi::UINT16> pos_type;
  std::vector <edfapi::UINT16> eye;
} TRIAL_RECORDINGS;

typedef struct TRAIL_SAMPLES{
  std::vector <unsigned int> trial_index;
  std::vector <edfapi::UINT32> time;
  std::vector <edfapi::UINT32> time_rel;
  std::vector <unsigned int> eye;
  // NumericVector pxL;
  // NumericVector pxR;
  std::vector <float> pxL;
  std::vector <float> pxR;
  std::vector <float> pyL;
  std::vector <float> pyR;
  std::vector <float> hxL;
  std::vector <float> hxR;
  std::vector <float> hyL;
  std::vector <float> hyR;
  std::vector <float> paL;
  std::vector <float> paR;
  std::vector <float> gxL;
  std::vector <float> gxR;
  std::vector <float> gyL;
  std::vector <float> gyR;
  std::vector <float> rx;
  std::vector <float> ry;
  std::vector <float> gxvelL;
  std::vector <float> gxvelR;
  std::vector <float> gyvelL;
  std::vector <float> gyvelR;
  std::vector <float> hxvelL;
  std::vector <float> hxvelR;
  std::vector <float> hyvelL;
  std::vector <float> hyvelR;
  std::vector <float> rxvelL;
  std::vector <float> rxvelR;
  std::vector <float> ryvelL;
  std::vector <float> ryvelR;
  std::vector <float> fgxvelL;
  std::vector <float> fgxvelR;
  std::vector <float> fgyvelL;
  std::vector <float> fgyvelR;
  std::vector <float> fhxvelL;
  std::vector <float> fhxvelR;
  std::vector <float> fhyvelL;
  std::vector <float> fhyvelR;
  std::vector <float> frxvelL;
  std::vector <float> frxvelR;
  std::vector <float> fryvelL;
  std::vector <float> fryvelR;

  std::vector <edfapi::INT16> hdata_1;
  std::vector <edfapi::INT16> hdata_2;
  std::vector <edfapi::INT16> hdata_3;
  std::vector <edfapi::INT16> hdata_4;
  std::vector <edfapi::INT16> hdata_5;
  std::vector <edfapi::INT16> hdata_6;
  std::vector <edfapi::INT16> hdata_7;
  std::vector <edfapi::INT16> hdata_8;

  std::vector <edfapi::UINT16> flags;
  std::vector <edfapi::UINT16> input;
  std::vector <edfapi::UINT16> buttons;
  std::vector <edfapi::INT16> htype;
  std::vector <edfapi::UINT16> errors;
} TRIAL_SAMPLES;


//' @title Converts a float value to an explicit NaN, if necessary
//' @param value float
//' @return float
//' @export
//' @keywords internal
inline float float_or_nan(float value) {
  if ((value <= MISSING_DATA) || (value >= 1e8)) return FLOAT_NAN;
  return value;
}

// ------------------ EDF API interface ------------------

//' @title Version of the EDF API library
//' @description Returns version of the EDF API library used to interface an EDF file.
//' @export
//' @examples
//' eyelinkReader::library_version()
//[[Rcpp::export]]
CharacterVector library_version(){
  Rcpp::StringVector version_info(1);
  version_info[0] = edfapi::edf_get_version();
  return version_info;
}


// @title Opens EDF file, throws exception on error
// @description Opens EDF file for reading, throws exception and prints error message if fails.
// @param std::string filename, name of the EDF file
// @param int consistency, consistency check control (for the time stamps of the start
// and end events, etc). 0, no consistency check. 1, check consistency and report.
// 2, check consistency and fix.
// @param int loadevents, load/skip loading events 0, do not load events. 1, load events.
// @param int loadsamples, load/skip loading of samples 0, do not load samples. 1, load samples.
// @return pointer to the EDF file
// @keywords internal
edfapi::EDFFILE* safely_open_edf_file(std::string filename, int consistency, int loadevents, int loadsamples){
  // opening the edf file
  int ReturnValue;
  edfapi::EDFFILE* edfFile = edfapi::edf_open_file(filename.c_str(), consistency, loadevents, loadsamples, &ReturnValue);

  // throwing an exception, if things go pear shaped
  if (ReturnValue != 0){
    std::stringstream error_message_stream;
    error_message_stream << "Error opening file '" << filename << "', error code: " << ReturnValue;
    ::Rf_error("%s", error_message_stream.str().c_str());
  }

  return edfFile;
}

//' @title Reads preamble of the EDF file as a single string.
//' @description Reads preamble of the EDF file as a single string.
//' Please, do not use this function directly. Instead, call \code{\link{read_preamble}} function
//' that provides a more consistent interface.
//' @return string with the preamble
//' @export
//' @keywords internal
//' @examples
//' \donttest{
//' read_preamble(system.file("extdata", "example.edf", package = "eyelinkReader"))
//' }
//[[Rcpp::export]]
std::string read_preamble_str(std::string filename){
  edfapi::EDFFILE* edfFile = safely_open_edf_file(filename, 2, 0, 0);

  // getting preable
  int ReturnValue;
  char preamble_buffer[2048];
  ReturnValue = edfapi::edf_get_preamble_text(edfFile, preamble_buffer, 2048);
  if (ReturnValue != 0)
  {
    std::stringstream error_message_stream;
    error_message_stream << "Error reading preable for file '" << filename << "', error code: " << ReturnValue;
    ::Rf_error("%s", error_message_stream.str().c_str());
  }
  std::string preamble(preamble_buffer);

  // closing file
  edfapi::edf_close_file(edfFile);

  return preamble;
}


//' @title Sets trial navigation for EDF API
//' @description Sets trial navigation via the markers for the start and the end of the trial.
//' @param EDFFILE* edfFile, pointer to the EDF file
//' @param std::string start_marker_string, event that marks trial start. Defaults to "TRIALID", if empty.
//' @param std::string end_marker_string, event that marks trial end
//' @seealso safely_open_edf_file
//' @keywords internal
void set_trial_navigation_up(edfapi::EDFFILE* edfFile, std::string start_marker_string, std::string end_marker_string){
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
void jump_to_trial(edfapi::EDFFILE* edfFile, int iTrial){
  if (edfapi::edf_jump_to_trial(edfFile, iTrial) != 0){
    std::stringstream error_message_stream;
    error_message_stream << "Error jumping to trial " << iTrial+1;
    ::Rf_error("%s", error_message_stream.str().c_str());
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
  for(int iTrial= 0; iTrial< total_trials; iTrial++)
  {
    row_index[iTrial] = iTrial+1;
  };

  // column names
  CharacterVector col_names= CharacterVector::create("trial", "duration", "starttime", "endtime",
                                                     "rec_time", "rec_sample_rate", "rec_eflags",
                                                     "rec_sflags", "rec_state", "rec_record_type",
                                                     "rec_pupil_type", "rec_recording_mode", "rec_filter_type",
                                                     "rec_pos_type", "rec_eye");

  // create the matrix
  NumericMatrix trial_headers= NumericMatrix(total_trials, col_names.size());
  trial_headers.attr("dimnames") = List::create(row_index, col_names);
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
void read_trial_header(edfapi::EDFFILE* edfFile, NumericMatrix &trial_headers, int iTrial){

  // obtaining the trial header
  edfapi::TRIAL current_header;
  if (edf_get_trial_header(edfFile, &current_header) != 0){
    std::stringstream error_message_stream;
    error_message_stream << "Error obtaining the header for the trial " << iTrial+1;
    ::Rf_error("%s", error_message_stream.str().c_str());
  }

  // copying it over
  trial_headers(iTrial, 0) = iTrial+1;
  trial_headers(iTrial, 1) = current_header.duration;
  trial_headers(iTrial, 2) = current_header.starttime;
  trial_headers(iTrial, 3) = current_header.endtime;
  trial_headers(iTrial, 4) = current_header.rec->time;
  trial_headers(iTrial, 5) = current_header.rec->sample_rate ;
  trial_headers(iTrial, 6) = current_header.rec->eflags;
  trial_headers(iTrial, 7) = current_header.rec->sflags;
  trial_headers(iTrial, 8) = current_header.rec->state;
  trial_headers(iTrial, 9) = current_header.rec->record_type;
  trial_headers(iTrial,10) = current_header.rec->pupil_type;
  trial_headers(iTrial,11) = current_header.rec->recording_mode;
  trial_headers(iTrial,12) = current_header.rec->filter_type;
  trial_headers(iTrial,13) = current_header.rec->pos_type;
  trial_headers(iTrial,14) = current_header.rec->eye;
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
void append_event(TRIAL_EVENTS &events, edfapi::FEVENT new_event, unsigned int iTrial, edfapi::UINT32 trial_start){
  events.trial_index.push_back(iTrial);
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
  edfapi::LSTRING* message_ptr = ((edfapi::LSTRING*)new_event.message);
  if (message_ptr == 0 || message_ptr == NULL){
    events.message.push_back("");
  }
  else{
    char* message_char = new char[message_ptr->len];
    strncpy(message_char, &(message_ptr->c), message_ptr->len);
    events.message.push_back(message_char);
    delete[] message_char;
  }
}

//' @title Appends recording to the recording structure
//' @description Appends a new recording to the recordings structure and copies all the data
//' @param TRIAL_RECORDINGS &recordings, reference to the trial recording structure
//' @param RECORDINGS new_rec, structure with recordiong info, as described in the EDF API manual
//' @param int iTrial, the index of the trial the event belongs to
//' @param UINT32 trial_start, the timestamp of the trial start.
//' Is used to compute event time relative to it.
//' @return modifies recordings structure
//' @keywords internal
void append_recording(TRIAL_RECORDINGS &recordings, edfapi::RECORDINGS new_rec, unsigned int iTrial, edfapi::UINT32 trial_start){
  recordings.trial_index.push_back(iTrial+1);
  recordings.time.push_back(new_rec.time);
  recordings.time_rel.push_back(new_rec.time-trial_start);
  recordings.sample_rate.push_back(new_rec.sample_rate);
  recordings.eflags.push_back(new_rec.eflags);
  recordings.sflags.push_back(new_rec.sflags);
  recordings.state.push_back(new_rec.state);
  recordings.record_type.push_back(new_rec.record_type);
  recordings.pupil_type.push_back(new_rec.pupil_type);
  recordings.recording_mode.push_back(new_rec.recording_mode);
  recordings.filter_type.push_back(new_rec.filter_type);
  recordings.pos_type.push_back(new_rec.pos_type);
  recordings.eye.push_back(new_rec.eye);
}


//' @title Appends sample to the samples structure
//' @description Appends a new sample to the samples structure and copies all the data
//' @param TRIAL_SAMPLES &samples, reference to the trial samples structure
//' @param FSAMPLE new_sample, structure with sample info, as described in the EDF API manual
//' @param int iTrial, the index of the trial the event belongs to
//' @param UINT32 trial_start, the timestamp of the trial start. Is used to compute event time relative to it.
//' @param LogicalVector sample_attr_flag, boolean vector that indicates which sample fields are to be stored
//' @return modifies samples structure
//' @keywords internal
void append_sample(TRIAL_SAMPLES &samples, edfapi::FSAMPLE new_sample, unsigned int iTrial, edfapi::UINT32 trial_start, LogicalVector sample_attr_flag)
{
  samples.trial_index.push_back(iTrial+1);

  if (new_sample.flags & SAMPLE_LEFT) {
    if (new_sample.flags & SAMPLE_RIGHT) {
      samples.eye.push_back(2);
    }
    else {
      samples.eye.push_back(0);
    }
  }
  else {
    samples.eye.push_back(1);
  }

  if (sample_attr_flag[0]){
    samples.time.push_back(new_sample.time);
    samples.time_rel.push_back(new_sample.time - trial_start);
  }
  if (sample_attr_flag[1]){
    samples.pxL.push_back(float_or_nan(new_sample.px[0]));
    samples.pxR.push_back(float_or_nan(new_sample.px[1]));
  }
  if (sample_attr_flag[2]){
    samples.pyL.push_back(float_or_nan(new_sample.py[0]));
    samples.pyR.push_back(float_or_nan(new_sample.py[1]));
  }
  if (sample_attr_flag[3]){
    samples.hxL.push_back(float_or_nan(new_sample.hx[0]));
    samples.hxR.push_back(float_or_nan(new_sample.hx[1]));
  }
  if (sample_attr_flag[4]){
    samples.hyL.push_back(float_or_nan(new_sample.hy[0]));
    samples.hyR.push_back(float_or_nan(new_sample.hy[1]));
  }
  if (sample_attr_flag[5]){
    samples.paL.push_back(float_or_nan(new_sample.pa[0]));
    samples.paR.push_back(float_or_nan(new_sample.pa[1]));
  }
  if (sample_attr_flag[6]){
    samples.gxL.push_back(float_or_nan(new_sample.gx[0]));
    samples.gxR.push_back(float_or_nan(new_sample.gx[1]));
  }
  if (sample_attr_flag[7]){
    samples.gyL.push_back(float_or_nan(new_sample.gy[0]));
    samples.gyR.push_back(float_or_nan(new_sample.gy[1]));
  }
  if (sample_attr_flag[8]){
    samples.rx.push_back(float_or_nan(new_sample.rx));
  }
  if (sample_attr_flag[9]){
    samples.ry.push_back(float_or_nan(new_sample.ry));
  }
  if (sample_attr_flag[10]){
    samples.gxvelL.push_back(float_or_nan(new_sample.gxvel[0]));
    samples.gxvelR.push_back(float_or_nan(new_sample.gxvel[1]));
  }
  if (sample_attr_flag[11]){
    samples.gyvelL.push_back(float_or_nan(new_sample.gyvel[0]));
    samples.gyvelR.push_back(float_or_nan(new_sample.gyvel[1]));
  }
  if (sample_attr_flag[12]){
    samples.hxvelL.push_back(float_or_nan(new_sample.hxvel[0]));
    samples.hxvelR.push_back(float_or_nan(new_sample.hxvel[1]));
  }
  if (sample_attr_flag[13]){
    samples.hyvelL.push_back(float_or_nan(new_sample.hyvel[0]));
    samples.hyvelR.push_back(float_or_nan(new_sample.hyvel[1]));
  }
  if (sample_attr_flag[14]){
    samples.rxvelL.push_back(float_or_nan(new_sample.rxvel[0]));
    samples.rxvelR.push_back(float_or_nan(new_sample.rxvel[1]));
  }
  if (sample_attr_flag[15]){
    samples.ryvelL.push_back(float_or_nan(new_sample.ryvel[0]));
    samples.ryvelR.push_back(float_or_nan(new_sample.ryvel[1]));
  }
  if (sample_attr_flag[16]){
    samples.fgxvelL.push_back(float_or_nan(new_sample.fgxvel[0]));
    samples.fgxvelR.push_back(float_or_nan(new_sample.fgxvel[1]));
  }
  if (sample_attr_flag[17]){
    samples.fgyvelL.push_back(float_or_nan(new_sample.fgyvel[0]));
    samples.fgyvelR.push_back(float_or_nan(new_sample.fgyvel[1]));
  }
  if (sample_attr_flag[18]){
    samples.fhxvelL.push_back(float_or_nan(new_sample.fhxvel[0]));
    samples.fhxvelR.push_back(float_or_nan(new_sample.fhxvel[1]));
  }
  if (sample_attr_flag[19]){
    samples.fhyvelL.push_back(float_or_nan(new_sample.fhyvel[0]));
    samples.fhyvelR.push_back(float_or_nan(new_sample.fhyvel[1]));
  }
  if (sample_attr_flag[20]){
    samples.frxvelL.push_back(float_or_nan(new_sample.frxvel[0]));
    samples.frxvelR.push_back(float_or_nan(new_sample.frxvel[1]));
  }
  if (sample_attr_flag[21]){
    samples.fryvelL.push_back(float_or_nan(new_sample.fryvel[0]));
    samples.fryvelR.push_back(float_or_nan(new_sample.fryvel[1]));
  }
  if (sample_attr_flag[22]){
    samples.hdata_1.push_back(new_sample.hdata[0]);
    samples.hdata_2.push_back(new_sample.hdata[1]);
    samples.hdata_3.push_back(new_sample.hdata[2]);
    samples.hdata_4.push_back(new_sample.hdata[3]);
    samples.hdata_5.push_back(new_sample.hdata[4]);
    samples.hdata_6.push_back(new_sample.hdata[5]);
    samples.hdata_7.push_back(new_sample.hdata[6]);
    samples.hdata_8.push_back(new_sample.hdata[7]);
  }
  if (sample_attr_flag[23]){
    samples.flags.push_back(new_sample.flags);
  }
  if (sample_attr_flag[24]){
    samples.input.push_back(new_sample.input);
  }
  if (sample_attr_flag[25]){
    samples.buttons.push_back(new_sample.buttons);
  }
  if (sample_attr_flag[26]){
    samples.htype.push_back(new_sample.htype);
  }
  if (sample_attr_flag[27]){
    samples.errors.push_back(new_sample.errors);
  }
}


// Internal function that reads EDF file
//
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
  // data storage
  TRIAL_EVENTS all_events;
  TRIAL_SAMPLES all_samples;
  TRIAL_RECORDINGS all_recordings;

  // collecting all message before the first recording
  // should contain service information, such as DISPLAY_COORDS
  edfapi::EDFFILE* edfFile = safely_open_edf_file(filename, consistency, 1, 0);
  for(bool keep_looking = true; keep_looking; ){
    int DataType = edfapi::edf_get_next_data(edfFile);
    edfapi::ALLF_DATA* current_data = edfapi::edf_get_float_data(edfFile);
    switch(DataType){
    case MESSAGEEVENT:
      append_event(all_events, current_data->fe, 0, 0);
      break;
    case RECORDING_INFO:
      // the recording has started, done with preliminaries
      keep_looking = false;
      break;
    }
  }
  edfapi::edf_close_file(edfFile);


  // opening the edf file to load info trial by trial
  edfFile = safely_open_edf_file(filename, consistency, import_events, import_samples);

  // set the trial navigation up
  set_trial_navigation_up(edfFile, start_marker_string, end_marker_string);

  // figure out, just how many trials we have
  unsigned int total_trials = edfapi::edf_get_trial_count(edfFile);
  if (verbose){
    ::Rprintf("Trials count: %d\n", total_trials);
  }

  // creating headers, events, and samples
  NumericMatrix trial_headers = prepare_trial_headers(total_trials);


  // looping over the trials
  Progress trial_counter(total_trials, verbose);
  for(unsigned int iTrial = 0; iTrial< total_trials; iTrial++){
    // visuals and interaction
    if (verbose){
      if (Progress::check_abort() ){
        break;
      }
      trial_counter.increment();
    }

    jump_to_trial(edfFile, iTrial);

    // read headers
    read_trial_header(edfFile, trial_headers, iTrial);

    // read trial
    edfapi::ALLF_DATA* current_data;
    edfapi::UINT32 trial_start_time = trial_headers(iTrial, 2);
    edfapi::UINT32 trial_end_time = trial_headers(iTrial, 3);
    if (trial_end_time <= trial_start_time){
      ::warning("Skipping trial %d due to zero or negative duration.", iTrial+1);
      continue;
    }

    bool TrialIsOver = false;
    edfapi::UINT32 data_timestamp = 0;
    for(int DataType = edfapi::edf_get_next_data(edfFile);
        (DataType != NO_PENDING_ITEMS) && !TrialIsOver;
        DataType = edfapi::edf_get_next_data(edfFile)){

      // obtaining next data piece
      current_data = edfapi::edf_get_float_data(edfFile);
      switch(DataType){
      case SAMPLE_TYPE:
        data_timestamp = current_data->fs.time;
        if (import_samples){
          append_sample(all_samples, current_data->fs, iTrial, trial_start_time, sample_attr_flag);
        }
        break;

      case STARTPARSE:
      case ENDPARSE:
      case BREAKPARSE:
      case STARTBLINK:
      case ENDBLINK:
      case STARTSACC:
      case ENDSACC:
      case STARTFIX:
      case ENDFIX:
      case FIXUPDATE:
      case MESSAGEEVENT:
      case STARTSAMPLES:
      case ENDSAMPLES:
      case STARTEVENTS:
      case ENDEVENTS:
      case BUTTONEVENT:
      case INPUTEVENT:
      case LOST_DATA_EVENT:
        data_timestamp = current_data->fe.sttime;
        if (data_timestamp > trial_end_time)
        {
          TrialIsOver = true;
          break;
        }
        if (import_events){
          append_event(all_events, current_data->fe, iTrial + 1, trial_start_time);
        }
        break;

      case RECORDING_INFO:
        data_timestamp = current_data->fe.time;
        if (import_recordings){
          append_recording(all_recordings, current_data->rec, iTrial, trial_start_time);
        }
        break;
      case NO_PENDING_ITEMS:
        break;
      }

      // end of trial check
      if (data_timestamp > trial_end_time)
        break;
    }
  }

  // closing file
  edfapi::edf_close_file(edfFile);

  // returning data
  List edf_recording;
  edf_recording["headers"] = trial_headers;

  // converting structure of vectors into a data frame
  if (import_events){
    DataFrame events;
    events["trial"] = all_events.trial_index;
    events["time"] = all_events.time;
    events["type"] = all_events.type;
    events["read"] = all_events.read;
    events["sttime"] = all_events.sttime;
    events["entime"] = all_events.entime;
    events["sttime_rel"] = all_events.sttime_rel;
    events["entime_rel"] = all_events.entime_rel;
    events["hstx"] = all_events.hstx;
    events["hsty"] = all_events.hsty;
    events["gstx"] = all_events.gstx;
    events["gsty"] = all_events.gsty;
    events["sta"] = all_events.sta;
    events["henx"] = all_events.henx;
    events["heny"] = all_events.heny;
    events["genx"] = all_events.genx;
    events["geny"] = all_events.geny;
    events["ena"] = all_events.ena;
    events["havx"] = all_events.havx;
    events["havy"] = all_events.havy;
    events["gavx"] = all_events.gavx;
    events["gavy"] = all_events.gavy;
    events["ava"] = all_events.ava;
    events["avel"] = all_events.avel;
    events["pvel"] = all_events.pvel;
    events["svel"] = all_events.svel;
    events["evel"] = all_events.evel;
    events["supd_x"] = all_events.supd_x;
    events["eupd_x"] = all_events.eupd_x;
    events["supd_y"] = all_events.supd_y;
    events["eupd_y"] = all_events.eupd_y;
    events["eye"] = all_events.eye;
    events["status"] = all_events.status;
    events["flags"] = all_events.flags;
    events["input"] = all_events.input;
    events["buttons"] = all_events.buttons;
    events["parsedby"] = all_events.parsedby;
    events["message"] = all_events.message;
    edf_recording["events"] = events;
  }

  if (import_recordings){
    DataFrame recordings;
    recordings["trial_index"] = all_recordings.trial_index;
    recordings["time"] = all_recordings.time;
    recordings["time_rel"] = all_recordings.time_rel;
    recordings["sample_rate"] = all_recordings.sample_rate;
    recordings["eflags"] = all_recordings.eflags;
    recordings["sflags"] = all_recordings.sflags;
    recordings["state"] = all_recordings.state;
    recordings["record_type"] = all_recordings.record_type;
    recordings["pupil_type"] = all_recordings.pupil_type;
    recordings["recording_mode"] = all_recordings.recording_mode;
    recordings["filter_type"] = all_recordings.filter_type;
    recordings["pos_type"] = all_recordings.pos_type;
    recordings["eye"] = all_recordings.eye;
    edf_recording["recordings"] = recordings;
  }

  if (import_samples){
    DataFrame samples;
    samples["trial"] = all_samples.trial_index;
    samples["eye"] = all_samples.eye;
    if (sample_attr_flag[0]){
      samples["time"] = all_samples.time;
      samples["time_rel"] = all_samples.time_rel;
    }
    if (sample_attr_flag[1]){
      samples["pxL"] = all_samples.pxL;
      samples["pxR"] = all_samples.pxR;
    }
    if (sample_attr_flag[2]){
      samples["pyL"] = all_samples.pyL;
      samples["pyR"] = all_samples.pyR;
    }
    if (sample_attr_flag[3]){
      samples["hxL"] = all_samples.hxL;
      samples["hxR"] = all_samples.hxR;
    }
    if (sample_attr_flag[4]){
      samples["hyL"] = all_samples.hyL;
      samples["hyR"] = all_samples.hyR;
    }
    if (sample_attr_flag[5]){
      samples["paL"] = all_samples.paL;
      samples["paR"] = all_samples.paR;
    }
    if (sample_attr_flag[6]){
      samples["gxL"] = all_samples.gxL;
      samples["gxR"] = all_samples.gxR;
    }
    if (sample_attr_flag[7]){
      samples["gyL"] = all_samples.gyL;
      samples["gyR"] = all_samples.gyR;
    }
    if (sample_attr_flag[8]){
      samples["rx"] = all_samples.rx;
    }
    if (sample_attr_flag[9]){
      samples["ry"] = all_samples.ry;
    }
    if (sample_attr_flag[10]){
      samples["gxvelL"] = all_samples.gxvelL;
      samples["gxvelR"] = all_samples.gxvelR;
    }
    if (sample_attr_flag[11]){
      samples["gyvelL"] = all_samples.gyvelL;
      samples["gyvelR"] = all_samples.gyvelR;
    }
    if (sample_attr_flag[12]){
      samples["hxvelL"] = all_samples.hxvelL;
      samples["hxvelR"] = all_samples.hxvelR;
    }
    if (sample_attr_flag[13]){
      samples["hyvelL"] = all_samples.hyvelL;
      samples["hyvelR"] = all_samples.hyvelR;
    }
    if (sample_attr_flag[14]){
      samples["rxvelL"] = all_samples.rxvelL;
      samples["rxvelR"] = all_samples.rxvelR;
    }
    if (sample_attr_flag[15]){
      samples["ryvelL"] = all_samples.ryvelL;
      samples["ryvelR"] = all_samples.ryvelR;
    }
    if (sample_attr_flag[16]){
      samples["fgxvelL"] = all_samples.fgxvelL;
      samples["fgxvelR"] = all_samples.fgxvelR;
    }
    if (sample_attr_flag[17]){
      samples["fgyvelL"] = all_samples.fgyvelL;
      samples["fgyvelR"] = all_samples.fgyvelR;
    }
    if (sample_attr_flag[18]){
      samples["fhxvelL"] = all_samples.fhxvelL;
      samples["fhxvelR"] = all_samples.fhxvelR;
    }
    if (sample_attr_flag[19]){
      samples["fhyvelL"] = all_samples.fhyvelL;
      samples["fhyvelR"] = all_samples.fhyvelR;
    }
    if (sample_attr_flag[20]){
      samples["frxvelL"] = all_samples.frxvelL;
      samples["frxvelR"] = all_samples.frxvelR;
    }
    if (sample_attr_flag[21]){
      samples["fryvelL"] = all_samples.fryvelL;
      samples["fryvelR"] = all_samples.fryvelR;
    }
    if (sample_attr_flag[22]){
      samples["hdata_1"] = all_samples.hdata_1;
      samples["hdata_2"] = all_samples.hdata_2;
      samples["hdata_3"] = all_samples.hdata_3;
      samples["hdata_4"] = all_samples.hdata_4;
      samples["hdata_5"] = all_samples.hdata_5;
      samples["hdata_6"] = all_samples.hdata_6;
      samples["hdata_7"] = all_samples.hdata_7;
      samples["hdata_8"] = all_samples.hdata_8;
    }
    if (sample_attr_flag[23]){
      samples["flags"] = all_samples.flags;
    }
    if (sample_attr_flag[24]){
      samples["input"] = all_samples.input;
    }
    if (sample_attr_flag[25]){
      samples["buttons"] = all_samples.buttons;
    }
    if (sample_attr_flag[26]){
      samples["htype"] = all_samples.htype;
    }
    if (sample_attr_flag[27]){
      samples["errors"] = all_samples.errors;
    }
    edf_recording["samples"] = samples;
  }

  edf_recording.attr("class") = "edf";
  return (edf_recording);
}

