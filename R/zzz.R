# This is merely a heck to prevent "no visible binding for global variable" notes
# I am not particularly fond of it, but it appears to be the preferred solution
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("type", "entime", "entime_rel", "duration", "eye", "time",
                           "read", "status", "flags", "input", "buttons", "parsedby",
                           "message", "assignment", "assignment2", "variable", "value", "trial", "sttime", "sttime_rel"))
}
