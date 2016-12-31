#
#
#
beagle_logger_createLogFile <- function(keyname, settings){
   #
   filename = paste(settings$LORIS_LOGFILE_PREFIX, 
                     "_run-", settings$LORIS_RUN_IDENTIFIER, 
                     settings$LORIS_LOGFILE_EXTENSION, sep="")
   filename_fullpath = file.path(settings$LORIS_ROOT_DIR, keyname, filename)

   # write out a new, empty json file (no need to use rjson here)
   cat("{}",file=filename_fullpath,sep="\n")

   # return an empty list
   emptyList = list()
   return(emptyList)
}


beagle_logger_loadFromFile <- function(keyname, settings){
   #
   filename = paste(settings$LORIS_LOGFILE_PREFIX, 
                     "_run-", settings$LORIS_RUN_IDENTIFIER, 
                     settings$LORIS_LOGFILE_EXTENSION, sep="")
   filename_fullpath = file.path(settings$LORIS_ROOT_DIR, keyname, filename)
   print(filename_fullpath)
   json.lst <- fromJSON(file=filename_fullpath)
   print("done")
   
   return(json.lst)
}


beagle_logger_loadFromFileAsDataFrame <- function(keyname, settings){
   #
   # read the JSON log file into a list()
   logs.lst <- beagle_logger_loadFromFile(keyname, settings)

   # extract the item names from the list
   # e.g.: "hclab_labels_fit_AAL|brynhild|AAL|20100806|stop_timestamp"
   item_names <- names(logs.lst)
   #
   # split at the pipe
   item_names_split <- strsplit(item_names,'|',fixed=TRUE)
   #
   # cast the split components into a data.frame; change column names
   item_names_split.df <- do.call(rbind.data.frame, item_names_split)
   colnames(item_names_split.df) <- c("program", "keyname", "modality", "scanDate", "messageKey")
   #
   # add a sequence column
   item_names_split.df$seqNo <- 1:nrow(item_names_split.df)
   #
   #
   # for the actual values, just remove the names and add the list of values to the data.frame
   names(logs.lst) <- NULL
   item_names_split.df$values <- logs.lst
   
   return(item_names_split.df)
}


beagle_logger_saveToFile <- function(keyname, json.lst, settings){
   #
   # convert to JSON, then write out the json file
   json.str <- toJSON(json.lst)

   filename = paste(settings$LORIS_LOGFILE_PREFIX, 
                     "_run-", settings$LORIS_RUN_IDENTIFIER, 
                     settings$LORIS_LOGFILE_EXTENSION, sep="")
   filename_fullpath = file.path(settings$LORIS_ROOT_DIR, keyname, filename)
   
   # write out a new, empty json file (no need to use rjson here)
   cat(json.str,file=filename_fullpath,sep="\n")
}


beagle_logger_logMessage <- function(progname, keyname, modality, scanDate, key, message, json.lst){
   #
   full_key <- paste(progname, keyname, modality, scanDate, key, sep="|")
   json.lst[[full_key]] <- message
   
   return(json.lst)
}


beagle_logger_logStartMessage <- function(progname, keyname, modality, scanDate, json.lst){
   #
   full_key <- paste(progname, keyname, modality, scanDate, 'start_timestamp', sep="|")
   json.lst[[full_key]] <- format(Sys.time(), "%Y.%m.%d %H:%M:%S")

   return(json.lst)
}


beagle_logger_logStopMessage <- function(progname, keyname, modality, scanDate, json.lst){
   #
   full_key <- paste(progname, keyname, modality, scanDate, 'stop_timestamp', sep="|")
   json.lst[[full_key]] <- format(Sys.time(), "%Y.%m.%d %H:%M:%S")

   return(json.lst)
}







