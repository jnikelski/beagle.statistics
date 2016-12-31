read_beagle_settings <- 
function(settings_file, verbose=FALSE){
   
   # create a temp env and source settings into it
   myenv <- new.env()
   sys.source(settings_file, myenv)

   # retrieve the settings as a list
   settings <- mget(ls(myenv), myenv)
   
   # print prior to return, if verbose
   if ( verbose ) {print(settings)}

   # return the list
   return(settings)
}


read_beagle_aggregated_settings <-
function(settings_file, verbose=FALSE){
   #
   json.lst <- fromJSON(file=settings_file)
   
   # print prior to return, if verbose
   if ( verbose ) {print(json.lst)}

   # return the list
   return(json.lst)
}

