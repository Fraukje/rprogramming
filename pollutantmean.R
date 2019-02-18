pollutantmean <- function(source, pollutantname, ids) 
##   INPUT
  ## source         = string with data source where files can be found
  ## pollutantname  = string with either 'nitrate' or 'sulfate'
  ## ids            = vector with ids
  
##   DESCRIPTION
  ## Function calculates the mean for all pollutant values across measurement stations.
  
##   OUTPUT
  ## Mean of pollutant values accross all stations. 
  
##   Created 18-02-2019 by Fraukje Coopmans
{
        ## Load stringi library: needed for stri_pad_left function
        library(stringi)
        
        ## Only allow sulfate or nitrate calculations
        if (!xor(pollutantname == 'sulfate', pollutantname == 'nitrate'))
                {stop(paste('Warning: Pollutant name should be sulfate or nitrate'))}   
  
        ## Initialize empty vector that will contain the measurements
        pollutant_values_all <- vector()      
  
        for(i in ids)
        {
                ## Create full datafile name and read data
                datafile <- paste(source, "/", stri_pad_left(i, 3, 0), ".csv", sep = "")
                data <- read.csv(datafile)
        
                ## Extract pollutant values and concatenate to data vector
                pollutant_values_all <- c(y, data[[pollutantname]])
      
        }
        ## Print mean of data without NA values
        mean(pollutant_values_all, na.rm = TRUE)
}