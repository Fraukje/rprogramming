best <- function(state, outcome) {
  ## Read outcome dataset
  datasource <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Set expected mortality type outcomes and indexes
  mortality_types <- c("heart attack", "heart failure", "pneumonia")
  mortality_indexes <- c(11, 17, 23)
  
  ## Check that provided state and outcome are valid
  if(!(state %in% datasource$State)) #Check state is valid
  {stop('invalid state')}
  
  if(!outcome %in% mortality_types) #Check mortality type (outcome) is valid
  {stop('invalid outcome')}
  
  ## Select the right data
  datasource <- datasource[datasource$State == state,] #Drop any out-of-state data
  datasource[, mortality_indexes[mortality_types == outcome]] <- 
    suppressWarnings(as.numeric(datasource[, mortality_indexes[mortality_types == outcome]])) 
    #Select the mortality type. Suppress warnings for trying to convert NA values.
  
  ## Perform calculations
  min_vals <- min(datasource[, mortality_indexes[mortality_types == outcome]], na.rm = TRUE) #Find min value(s)
  hospital_names <- datasource[which(datasource[, mortality_indexes[mortality_types == outcome]]==min_vals),2] #Find hospital name(s) corresponding to min value(s)
  
  ## Return hospital name in that state with lowest 30-day death rate
  hospital_name <- sort(hospital_names) #In case of tie, sort by alphabetic name
  hospital_name[1] #Choose first item
  
  }