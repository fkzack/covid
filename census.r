library (jsonlite)


GetStatesAndPopulation <- function() {
  # get 2019 state population data from census bureau
  # return df with state name, abbreviation, fips code, and popultation
  pop <- fromJSON("https://api.census.gov/data/2019/pep/population?get=NAME,POP&for=state:*")
  popNames <- pop[1,]
  pop <- data.frame(pop[-1,], stringsAsFactors = FALSE)
  names(pop) <- popNames
  pop <- rename(pop, "state.name" = NAME , "state.FIPS" = state, "state.population" = POP)
  
  #need to match state names with state abbreviations
  states <- data.frame(state.name, state.abb, stringsAsFactors = FALSE)
  states <- add_row(states, state.name="District of Columbia",  state.abb ="DC")
  states <- add_row(states, state.name="Puerto Rico",  state.abb ="PR")
  states <- merge(states, pop)
  str(states)
  
  return (states)
}



GetCountyPopulationsFromCensus <- function() {
  # get 2019 county population data from census bureau
  # note that the nytimes data combines all of the new york city bouroughs into "new york city", and does not include a fips code,
  # so to use this I would have to combine bouroghs populations and replace into ny
  pop <- fromJSON("https://api.census.gov/data/2019/pep/population?get=NAME,POP&for=county:*")
  popNames <- pop[1,]
  pop <- data.frame(pop[-1,], stringsAsFactors = FALSE)
  names(pop) <- popNames
  pop <- rename(pop, "county.name" = "NAME" , "state.FIPS" = "state", "county.FIPS" = "county", "county.population" = "POP")
  pop$state.FIPS <- as.numeric(pop$state.FIPS)
  pop$county.FIPS <- as.numeric(pop$county.FIPS)
  pop$fips <- pop$state.FIPS * 1000 + pop$county.FIPS
  pop$county.population <- as.numeric(pop$county.population)
  return (pop)
}


#get county populations in a form suitable for use with NYT data
#treat new york city (all five boroughs) as one county with fips 36000
GetCountyPopulationsNYT <- function (){
  #get data from census
  pop <- GetCountyPopulationsFromCensus();
  
  #extract NYC data as used by ny times 
  nyc_fips <- c(5,47,61,81,85) # the five boroughs of manhattan, which NYT combines
  nyc_pop <- sum(as.numeric(pop[which(pop$county.FIPS %in% nyc_fips & pop$state.FIPS==36),]$county.population))
  nyc_entry <- pop[which(pop$county.FIPS == 5  & pop$state.FIPS==36),]
  nyc_entry$county.name <- "New York City"
  nyc_entry$county.population <- nyc_pop
  nyc_entry$state.FIPS = 36
  nyc_entry$county.FIPS = NA
  nyc_entry$fips = 36000
  
  #remove five boroughs and add the combined boroughs
  pop <- pop[-which(pop$county.FIPS %in% nyc_fips & pop$state.FIPS==36),]
  pop <- rbind(nyc_entry, pop)
  
  
  
  
  
}
