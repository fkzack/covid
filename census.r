library (jsonlite)
GetStatesAndPopulation <- function() {
  # get 2019 state population data from census bureau
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
  print(str(states))
  return (states)
  head(states)
}