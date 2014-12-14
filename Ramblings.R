# Read in some of the csv file to grab the variable names
storm <- read.csv("repdata-data-StormData.csv", stringsAsFactors = FALSE, nrows = 10)

# Convert selected columns to factors: STATE, EVTYPE
# Look at work done on RT report
names(storm)

# cols <- c("character",
#           "character",
#           "numeric",
#           "character",
#           "character",
#           "character",
#           "factor",
#           "factor",
#           rep("numeric",10), # needed? could be "NULL"
#           "numeric",
#           "numeric",
#           "numeric", # F? factor of some kind?
#           "numeric",
#           "numeric",
#           "numeric",
#           "numeric",
#           "character", #PROPDMGEXP, factor?
#           "numeric",
#           "character", #CROPDMGEXP, factor?
#           rep("character",3),
#           rep("numeric",4),
#           "character",
#           "character")

storm <- read.csv("repdata-data-StormData.csv", stringsAsFactors = FALSE)

# Construct a new data set with only the relevant data, 
# i.e. Fatalities/Injuries, economic impact indicators

storm_dead <- storm %>%
    group_by(EVTYPE) %>%
    summarise(FATALITIES = sum(FATALITIES,na.rm = T), INJURIES = sum(INJURIES, na.rm = T)) %>%
    arrange(desc(FATALITIES), desc(INJURIES))

# Breakdown of casualties from Tornados by state

tornado_by_st <- storm %>%
    filter(EVTYPE == "TORNADO") %>%
    group_by(STATE) %>%
    summarise(FATALITIES = sum(FATALITIES,na.rm = T), INJURIES = sum(INJURIES, na.rm = T)) %>%
    arrange(desc(FATALITIES), desc(INJURIES))

# Create a mapping of State Abbreviations to Names
# Merge this with existing data sets to allow mapping using map_data("state")
# Would need to delete "DC" and "PR"

states <- cbind(state.abb,state.name)


# Worth analysing this data.frame to group similar categories

# Construct a new data set with only the relevant data, 
# i.e. Fatalities/Injuries, economic impact indicators

# As decisions made at state level, would be useful to give a breakdown by state
# Use ggplot2 map coords to illustrate?
# But see Deisgnator codes in table 2.1.1. of data documentation