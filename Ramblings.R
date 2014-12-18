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

# Tyring to grep out related events:
EV_types <- unique(storm$EVTYPE)

# First, remove the 'Summary' events
No_summaries <- setdiff(EV_types,grep("summary", setdiff(EV_types,no_wchf), ignore.case = TRUE, value = TRUE))

# Extreme wind events
wind <- c("wind", "tornado", "torndao", "storm", "hurricane", "cloud", "gustnado", "funnel", "typhoon", 
          "tropical depression", "tstm", "wnd", "remnants of floyd")
wind_related <- grep(paste(wind, collapse = "|"), No_summaries, ignore.case = TRUE, value = TRUE)
#no_wind <- setdiff(No_summaries, wind_related)

# Events related to ice and extreme cold
ice <- c("snow", "ice", "icy", "cold", "hail", "freez", "blizzard", "frost", "hypothermia", "sleet", 
         "avalanche", "winter", "avalance", "fog", "record low", "wintry", "hyperthermia", 
         "low temperature", "cool", "glaze", "heavy mix")
ice_related <- grep(paste(ice, collapse = "|"), No_summaries, ignore.case = TRUE, value = TRUE)
#no_ice <- setdiff(No_summaries, ice_related)

# Events related to extreme heat
heat <- c("heat", "warm", "dry", "driest", "drought", "hot", "record high", "dust", "fire", "smoke",
          "high temperature")
heat_related <- grep(paste(heat, collapse = "|"), No_summaries, ignore.case = TRUE, value = TRUE)
#no_heat <- setdiff(No_summaries, heat_related)

# Rain and flood related events
flood <- c("rain", "flood", "floood", "wet", "precipitation", "precip", "slide", "shower", "burst", 
           "dam", "fld", "slump", "stream", "rising water", "drowning", "urban")
flood_related <- grep(paste(flood, collapse = "|"), No_summaries, ignore.case = TRUE, value = TRUE)
#no_flood <- setdiff(No_summaries, flood_related)

# Marine events
marine <- c("sea", "marine", "current", "tide", "surf", "spout", "swell", "seiche", "tsunami", "wave", 
            "surge", "high water")
marine_related <- grep(paste(marine, collapse = "|"), No_summaries, ignore.case = TRUE, value = TRUE)

# Lightning
lightning <- c("lightning", "lighting", "ligntning", "red flag criteria")
lightning_related <- grep(paste(lightning, collapse = "|"), No_summaries, ignore.case = TRUE, value = TRUE)

# Volcanic
volcanic <- c("volcanic", "vog")
volcanic_related <- grep(paste(volcanic, collapse = "|"), No_summaries, ignore.case = TRUE, value = TRUE)

other <- setdiff(No_summaries,
        union(wind_related,
              union(ice_related,
                    union(heat_related,
                          union(flood_related,
                                union(marine_related,
                                      union(lightning_related, volcanic_related)))))))

uncategorised <- storm[storm$EVTYPE %in% other,]

storm_cost_significant <- filter(storm, (PROPDMG != 0 & CROPDMG != 0))

expon <- function(x) {
    if (x == "B" | x == "b" ) {
        x <- 10^9
    } else {
        if (x == "M" | x == "m") {
            x <- 10^6
        } else {
            if (x == "K" | x == "k") {
                x <- 10^3
            } else {
                if (x == "") {
                    x <- 1
                } else {
                    x <- 10^as.numeric(x)
                }
            }
        }
    }
    print(x)
}
