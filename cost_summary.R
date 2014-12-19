storm <- read.csv("repdata-data-StormData.csv.bz2", stringsAsFactors = FALSE)
library(dplyr)


# Remove all those events which did not incur costs
storm_cost_significant <- filter(storm, (PROPDMG != 0 & CROPDMG != 0))

# Replace duplicates caused by case variance
storm_cost_significant$PROPDMGEXP <- sapply(storm_cost_significant$PROPDMGEXP,toupper)
storm_cost_significant$CROPDMGEXP <- sapply(storm_cost_significant$CROPDMGEXP,toupper)

# Convert to factor so that the is.na function works later
#storm_cost_significant$PROPDMGEXP <- factor(storm_cost_significant$PROPDMGEXP)
#storm_cost_significant$CROPDMGEXP <- factor(storm_cost_significant$CROPDMGEXP)

# Method to expand the exponents. Does it match "3" and "5"?
exp <- function (e) if (is.null(e) || is.na(e)) 1 else if (e == 'K') 1000 else if (e == 'M') exp('K') * 1000 else if (e == 'B') exp('M') * 1000 else 1 

# Replace with my own version

expand <- function(x) {
        if (is.na(as.integer(x))) {
                return (1)
        } else {
                if (x == "K") {
                        return (1000)
                } else {
                        if (x == "M") {
                                return (10^6)
                        } else {
                                if (x == "B") {
                                        return (10^9)
                                } else {
                                        return(10^(as.integer(x)))
                                }
                        }
                }
        }
}

# exp2 <- function(x) {
#         if (is.na(as.integer(x))) {
#                 return (1)
#         } else {
#                 if (x == "K") {
#                         return (1000)
#                 } else {
#                         if (x == "M") {
#                                 return (10^6)
#                         } else {
#                                 if (x == "B") {
#                                         return (10^9)
#                                 } else {
#                                         print(paste(x,":",as.integer(x),":",10^(as.integer(x))))
#                                         return(10^(as.integer(x)))
#                                 }
#                         }
#                 }
#         }
# }
        
tpd <- with(storm_cost_significant, PROPDMG * mapply(expand, PROPDMGEXP))
tcd <- with(storm_cost_significant, CROPDMG * mapply(expand, CROPDMGEXP))
# then combine with data frame as below

storm_cost_sum <- cbind(storm_cost_significant[c(1,2,7,8,25:28)], 
                        PropertyCost = tpd, CropCost = tcd)


# Either:
# vexp <- Vectorize(exp)
# 
# total_crop_dmg <- storm_cost_significant$CROPDMG * vexp(storm_cost_significant$CROPDMGEXP)
# total_property_dmg <- storm_cost_significant$PROPDMG * vexp(storm_cost_significant$PROPDMGEXP)
# 
# # Or:
# total_crop_dmg <- with(storm_cost_significant, CROPDMG * mapply(exp,CROPDMGEXP))
# total_property_dmg <- with(storm_cost_significant, PROPDMG * mapply(exp,PROPDMGEXP))
# 
# 
# storm_cost_sum <- cbind(storm_cost_significant[c(1,2,7,8,25:28)], 
#                         PropertyCost = total_property_dmg, CropCost = total_crop_dmg)


property_cost <- storm_cost_sum %>%
        group_by(EVTYPE) %>%
        summarise(Prop_Cost = sum(PropertyCost)) %>%
        arrange(desc(Prop_Cost))

crop_cost <- storm_cost_sum %>%
        group_by(EVTYPE) %>%
        summarise(Crop_Cost = sum(CropCost)) %>%
        arrange(desc(Crop_Cost))

Costs <- merge(property_cost, crop_cost)

Costs <- Costs %>%
        mutate(Total_Cost = Prop_Cost + Crop_Cost) %>%
        arrange(desc(Total_Cost))

# ALTERNATIVELY:

# Order the exponents as factors
storm_cost_significant$PROPDMGEXP <- factor(storm_cost_significant$PROPDMGEXP, 
        levels = c("", "0", "3", "5", "K", "M", "B"), ordered = TRUE)
storm_cost_significant$CROPDMGEXP <- factor(storm_cost_significant$CROPDMGEXP, 
        levels = c("", "0", "K", "M", "B"), ordered = TRUE)

# test <- storm_cost_significant %>%
#         group_by(EVTYPE) %>%
#         arrange(PROPDMGEXP)

# Group together the exponents and sum over them
# Problems arise because some entries add up to more than the exponent value represents
property <- storm_cost_significant %>%
        group_by(PROPDMGEXP, EVTYPE) %>%
        summarise(cost = sum(PROPDMG)) %>%
        arrange(PROPDMGEXP, cost, EVTYPE)

# Reverse to give a descending value of exponent
property[rev(seq(1:nrow(property))),]

crops <- storm_cost_significant %>%
        group_by(CROPDMGEXP, EVTYPE) %>%
        summarise(cost = sum(CROPDMG)) %>%
        arrange(CROPDMGEXP, cost, EVTYPE)

# Reverse to give a descending value of exponent
crops[rev(seq(1:nrow(crops))),]

# This only captures events where there are both F & I
storm_dead <- filter(storm, FATALITIES != 0 & INJURIES != 0)
states_dead  <- storm_dead %>%
        select(STATE, EVTYPE, FATALITIES, INJURIES) %>%
        group_by(STATE) %>%
        filter(FATALITIES == max(FATALITIES)) %>%
        arrange(STATE)

storm_dead1 <- filter(storm, FATALITIES != 0 & INJURIES != 0) %>%
        mutate(CASUALTIES = FATALITIES + INJURIES) %>%
        select(STATE, EVTYPE, CASUALTIES)
states_cas  <- storm_dead1 %>%
        select(STATE, EVTYPE, CASUALTIES) %>%
        group_by(STATE) %>%
        filter(CASUALTIES == max(CASUALTIES)) %>%
        arrange(STATE)

test <- test %>%
        select(STATE, EVTYPE, FATALITIES, INJURIES) %>%
        group_by(STATE) %>%
        filter(FATALITIES == max(FATALITIES)) %>%
        arrange(STATE)

storm_ei_state <- storm_ei %>%
        mutate(Total_Cost = PropertyCost + CropCost) %>%
        select(STATE, EVTYPE, Total_Cost) %>%
        group_by(STATE) %>%
        filter(Total_Cost == max(Total_Cost)) %>%
        arrange(STATE)


#ggplot(test, aes(factor(STATE), weight = FATALITIES, fill = factor(EVTYPE))) + 
#        geom_bar(position="dodge")
ggplot(test, aes(STATE, FATALITIES, fill = EVTYPE)) + 
        geom_bar(position="dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 270))

states_gg <- ggplot(test, aes(STATE, FATALITIES, fill = EVTYPE)) +
        geom_bar(position="dodge", stat = "identity") +
        theme(axis.text.x = element_text(angle = 270)) +
        guides(fill = guide_legend(keyheight = 0.8, keywidth = 0.5))
states_gg

