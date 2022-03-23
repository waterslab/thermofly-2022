# Draft meta analysis 
# March 23 2022

library(tidyverse)

# Importing summary data from the google sheet
flies <- read.csv("Spring 2022 Thermofly Experiment - Measurements.csv")
head(flies)

# Data cleanup: remove skipped rows
flies <- flies %>% filter(flies$Measurement != "NA")

# Data cleanup: remove empty chambers
flies <- flies %>% filter(flies$Notes != "empty")

# Data cleanup: remove leaky chambers
flies <- flies %>% filter(flies$quality == "pass")


# First plot
ggplot(flies, aes(x=median_temp, y=median_co2_ul.h)) + geom_point() + xlab("Measurement temperature (°C)") + ylab("Metabolic rate (µL CO2/hr)") + stat_smooth(method="lm", formula= y ~ poly(x, 2))

# Separating by sex
ggplot(flies, aes(x=median_temp, y=median_co2_ul.h, col=Sex)) + geom_point() + xlab("Measurement temperature (°C)") + ylab("Metabolic rate (µL CO2/hr)") + stat_smooth(method="lm", formula= y ~ poly(x, 2))


# Mass-specific metabolic rates
ggplot(flies, aes(x=median_temp, y=median_co2_ul.h/Mass_mg)) + geom_point() + xlab("Measurement temperature (°C)") + ylab("Metabolic rate (µL CO2/hr)") + stat_smooth(method="lm", formula= y ~ poly(x, 2))

ggplot(flies, aes(x=median_temp, y=median_co2_ul.h/Mass_mg, col=Sex)) + geom_point() + xlab("Measurement temperature (°C)") + ylab("Metabolic rate (µL CO2/hr)") + stat_smooth(method="lm", formula= y ~ poly(x, 2))


# Mass data: Males vs Females
ggplot(flies, aes(x=Mass_mg, fill=Sex)) + geom_histogram() + facet_grid(Sex ~ .) + guides(fill=F)

# Mass data: Effect of measurement temperature?
ggplot(flies, aes(x=Mass_mg, fill=Sex)) + geom_histogram() + facet_grid(Measurement_temp ~ Sex) + guides(fill=F)


