# Analysis of MAVEn flow-through respirometry data
# March 2022

# Data files locally pre-processed in Expedata and exported as CSV
# Data files uploaded to and retrieved from Google Drive
# Files uploaded to github repo for this analysis
# https://happygitwithr.com/rstudio-git-github.html

# Starting the analysis
# Load the required packages
library(tidyverse)
library(mavenR)

# Set directory
data.location <- getwd() #should be rstudio project directory
data.file <- "001-FH-25-20C-2022-03-15_trans_isol_R.csv"
data.label <- "D. pseudoobscura, reared at 25C, measured at 20C, 3/15/2022"
maven.run <- substr(data.file, 1, 3)
output.dir <- paste("./output", "-", maven.run, sep="")

# There's a bug in the package code
# It looks for a column called TC1, so this is a temp fix
temp.file <- read.csv(data.file)
names(temp.file)[12] <- "TC1"
write.csv(temp.file, data.file)

## Evaluate the experimental timecourse ----
maven_raw <- read_maven(datadir = data.location,
  maven_datafile = data.file, baseline = T)

# Save overview plot
plot_maven_overview(maven_raw, maven_experiment = data.label) 
pdf(paste("Overview_", maven.run, ".pdf", sep=""))
dev.off()

# Were there any LEAKS???
levels(as.factor(maven_raw$Chamber[maven_raw$FRC_mlmin < 20]))
maven_raw$Chamber[maven_raw$FRC_mlmin < 20]
# e.g. leaky chamber #5 and #12, leaks <- c(5, 12)
leaks <- c()



##  Data processing pipeline ----
# Step 1: load the MAVEn dataset without baseline ----
maven <- read_maven(datadir = data.location,
  maven_datafile = data.file, baseline = F)


# Step 2: Assign a cycle number to the data ----
maven.cycle <- assign_cyclenumber(maven)

# Step 3a: Extract the metabolism data from the dataset ----
animal_metabolism <- extract_metabolism(maven.cycle)
animal_metabolism

# Step 3b: Visualize the trend data to check for issues in measurements ----
metabolism_trend(animal_metabolism, maven_experiment = maven.run, 
  outdir = output.dir) 

# Step 4: Produce a summary table for animal metabolism ----
## `summarize_metabolism` currently allows the user to create a table for all 
#  data ("by_cycle") or summarized by chamber ("by_chamber"). 

# fixing issue with temp column name
# note that column 6 is "DegC1" aka "TC1" (thermocouple #1)
names(animal_metabolism)[6] <- "TempC"


metabolism_summary_cycle <- summarize_metabolism(animal_metabolism, type = "by_cycle")
metabolism_summary_cycle

# generate summary table by chamber
metabolism_summary_chamber <- summarize_metabolism(animal_metabolism, type = "by_chamber")
metabolism_summary_chamber


summarize_metabolism


# Step 5: Visual diagnostic of calculated data on raw data ----
metabolism_diag(maven_raw, metabolism_summary_cycle, 
  maven_experiment = data.file, outdir = output.dir) + geom_path(size=0.2)




# Step 6a: Extract animal activity data based on metabolism calculations ----
#
# `extract_activity` requires the user to input an interval (measured in 
#   seconds) and a threshold activity level. Given the variability in  
#   where the instrument starts measurements, it is recommended to 
#   select a value no longer than 60 seconds (within the CO2 measurement 
#   interval)

animal_activity <- extract_activity(maven.cycle, metabolism_summary_cycle, 
  interval = 60, activity_baseline = 0.01)

#	James' alternative activity plot
# mc3 <- pivot_longer(maven.cycle, -c(1:15, 32:67))
#vggplot(mc3, aes(x=Seconds, y=value, color=name)) + facet_grid(name~., scale="free_y") +  geom_point(size=0.1) + theme_void()

# baseline sets zero level
# interval: period on either side of the median to include in analysis
#	 note warning for beginning if it goes over

# Step 6b: Plot animal activity ----
# These plots are again, standardized by the measurement number

activity_trend(animal_activity, maven_experiment = maven.run, activity_baseline =.01,
  outdir = output.dir)

# Modifying the activity plot 
p2 <- activity_trend(animal_activity, maven_experiment = data.label, activity_baseline =.01)
p2$layers[[1]]$aes_params$size <- 0
p2$layers[[2]]$aes_params$size <- 0
p2$layers[[3]]$aes_params$size <- 0
p2 + geom_path() + facet_grid(Chamber ~ cycle) + theme_void() + guides(color=F)




# Step 7: Generate animal activity summary tables ----

activity_summary_cycle <- summarize_activity(animal_activity, 
  type = "by_cycle", 
  activity_threshold = 1)
activity_summary_chamber <- summarize_activity(animal_activity, 
  type = "by_chamber")


# Step 8: Visual diagnostic of animal activity ----
activity_diag(maven_raw, metabolism_summary_cycle, activity_summary_cycle,
  maven_experiment = data.file, interval = 40, 
  outdir = output.dir)


# Step 9: Create the finalized data table ----
#
# create data table for analysis purposes

experiment.quality <- rep("pass", 16)
experiment.quality[leaks] <- "fail"

test.out <- maven_datatable(metabolism_summary_cycle, activity_summary_cycle, 
  maven_experiment = maven.run) 
test.out.2 <- test.out %>% 
  add_column(quality = experiment.quality) %>% 
  mutate(source.data = data.file)
test.out.2

# Saving output
final.data.output <- as.data.frame(test.out.2)
write.table(final.data.output, paste(maven.run, "-summary",".csv", sep=""), sep=",", row.names=F)