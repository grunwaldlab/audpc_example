# Load packages
library('knitr')     # Used to make tables 
library('dplyr')     # Used for data.frame manipulations
library('tidyr')     # Used for data.frame manipulations
library('agricolae') # Used to calculated AUDPC
library('ggplot2')   # Used for graphing
library('magrittr')  # Adds %>% operator for chaining commands


# Parse input file
data <- read.csv("fungicide_data.csv")


# Tidy up the data
long_data <- data %>% 
  gather(cultivar, disease, -Julian.Date) %>% # gather by cultivar, call the new variable disease, and keep the Julian.Date column
  mutate(fungicide = grepl(pattern = "\\.trt$", cultivar, ignore.case = TRUE)) %>% # add fungicide column
  transform(fungicide = ifelse(fungicide, "treated", "untreated")) %>% # make it sensible
  transform(cultivar = gsub("\\.trt$", "", cultivar, ignore.case = TRUE)) # clean up the variable


# Graph the disease progress curves
dis_plot <- ggplot(long_data)
dis_plot <- dis_plot + geom_line(aes(x = Julian.Date, y = disease, linetype = cultivar))
dis_plot <- dis_plot + facet_wrap(~fungicide)
dis_plot <- dis_plot + theme_bw()
dis_plot <- dis_plot + labs(x = "Julian Day", y = "Percent Disease", title = "Disease Progress Curve")
dis_plot


# Calculate AUDPC
audpc_data <- long_data %>% 
  group_by(cultivar, fungicide) %>% # do any following calculations on every combination of treatments
  summarize(AUDPC = audpc(disease, Julian.Date)) 
audpc_data %>% 
  spread(fungicide, AUDPC) %>% # create a table with fungicide in columns and cultivar in rows
  arrange(untreated) %>%       # Sort by untreated
  kable() # display as a table


# Creating a barplot for the AUDPC results
audpc_plot <- ggplot(audpc_data, aes(x = fungicide, y = AUDPC, fill = cultivar))
audpc_plot <- audpc_plot + geom_bar(stat = "identity", position = "dodge")
audpc_plot <- audpc_plot + theme_bw()
audpc_plot <- audpc_plot + labs(title = "Area Under the Disease Progress Curve")
# audpc_plot <- audpc_plot + scale_fill_brewer(type = "qual", palette = "Set1") # use color
audpc_plot <- audpc_plot + scale_fill_grey()
audpc_plot
