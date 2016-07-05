data <- read.csv("http://www.apsnet.org/edcenter/advanced/topics/EcologyAndEpidemiologyInR/DiseaseProgress/Documents/FungicideExample.csv")

library('dplyr')
library('reshape2')
long_data <- data %>% 
  melt(id.vars = "Julian.Date", value.name = "disease") %>%            # long form
  mutate(fungicide = grepl(pattern = "\\.[Tt]rt$", variable)) %>%      # add fungicide column
  transform(fungicide = ifelse(fungicide, "treated", "untreated")) %>% # make it sensible
  transform(variable = gsub("\\.[Tt]rt$", "", variable))               # clean up the variable
head(long_data)

library('ggplot2')
ggplot(long_data) + 
  geom_line(aes(x = Julian.Date, y = disease, color = fungicide)) + 
  facet_wrap(~variable)