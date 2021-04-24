library(tidyverse)
library(cowplot)
library(lubridate)

setwd("D:/oeconomica/final_project/spring1")  #change it to your directory

# data import
univ_institutional_info <- read_csv("univ_institutional_info_v2.csv")
univ_system_patents <- read_csv("univ_system_patents_v2.csv")
univ_org_info <- read_csv("univ_org_info_v2.csv")

glimpse(univ_institutional_info)
glimpse(univ_system_patents)
glimpse(univ_org_info)

# unitid: Integrated Post-secondary Education Data System identifier


# merge data
univ_info <- inner_join(univ_org_info,univ_institutional_info,
                        by ="unitid")
glimpse(univ_info)
glimpse(univ_system_patents)

univ_patents <- inner_join(univ_info,univ_system_patents, by = c("org_id","yr"))

glimpse(univ_patents)

ggplot(univ_patents, aes(x = yr, y =num_patents))+
  geom_point()

univ_by_city <- univ_patents %>%
  group_by(city) %>%
  summarise(num_univ = n_distinct(unitid))

patent_by_city <- univ_patents %>%
  group_by(city) %>%
  summarise(num_patent = sum(num_patents))

by_city <- inner_join(univ_by_city,patent_by_city, by = "city")

# plot
ggplot(by_city, aes(x = num_univ, y = num_patent))+
  geom_point()

# linear regression
mod <- lm(num_univ ~ num_patent, data = by_city)
summary(mod)



