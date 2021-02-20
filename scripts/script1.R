library(tidyverse)
library(cowplot)
library(lubridate)

setwd("D:/oeconomica/final_project")  #change it to your directory

patents_v2 <- read_csv("patents_v2.csv")
patent_us_classes <-  read_csv("patent_us_classes_v2.csv")
patent_intl_classes <-  read_csv("patent_int_classes_v2.csv")

patent_new <- inner_join(patents_v2, patent_us_classes, 
                         by = "patent_id") %>%
  rename(pos_us = pos)

patent_new <- inner_join(patent_new, patent_intl_classes, 
                         by = "patent_id") %>%
  rename(pos_intl = pos)

grants_v2 <- read_csv("grants_v2.csv")

## Finding: The US and international patent classification are not informative
## Solution: either find the complete codebook for patent classification, or 
## study the Zucker-Darby Science and Technology Area Category first


####### by ZD Category #############

patent_zd_cats <- read_csv("patent_zd_cats_v2.csv")
grant_zd_cats <- read_csv("grant_zd_cats_v2.csv")

# see which ZD category has more patents
patent_zd_sum <- patent_zd_cats %>%
  group_by(zd)%>%
  summarize(count=n())%>%
  arrange(desc(count))

p1 <- ggplot(patent_zd_cats, aes(x = zd)) +
  geom_bar()


# see which ZD category has more grants
grant_zd_sum <- grant_zd_cats %>%
  group_by(zd)%>%
  summarize(count=n())%>%
  arrange(desc(count))

p2 <- ggplot(grant_zd_cats, aes(x = zd)) +
  geom_bar()

# compare the results
plot_grid(p1, p2, labels = c("patents","grants"))


######## by year #############

count_year <- patents_v2 %>%
  mutate(year = year(grant_date)) %>%
  group_by(year) %>%
  summarise(count = n())

ggplot(count_year, aes(x = year, y = count)) +
  geom_line()









          