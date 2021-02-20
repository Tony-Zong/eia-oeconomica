##-----##
# Oeconomica's Entrepreneurship, Innovation & Antitrust Cohort, 2020–2021
##-----##

# Everything you need to run this code (data and instructions) is here:
# https://github.com/gelkouh/eia-oeconomica

# This snippet of code is a little loop that makes my code work on your computer
root <- getwd()
while(basename(root) != "eia-oeconomica") { # this is the name of your project directory you want to use
  root <- dirname(root)
}

# This line runs the script in your data.R file so that each person can have
# their data in a different place because everyone's file structure will be 
# a little different
source(file.path(root, "data.R"))

# This is the specific folder we want to access data from
ddir <- file.path(ddir, 'eia cohort project')

# Loading the packages we want
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
library(readxl)
if (!require(scales)) install.packages('scales')
library(scales)
if (!require(reshape2)) install.packages('reshape2')
library(reshape2)
if (!require(cowplot)) install.packages('cowplot')
library(cowplot)
if (!require(plm)) install.packages('plm')
library(plm) # masks lag from dplyr (so we need to specify dplyr::lag)
if (!require(stargazer)) install.packages('stargazer')
library(stargazer) 

##-----##
# Load data and select columns of interest
##-----##

loadRData <- function(fileName){
  # Loads RData file and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# Panel Study of Entrepreneurial Dynamics, PSED II, United States, 2005-2011 (ICPSR 37202)
# http://www.psed.isr.umich.edu/psed/home
df_icpsr_screeners <- loadRData(file.path(ddir, 'ICPSR_37202', 'DS0001', '37202-0001-Data.rda'))
df_icpsr_followups <- loadRData(file.path(ddir, 'ICPSR_37202', 'DS0003', '37202-0003-Data.rda'))

# ICPSR data cut down to columns of interest
df_icpsr_selected_cols <- df_icpsr_followups %>%
  select(c('CDIVISON', 'METRO', 'QS7', 'INCOME', 'REGIONA', 'IWYEAR', 'AA1', 'BA1A', 'AA5A', 'AE15',
           'AB1', 'AF1', 'AF9', 'AF10', 'AQ10_1', 'AS5'))

# Connecting Outcome Measures in Entrepreneurship, Technology, and Science (COMETS) database
# https://www.kauffman.org/entrepreneurship/research/comets/
# Note: read_csv works with zipped files
df_comets_patents <- read_csv(file.path(ddir, 'COMETS', 'All CSV', 'Patent CSV', 'patents_v2.csv.zip')) %>%
  select(c('patent_id', 'grant_date', 'patent_title'))
df_comets_patent_cite_counts <- read_csv(file.path(ddir, 'COMETS', 'All CSV', 'Patent CSV', 'patent_cite_counts_v2.csv.zip'))
df_comets_patent_us_classes <- read_csv(file.path(ddir, 'COMETS', 'All CSV', 'Patent CSV', 'patent_us_classes_v2.csv.zip')) %>%
  select(c('patent_id', 'us_class'))
# This failed for some reason: 
# df_comets_patent_assignees <- read_csv(file.path(ddir, 'COMETS', 'All CSV', 'Patent CSV', 'patent_assignees_v2.csv.zip')) %>%
#   select(c('patent_id', 'org_type'))
df_comets_patent_zd_cats <- read_csv(file.path(ddir, 'COMETS', 'All CSV', 'Patent CSV', 'patent_zd_cats_v2.csv.zip')) %>%
  rename(patent_id = patent) %>%
  select(c('patent_id', 'zd'))

# Note: amount is NA for all values (probably to anonymize the data)
df_comets_grants <- read_csv(file.path(ddir, 'COMETS', 'All CSV', 'Grant CSV', 'grants_v2.csv.zip')) %>%
  select(c('grant_num', 'grant_agency', 'start_date', 'end_date', 'amount'))
df_comets_grantee_orgs <- read_csv(file.path(ddir, 'COMETS', 'All CSV', 'Grant CSV', 'grantee_orgs_v2.csv.zip')) %>%
  select(c('grant_num', 'grant_agency', 'org_type'))
df_comets_grant_zd_cats <- read_csv(file.path(ddir, 'COMETS', 'All CSV', 'Grant CSV', 'grant_zd_cats_v2.csv.zip')) %>%
  select(c('grant_num', 'grant_agency', 'zd'))

# Subsetting
subset_df <- function(df, n) {
  set.seed(60637)
  df[sample(1:nrow(df), n, replace=FALSE),]
}

df_comets_patents_subset <- subset_df(df_comets_patents, 10000)
df_comets_patent_subset <- df_comets_patents_subset %>%
  left_join(df_comets_patent_cite_counts, by = 'patent_id') %>%
  left_join(df_comets_patent_us_classes, by = 'patent_id') %>%
  left_join(df_comets_patent_zd_cats, by = 'patent_id')

df_comets_grants_subset <- subset_df(df_comets_grants, 207700)
df_comets_grant_subset <- df_comets_grants_subset %>%
  left_join(df_comets_grantee_orgs, by = 'grant_num') %>%
  left_join(df_comets_grant_zd_cats, by = 'grant_num')

# NBER Manufacturing 
# https://www.nber.org/research/data/nber-ces-manufacturing-industry-database
df_nber_sic5811 <- read_csv(file.path(ddir, 'NBER Manufacturing', 'sic5811.csv')) %>%
  select(c('sic', 'year', 'emp', 'pay', 'prode', 'prodw', 'vadd', 
           'invest', 'dtfp5', 'equip'))

sic_names <- read_csv(file.path(ddir, 'NBER Manufacturing', 'sic_names_87.csv'), col_names = c('sic', 'sic_name'))

df_nber_sic5811 <- merge(df_nber_sic5811, sic_names, by = 'sic')

# NAICS codes actually merge better with entrepreneurship data
df_nber_naics5811 <- read_csv(file.path(ddir, 'NBER Manufacturing', 'naics5811.csv')) %>%
  select(c('naics', 'year', 'emp', 'pay', 'prode', 'prodw', 'vadd', 
           'invest', 'dtfp5', 'equip'))

# Here are the four datasets we will use
# We will "unsubset" at the end to run our regressions on the whole dataset
View(df_icpsr_selected_cols)
View(df_comets_patent_subset)
View(df_comets_grant_subset)
View(df_nber_naics5811)

##-----##
# Exploratory Data Analysis (make a new script with this code)
##-----##

## Finding: The US and international patent classification are not informative
## Solution: either find the complete codebook for patent classification, or 
## study the Zucker-Darby Science and Technology Area Category first

# see which ZD category has more patents
patent_zd_sum <- df_comets_patent_subset %>%
  group_by(zd)%>%
  summarize(count=n())%>%
  arrange(desc(count))

p1 <- ggplot(df_comets_patent_subset, aes(x = zd)) +
  geom_bar()

# see which ZD category has more grants
grant_zd_sum <- df_comets_grant_subset %>%
  group_by(zd)%>%
  summarize(count=n())%>%
  arrange(desc(count))

p2 <- ggplot(df_comets_grant_subset, aes(x = zd)) +
  geom_bar()

# compare the results
plot_grid(p1, p2, labels = c("patents","grants"))

# Capital expenditures in different industries 

df_invest_by_sic <- df_nber_sic5811 %>%
  group_by(sic) %>%
  summarize(invest_per_equip_mean = mean(invest/equip), sic_name, year) %>%
  arrange(desc(invest_per_equip_mean))

ggplot(df_invest_by_sic, aes(x = sic, y = invest_per_equip_mean)) +
  geom_col() +
  theme_void()

# Merge ICPSR on SIC codes (AA1A)
df_icpsr_selected_cols <- df_icpsr_followups %>%
  rename(naics = AA1) 

icpsr_nber <- merge(df_icpsr_selected_cols, df_nber_naics5811, on = 'naics')
View(icpsr_nber)
