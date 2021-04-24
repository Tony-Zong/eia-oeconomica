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
if(!require(rvest)) install.packages('rvest')
library(rvest)
if (!require(texreg)) install.packages('texreg')
library(texreg)
if(!require(sjPlot)) install.packages('sjPlot')
library(sjPlot)
theme_set(theme_sjplot())

# We will consider six states: 
# California (CA), Texas (TX), Illinois (IL), Massachusetts (MA), New York (NY), Florida (FL)
state_list <- c('CA', 'TX', 'IL', 'MA', 'NY', 'FL')
state_code <- c('06', '12', '17', '25', '36', '48')

# COMETS 
# https://www.kauffman.org/entrepreneurship/research/comets/
univ_institutional_info <- read_csv(file.path(ddir, 'spring1/univ_institutional_info_v2.csv')) 
univ_system_patents <- read_csv(file.path(ddir, 'spring1/univ_system_patents_v2.csv'))
univ_org_info <- read_csv(file.path(ddir, 'spring1/univ_org_info_v2.csv'))

univ_info <- univ_institutional_info %>%
  inner_join(univ_org_info, by = 'unitid') %>%
  filter(stabbr %in% state_list)

univ_patents <- univ_info %>%
  inner_join(univ_system_patents, by = c('org_id', 'yr')) %>%
  rename(GeoName = city, year = yr) %>%
  mutate(year = as.factor(year),
         State = ifelse(stabbr == 'TX', 'Texas', 
                        ifelse(stabbr == 'CA', 'California',
                               ifelse(stabbr == 'IL', 'Illinois',
                                      ifelse(stabbr == 'MA', 'Massachusetts',
                                             ifelse(stabbr == 'FL', 'Florida', 'New York')))))) %>%
  group_by(instnm, GeoName, State, stabbr, year) %>%
  summarize(num_patents = sum(num_patents)) %>%
  group_by(GeoName, State, stabbr, year) %>%
  summarize(patent_universities_count = n_distinct(instnm), num_patents = sum(num_patents))

# USPTO
# https://www.uspto.gov/web/offices/ac/ido/oeip/taf/univ/org_gr/all_univ_ag.htm

web_address <- 'https://www.uspto.gov/web/offices/ac/ido/oeip/taf/univ/org_gr/all_univ_ag.htm'
webpage_code <- read_html(web_address)

webpage_text <- html_text(html_nodes(webpage_code, '.data , .header'))
index <- 1
# index <- 26
# tail(webpage_text, -25))
for (item in webpage_text) {
  webpage_text[index] <- str_trim(gsub(item, pattern = "\n", replacement = ""), side = 'both')
  index <- index + 1
}

uspto_patents <- data.frame(matrix(unlist(webpage_text), ncol = 25, byrow = TRUE))
colnames(uspto_patents) <- as.character(uspto_patents[1,])
uspto_patents <- uspto_patents[-1,] %>%
  select(-c('Total', 'PRE_1992')) %>%
  melt(id=c('Organizational Identifier', 'State')) %>%
  rename(year = variable, patent_count = value, NAME = `Organizational Identifier`, STATE = State)

# HIFLD
# https://hifld-geoplatform.opendata.arcgis.com/datasets/colleges-and-universities?geometry=25.454%2C-16.798%2C-24.819%2C72.130&selectedAttribute=NAICS_CODE
univ_locations <- read_csv(file.path(ddir, 'Colleges_and_Universities.csv'))
univ_locations <- univ_locations %>%
  select(c('NAME', 'CITY', 'STATE'))

# Error introduced here: only merges on full university names (e.g., see Harvard)
# Maybe do a fuzzy match? 
# Or go through by hand and edit names?
univ_uspto_patents <- uspto_patents %>%
  inner_join(univ_locations, by = c('NAME', 'STATE')) %>%
  filter(STATE %in% state_list) %>%
  rename(GeoName = CITY) %>%
  mutate(year = as.factor(year),
         patent_count = as.numeric(patent_count),
         State = ifelse(STATE == 'TX', 'Texas', 
                        ifelse(STATE == 'CA', 'California',
                               ifelse(STATE == 'IL', 'Illinois',
                                      ifelse(STATE == 'MA', 'Massachusetts',
                                             ifelse(STATE == 'FL', 'Florida', 'New York')))))) %>%
  group_by(GeoName, State, STATE, year) %>%
  summarize(patent_universities_count = n_distinct(NAME), patent_count = sum(patent_count)) %>%
  mutate(patent_universities_count = ifelse(patent_count == 0, 0, patent_universities_count)) %>%
  filter(patent_universities_count > 0)

# BEA
# https://apps.bea.gov/regional/downloadzip.cfm
# Deleted last four rows of each of these files by hand in Excel (generally bad practice...)
bea_ca <- read_csv(file.path(ddir, 'CAINC6N/CAINC6N_CA_2001_2019.csv')) %>%
  mutate(State = 'California')
bea_tx <- read_csv(file.path(ddir, 'CAINC6N/CAINC6N_TX_2001_2019.csv')) %>%
  mutate(State = 'Texas')
bea_il <- read_csv(file.path(ddir, 'CAINC6N/CAINC6N_IL_2001_2019.csv')) %>%
  mutate(State = 'Illinois')
bea_ma <- read_csv(file.path(ddir, 'CAINC6N/CAINC6N_MA_2001_2019.csv')) %>%
  mutate(State = 'Massachusetts')
bea_ny <- read_csv(file.path(ddir, 'CAINC6N/CAINC6N_NY_2001_2019.csv')) %>%
  mutate(State = 'New York')
bea_fl <- read_csv(file.path(ddir, 'CAINC6N/CAINC6N_FL_2001_2019.csv')) %>%
  mutate(State = 'Florida')

bea_full <- rbind(bea_ca, bea_tx, bea_il, bea_ny, bea_fl) %>%
  filter(GeoFIPS != '"36000"' & GeoFIPS != '"06000"' & GeoFIPS != '"48000"' &
           GeoFIPS != '"17000"' & GeoFIPS != '"25000"' & GeoFIPS != '"12000"') %>%
  mutate(GeoName = toupper(GeoName)) %>%
  select(-c('GeoFIPS', 'Region', 'TableName', 'LineCode')) %>%
  melt(id=c('GeoName', 'IndustryClassification', 'Description', 'Unit', 'State')) %>%
  rename(year = variable, compensation = value) %>%
  separate(GeoName, c('GeoName', 'state_code'), ', ') %>%
  select(-c('state_code'))

# Merge datasets
univ_full <- bea_full %>%
  inner_join(univ_uspto_patents, by = c('GeoName', 'year', 'State')) %>%
  select(!c('State')) %>% 
  rename(state = STATE, city = GeoName, naics = IndustryClassification, naics_description = Description,
         compensation_unit = Unit)

univ_naics_simple <- univ_full %>%
  filter(nchar(naics) == 3)

kbi_list <- c(325, 333, 334, 335, 512, 513, 211, 324, 332, 486)
# cross check and flag with KBI list
naics_codes_kbi <- univ_naics_simple %>%
  filter(naics != '...') %>%
  select(c('naics', 'naics_description')) %>%
  distinct() %>%
  mutate(naics = as.numeric(naics)) %>%
  arrange(naics) %>%
  mutate(kbi = ifelse(naics %in% kbi_list, 1, 0)) %>%
  select(-naics_description)

`%not_in%` <- purrr::negate(`%in%`)
univ_kbi <- univ_naics_simple %>%
  filter(naics != '...') %>%
  filter(compensation %not_in% c('(D)', '(NA)', '(NM)')) %>%
  mutate(naics = as.numeric(naics), compensation = as.numeric(compensation)) %>%
  left_join(naics_codes_kbi, by = 'naics') %>%
  select(-c('naics_description', 'compensation_unit')) %>%
  group_by(year, city, state) %>% 
  mutate(total_comp = sum(compensation)) %>%
  group_by(year, city, state, kbi) %>%
  mutate(kbi_comp = sum(compensation)) %>%
  filter(kbi == 1) %>%
  ungroup() %>%
  mutate(kbi_share = kbi_comp/total_comp) %>%
  select(c('year', 'city', 'state', 'patent_universities_count', 'patent_count', 'kbi_comp', 'kbi_share')) %>%
  distinct() %>%
  filter(kbi_comp > 0) %>%
  mutate(ln_kbi_comp = log(kbi_comp)) %>%
  mutate(kbi_comp_mil = kbi_comp/1000) %>%
  mutate(ln_patent_count = log(patent_count))

univ_kbi_pd <- pdata.frame(x = univ_kbi, index = c('city', 'year'))

# REGRESSIONS

lm_fe_1 <- plm(ln_kbi_comp ~ ln_patent_count,
               data = univ_kbi_pd, model = "within", effect = "twoways")

lm_fe_2 <- plm(kbi_comp_mil ~ patent_count,
               data = univ_kbi_pd, model = "within", effect = "twoways")
lm_plot_2 <- lm(kbi_comp_mil ~ patent_count + as.factor(year) + as.factor(city), data = univ_kbi)

lm_fe_3 <- plm(kbi_comp_mil ~ ln_patent_count,
               data = univ_kbi_pd, model = "within", effect = "twoways")
lm_plot_3 <- lm(kbi_comp_mil ~ ln_patent_count + as.factor(year) + as.factor(city), data = univ_kbi)

reg_table <- texreg(list(lm_fe_2, lm_fe_3), include.ci = FALSE, digits = 3,
                 custom.coef.map = list('patent_count'= "UnivPatents",
                                        'ln_patent_count' = "ln(UnivPatents)"),
                 custom.header = list('KBIComp' = 1:2),
                 custom.model.names = c('(1)', '(2)'),
                 caption = 'Regression analysis',
                 include.adjrs = FALSE)
write.table(reg_table, file.path(root, 'docs', 'reg_table.tex'), col.names = FALSE, row.names = FALSE, quote = FALSE)

# FIGURES

model_fig1 <- plot_model(lm_plot_2, type = "pred", terms = "patent_count", 
                         title = 'Model Predicted Knowledge-based industry compensation vs. University patent count | Fixed effects',
                         axis.title = c('University patent count','Knowledge-based industry compensation in millions'))
ggsave(file.path(root, 'docs', 'model_fig1.png'), 
       plot = model_fig1, width = 10, height = 7)

model_fig2 <- plot_model(lm_plot_3, type = "pred", terms = "ln_patent_count", 
                        title = 'Model Predicted Knowledge-based industry compensation vs. University patent count | Fixed effects',
                        axis.title = c('University patent count (log)','Knowledge-based industry compensation in millions'))
ggsave(file.path(root, 'docs', 'model_fig2.png'), 
       plot = model_fig2, width = 10, height = 7)

patents_fig_df <- univ_kbi %>%
  group_by(state, year) %>%
  summarise(patent_count)

patents_fig <- ggplot(data = patents_fig_df, aes(x = year, y = patent_count, group = state, color = state)) +
  geom_smooth(se = FALSE) + 
  xlab('Year') +
  ylab('University patent count') +
  ggtitle('University patent counts in cities in sample')
ggsave(file.path(root, 'docs', 'patents_fig.png'), 
       plot = patents_fig, width = 7, height = 5)

kbi_comp_df <- univ_kbi %>%
  group_by(state, year) %>%
  summarise(kbi_comp_mil)

kbi_comp_fig <- ggplot(data = kbi_comp_df, aes(x = year, y = kbi_comp_mil, group = state, color = state)) +
  geom_smooth(se = FALSE) + 
  xlab('Year') +
  ylab('Knowledge-based industry compensation in millions') +
  ggtitle('Knowledge-based industry compensation in cities in sample')
ggsave(file.path(root, 'docs', 'kbi_comp_fig.png'), 
       plot = kbi_comp_fig, width = 7, height = 5)
