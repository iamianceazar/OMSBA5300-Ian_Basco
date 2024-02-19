library(rio)
library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(vtable)
library(fixest)

# Import data
googleTrends_list <- list.files(path = "./Lab3_Rawdata", pattern = "trends_up_to_", full.names = TRUE)


googleTrends_rawData <- import_list(googleTrends_list, rbind = TRUE)

googleTrends_dataset <- googleTrends_rawData %>% 
  mutate(dateWeek = ymd(str_sub(monthorweek, end = 10))) %>% 
  mutate(dateMonth = floor_date(dateWeek, 'month'))%>% 
  group_by(schname, keyword) %>%
  mutate(indexStandard = (index - mean(index, na.rm = TRUE)) / sd(index, na.rm = TRUE)) %>% 
  group_by(schname, dateMonth) %>% 
  summarize(indexStandard = sum(indexStandard, na.rm = TRUE)) %>% 
  drop_na()
 
# vtable(googleTrends_dataset)
# summary(googleTrends_dataset)
# 
# googleTrends_test <- googleTrends_rawData %>% 
#   mutate(dateWeek = ymd(str_sub(monthorweek, end = 10))) %>% 
#   mutate(dateMonth = floor_date(dateWeek, 'month'))%>% 
#   group_by(schname, keyword) %>%
#   mutate(indexStandard = (index - mean(index, na.rm = TRUE)) / sd(index, na.rm = TRUE)) %>% 
#   group_by(schname, dateWeek) %>% 
#   summarize(indexStandard = sum(indexStandard)) %>% 
#   drop_na()

scoreCard <- import('./Lab3_Rawdata/Most+Recent+Cohorts+(Scorecard+Elements).csv')
names(scoreCard) <- tolower(names(scoreCard))
# print(scoreCard)

idName <- import('./Lab3_Rawdata/id_name_link.csv')
idName <- idName %>% 
  group_by(schname) %>% 
  mutate(n = n()) %>% 
  filter(n == 1)

join_idName <- inner_join(idName, googleTrends_dataset, join_by(schname))
join_scoreCard <- inner_join(join_idName, scoreCard, join_by(unitid, opeid))
summary(join_scoreCard)

mainData <- join_scoreCard %>% 
  filter(preddeg == 3) %>%   # Predominantly bachelor's-degree granting
  mutate(earningsReported = as.integer(`md_earn_wne_p10-reported-earnings`)) %>% #change to integer
  drop_na()

# vtable(mainData)
# summary(mainData)
# summary(mainData$earningsReported) # mean = $42,808

### use $42,808 as baseline for income.
### Sept 01, 2015 as cutoff

mainData <- mainData %>% 
  mutate(earningMedian = case_when(earningsReported >= 42808 ~ '1',
                                   earningsReported < 42808 ~ '0')) %>% 
  mutate(releasedDate = case_when(dateMonth >= ymd("2015-09-01") ~ '1',
                                  dateMonth < ymd("2015-09-01") ~ '0')) %>% 
  drop_na(releasedDate)

summary(mainData)
mainData <- mainData %>% 
  select(-contains("pcip")) %>% 
  select(-contains("ugds")) %>%
  select(-contains("npt4"))
vtable(mainData)

ggplot(mainData, aes(x= dateMonth , y = indexStandard, colour = factor(earningMedian))) +
  geom_smooth(method = 'lm') +
  geom_vline(xintercept = ymd("2015-09-01"), linetype = "dashed", color = "blue") +
  annotate("text", x = ymd("2015-09-01"), y = 6, label = "Released date", vjust = 1, hjust = 1.05, color = "blue", size = 3) +
  scale_color_manual(values = c("1" = "green",
                                "0" = "red"))
# high earning colleges seems to have a lower index than low earning
# colleges after the 09/2015 released date.

mainData %>% 
  group_by(earningMedian, releasedDate) %>% 
  summarize(mean(indexStandard))

ggplot(mainData, aes(x = dateMonth, y = indexStandard)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

regressionInt <- feols(indexStandard ~ i(month(dateMonth)) + releasedDate + earningMedian + releasedDate*earningMedian, data = mainData, vcov='hetero')
summary(regressionInt)
etable(regressionInt)
wald(regressionInt)


regressionFE <- feols(indexStandard ~ i(month(dateMonth)) + releasedDate + earningMedian + releasedDate*earningMedian | schname, data = mainData, vcov='hetero')
summary(regressionFE)
etable(regressionInt,regressionFE)
wald(regressionFE)

