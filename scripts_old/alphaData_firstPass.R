library(tidyverse)

wd = '~/Documents/UCSD/Research/Process-Shift/'

alphaData = read_csv(paste0(wd, 'data/alphaData_raw.csv')) %>%
  filter(acc == 1) %>%
  select(subject = su,
         block = bl,
         item = item,
         strategy = strat,
         rt = rt) %>%
  mutate(strategy = fct_recode(as.factor(strategy),
                               unprobed = '0',
                               algorithm = '1',
                               retrieval = '2',
                               forgot = '3'))
alphaData %>% 
  ggplot(aes(x = block, 
             y = rt,
             color = strategy))+
  geom_line(size = .667, alpha = .75) +
  geom_point(size = 1, alpha = .75) +
  theme_linedraw() +
  facet_grid(subject ~ item)+ 
  theme(legend.text=element_text(size=22),
        legend.title=element_text(size=26))


tempData = alphaData %>%
  filter(strategy %in% c('algorithm', 'retrieval')) %>%
  group_by(block, strategy) %>%
  summarize(meanRT = mean(rt),
            seRT = sd(rt)/sqrt(n()))

ggplot(tempData, aes(x = block, 
           y = meanRT,
           color = strategy))+
  geom_errorbar(aes(ymin = meanRT - seRT, ymax = meanRT + seRT),
                width = .1, color = 'grey')+
  geom_line(size = .667, alpha = .75) +
  geom_point(size = 1, alpha = .75) +
  theme_linedraw()

x = 1:50
patterns <- list(
  x %% 35 == 0 ~ "fizz buzz",
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz",
  TRUE ~ as.character(x)
)
case_when(!!!patterns)

  