#### # # # # 
# Alphabet Data p(retrieval) analysis


dat = read_csv('~/Documents/UCSD/Research/Process-Shift/data/alphaData_raw.csv') %>%
  filter(acc == 1) %>%
  select(subject = su,
         trial = bl,
         item = item,
         strategy = strat,
         RT = rt) %>%
  mutate(strategy = fct_recode(as.factor(strategy),
                               unprobed = '0',
                               algorithm = '1',
                               retrieval = '2',
                               forgot = '3'),
         is.alg = ifelse(strategy == 'algorithm', 1, 0),
         is.ret = ifelse(strategy == 'retrieval', 1 , 0),
         is.unknown = ifelse(strategy == 'unprobed', 1 , 0)) %>%
  filter(strategy != 'forgot') %>%
  mutate(subject = as.numeric(as.factor(subject)),
         item = as.numeric(as.factor(item))) 


by.trial.dat <- dat %>%
  filter(strategy != 'unprobed') %>%
  group_by(trial) %>%
  summarize(Prop_Retrieval = mean(strategy == 'retrieval'),
            successes = sum(strategy == 'retrieval'),
            total = n(),
            se = sqrt((Prop_Retrieval*(1-Prop_Retrieval)) / n()))


trial.plot <- by.trial.dat %>%
  ggplot(aes(x = trial, y = Prop_Retrieval))+
  geom_ribbon(aes(ymin = Prop_Retrieval - se,
                  ymax = Prop_Retrieval + se),
              fill = 'red', alpha = .5)+
  geom_line()+
  geom_point()+
  theme_linedraw()
trial.regression <- glm(cbind(successes, total) ~ trial, family = binomial(), data = by.trial.dat)
summary(trial.regression) # AIC: 374.68

log.trial.plot <- by.trial.dat %>%
  ggplot(aes(x = log(trial), y = Prop_Retrieval))+
  geom_ribbon(aes(ymin = Prop_Retrieval - se,
                  ymax = Prop_Retrieval + se),
              fill = 'red', alpha = .5)+
  geom_line()+
  geom_point()+
  theme_linedraw()
log.trial.regression <- glm(cbind(successes, total) ~ log(trial), family = binomial(), data = by.trial.dat)
summary(log.trial.regression) # AIC: 278.09


