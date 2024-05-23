#####Create Telemedicine dataset####
##Anna Gage, annagage@uw.edu
##10.07.2023

rm(list = ls())
#library(RODBC)
library(data.table)
library(tidyverse)
library(lme4)
library(table1)
library(arrow)
library(PanelMatch)
library(PSweight)
library(usmap)
library(ggpubr)

##Create a Table 1

#Client characteristics
client_dem <- read.csv("D:/Users/anna_gage/Documents/Healthjump analysis/Data/client_dem.csv")
april_2020 <- c("AZ", "CA", "CT", "IL", "IA", "MA", "MT", "NH", "RI", "VT", "WA", "GA", "AR", "NM", "CO", "MN", "HI", "KT", "VA", "NJ", "DE")
other_date <- c("MD", "NV", "NY", "OK", "OR", "TN")
client_dem <- client_dem %>%
  mutate(parity_group = case_when(state %in% april_2020 ~ "2_april_2020", 
                                  state %in% other_date ~ "3_other_date", 
                                  TRUE ~ "1_Never"))

client_dem %>%
  group_by(parity_group, state) %>%
  summarize(count = n()) %>%
  group_by(parity_group) %>%
  summarize(n_states = n_distinct(state))

state_mandates<- read.csv("D:/Users/anna_gage/Documents/Healthjump analysis/Data/ihme_mandate_stringency.csv")
left_join(client_dem, state_mandates, by = "state") %>%
  group_by(state, parity_group) %>%
  summarize(index = first(index)) %>%
  group_by(parity_group) %>%
  summarize(index = mean(index), sd = sd(index))
client_state <- left_join(client_dem, state_mandates, by = "state") %>%
  group_by(state, parity_group) %>%
  summarize(index = first(index)) 


summary(client_dem$PCT_RURAL_2010)
summary(client_dem$POVERTYRATE)
client_dat <- read.csv("D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_client_imp.csv")
client_dat <- subset(client_dat, imp==1) %>%
  group_by(CLIENT_ID) %>%
  summarise(mpatients = mean(mpatients, na.rm = TRUE))
client_dem <- left_join(client_dem, client_dat)
client_dem %>%
  group_by(parity_group) %>%
  summarize(count = n(), across(c(mpatients, PCT_RURAL_2010, POVERTYRATE), ~median(.x, na.rm = TRUE)))
client_dem %>%
  group_by(parity_group) %>%
  summarize(across(c(mpatients, PCT_RURAL_2010, POVERTYRATE), ~IQR(.x, na.rm = TRUE)))
client_dem %>%
  summarize(across(c(mpatients, PCT_RURAL_2010, POVERTYRATE), ~IQR(.x, na.rm = TRUE)))

#Patient characteristics
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.0f%%)", FREQ, PCT))))
}
abbrev.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%0.0f%%", PCT))))
}
my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("",
                                                           "Mean (SD)"=sprintf("%s (%s)", MEAN, SD)))
}
patients <- read_feather("D:/Users/anna_gage/Documents/Healthjump analysis/Data/completed_1.arrow")
patients <- left_join(patients, client_dem, by = "CLIENT_ID") %>%
  mutate(age = factor(case_when(2020-BIRTH_YEAR<15 ~"Under 15", 
                                2020-BIRTH_YEAR>=15 & 2020-BIRTH_YEAR<65 ~ "15-64", 
                                2020-BIRTH_YEAR>=65 ~ "65 plus")),
         female = gender=="F", 
         re = factor(re, levels=c("wnh", "bnh", "hispanic", "aapi", "aian"), 
                     labels = c("White", "Black", "Hispanic", "Asian", "AI/AN")))

label(patients$female) <- "Female"
label(patients$age) <- "Age"
label(patients$re) <- "Race/Ethnicity"
label(patients$VISIT_COUNT) <- "Visits 2019-2022"

table1(~ age + female + re + VISIT_COUNT | parity_group, data =patients, render.categorical = abbrev.cat, render.continuous = my.render.cont)

proc_pull <- read_feather("D:/Users/anna_gage/Documents/Healthjump analysis/Data/proc_pull.arrow")
telehealth_dat <- read.csv("D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_client_imp.csv")
subset(telehealth_dat, imp == 1) %>%
  select(c(CLIENT_ID, telehealth, mpatients)) %>%
  left_join(client_dem, by= "CLIENT_ID") %>%
  group_by(parity_group) %>%
  summarize(tpatients = sum(mpatients.y), telehealth = weighted.mean(telehealth, mpatients.y))

####Make Maps####
client_dem <- read.csv("D:/Users/anna_gage/Documents/Healthjump analysis/Data/client_dem.csv")
state_clients <- client_dem %>%
  group_by(state) %>%
  summarize(clients = case_when(n()<4 ~"1-3", 
                                n()>3 & n()<7 ~ "4-6", 
                                n()>6 & n()<15 ~ "7-15", 
                                n()>14 ~ "Over 15"))
state <- c("AK", "NV", "ND", "SD", "WI", "VT", "HI", "DC")
clients <- c("0", "0", "0", "0", "0", "0", "0", "0")
add_states <- data.frame(state, clients)
state_clients<- rbind(state_clients, add_states)
state_clients$clients <- as.factor(state_clients$clients)
plot_usmap(data = state_clients, values = "clients") +
  scale_fill_manual(values = c("0" = "gray", "1-3"="#ffffcc", "4-6" = "#a1dab4", "7-15"="#41b6c4","Over 15"="#225ea8"), name= "Networks")

#Parity start date
parity_states<- c("AZ", "AR", "CA", "CO", "CT", "DE", "GA", "HI", "IL", "IA", "KY", "MD", "MA", "MN", "MT", "NV", "NH",
                  "NJ", "NM", "NY", "OK", "OR", "RI", "TN", "VT", "VA", "WA")
state_clients <- state_clients %>%
  mutate(parity = state %in% parity_states,
         parity_date = case_when(parity == FALSE ~ "Never", 
                                 state %in% c("AR", "CO", "DE", "HI", "KY", "MN", "NJ", "NM", "VA") ~ "Before 2019",
                                 state =="GA" ~ "Jan 2020",
                                 state %in% c("AZ", "CA", "CT", "IL", "IA", "MA", "MT", "NH", "RI", "VT", "WA") ~ "April 2020", 
                                 state == "TN" ~ "Aug 2020",
                                 state == "OR" ~ "June 2021",
                                 state == "MD" ~ "July 2021", 
                                 state == "NV"~ "Oct 2021", 
                                 state == "OK"~ "Jan 2022", 
                                 state == "NY" ~ "Apr 2022"))
plot_usmap(data = state_clients, values = "parity_date") +
  scale_fill_manual(values = c("Never" = "gray", "Before 2019"="#ffffd9", 
                               "Jan 2020" = "#edf8b1", "April 2020"="#c7e9b4",
                               "Aug 2020"="#7fcdbb", "June 2021" = "#41b6c4", 
                               "July 2021" = "#1d91c0", "Oct 2021" = "#225ea8", 
                               "Jan 2022" = "#253494", "Apr 2022" = "#081d58"), 
                    breaks = c("Never", "Before 2019", "Jan 2020", "April 2020", "Aug 2020", "June 2021", "July 2021", "Oct 2021", "Jan 2022", "Apr 2022"), 
                    name= "Mandated parity start date")+
  theme(legend.position = "right")


#Figure 1: Overall telehealth usage
sum_dat <- read.csv("D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_client_imp.csv")
ne_states <-c("ME", "VT", "NH", "MA", "CT", "RI", "NY", "NJ", "PA", "MD", "DE")
mw_states <-c("MI", "OH", "IN", "IL", "WI", "MN", "IA", "MO", "ND", "SD", "NE", "KS")
south_states <-c("VA", "WV", "KY", "TN", "NC", "SC", "GA", "FL", "AL", "MS", "AR", "LA", "OK", "TX", "PR")
west_states <-c("MT", "WY", "CO", "NM", "AZ", "NV", "UT", "ID", "WA", "OR", "CA", "AK", "HI")
sum_dat <- subset(sum_dat, imp==1) %>%
  mutate(weight = mpatients/sum(mpatients), 
         month = as.Date(month, format = '%Y-%m-%d'), 
         Location = ifelse(PCT_RURAL_2010<25, "Urban", "Rural \n(Zip3 over \n25% rural)"), 
         Location_det = case_when(PCT_RURAL_2010<5.3 ~ "Most urban", 
                                  PCT_RURAL_2010>5.3 & PCT_RURAL_2010<27.4 ~ "Part urban", 
                                  PCT_RURAL_2010>27.4 & PCT_RURAL_2010<39 ~ "Part rural",
                                  PCT_RURAL_2010>39 ~ "Most rural"),
         Poverty = ifelse(POVERTYRATE>15, "More poverty \n(Over 15% of Zip \nunder poverty \nthreshold)", "Less poverty"),
         Region = case_when(state %in% ne_states ~ "Northeast", 
                            state %in% mw_states ~ "Midwest", 
                            state %in% south_states ~ "South", 
                            state %in% west_states ~ "West")) %>%
  group_by(CLIENT_ID) %>%
  mutate(avg_patients = mean(mpatients))
sum_dat <- sum_dat %>% 
  ungroup() %>%
  mutate(Size= ifelse(avg_patients<1300, "Under 1300 \npatients per \nmonth", "1300 or more \npatients per \nmonth"),
         size_det = as.factor(cut(avg_patients, 
                                  breaks = quantile(avg_patients, probs=0:5/5, na.rm = TRUE), 
                                  labels = FALSE, include.lowest = TRUE)))

fig_1a_dat<- sum_dat %>%
  group_by(month) %>%
  summarize(percent = 100*weighted.mean(telehealth, weight), 
            lci = percent - 1.96*sqrt(sum(weight^2)*percent*(100-percent)), 
            uci = percent + 1.96*sqrt(sum(weight^2)*percent*(100-percent)))
fig_1a<- ggplot(fig_1a_dat, aes(x = month, y = percent)) +
  geom_line() +
  geom_ribbon(aes(ymin = lci, ymax = uci), 
              alpha = 0.2, color = NA) +
  labs(y = "Telehealth visits per 100 outpatient visits", x = "Visit date") +
  ggtitle("1a") +
  theme_bw() 

####Panel 2: Telehealth use by characteristic over time####

#Figure of telehealth use by race over time
sum_re_imp <- read.csv("D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_month_imp.csv")
imp_number <- 10
th_list <- paste0("telehealth_", seq(1:imp_number))

fig1_pooled <- pivot_wider(sum_re_imp, id_cols = c(month, Race.Ethnicity), names_from = imp, values_from = c(telehealth, se))

fig1_pooled$th_adj <- rowMeans(fig1_pooled[, th_list])
fig1_pooled <- fig1_pooled %>%
  rowwise() %>%
  mutate(v_b = sum((telehealth_1-th_adj)^2, (telehealth_2-th_adj)^2, (telehealth_3-th_adj)^2, (telehealth_4-th_adj)^2, 
                   (telehealth_5-th_adj)^2, (telehealth_6-th_adj)^2, (telehealth_7-th_adj)^2, (telehealth_8-th_adj)^2, 
                   (telehealth_9-th_adj)^2, (telehealth_10-th_adj)^2)/(imp_number-1),
         v_w = sum((se_1)^2, (se_2)^2, (se_3)^2, (se_4)^2, (se_5)^2, (se_6)^2, (se_7)^2, (se_8)^2, (se_9)^2, (se_10)^2)/imp_number, 
         v_total = v_w + v_b + v_b/imp_number, se_pooled = sqrt(v_total), 
         lci = th_adj - 1.96*se_pooled,
         uci = th_adj + 1.96*se_pooled, 
         month = as.Date(month, format = '%Y-%m-%d'))

fig_1b<- ggplot(fig1_pooled, aes(x = month, y = th_adj, color = Race.Ethnicity)) +
  geom_line() +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = Race.Ethnicity), 
              alpha = 0.2, color = NA) +
  labs(y = "Telehealth visits per 100 outpatient visits", x = "Visit date") +
  ggtitle("1b") +
  theme_bw()

#Figure of telehealth by rural/urban
fig_1c_dat <- sum_dat %>%
  group_by(month, Location) %>%
  summarize(percent = 100*weighted.mean(telehealth, weight), 
            lci = percent - 1.96*sqrt(sum(weight^2)*percent*(100-percent)), 
            uci = percent + 1.96*sqrt(sum(weight^2)*percent*(100-percent)))
fig_1c<- ggplot(fig_1c_dat, aes(x = month, y = percent, color = Location)) +
  geom_line() +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = Location), color = NA, alpha = 0.2) +
  scale_color_manual(values=c("#bae4b3", "#238b45")) + 
  scale_fill_manual(values=c("#bae4b3", "#238b45")) +
  labs(y = "Telehealth visits per 100 outpatient visits", x = "Visit date") +
  ggtitle("1c") +
  theme_bw()
#Figure by poverty
fig_1d_dat <- sum_dat %>%
  group_by(month, Poverty) %>%
  summarize(percent = 100*weighted.mean(telehealth, weight), 
            lci = percent - 1.96*sqrt(sum(weight^2)*percent*(100-percent)), 
            uci = percent + 1.96*sqrt(sum(weight^2)*percent*(100-percent)))
fig_1d<- ggplot(fig_1d_dat, aes(x = month, y = percent, color = Poverty)) +
  geom_line() +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = Poverty), color = NA, alpha = 0.2) +
  scale_color_manual(values=c("#6a51a3", "#cbc9e2")) + 
  scale_fill_manual(values=c("#6a51a3", "#cbc9e2")) +
  labs(y = "Telehealth visits per 100 outpatient visits", x = "Visit date") +
  ggtitle("1d") +
  theme_bw()
#Figure by size
fig_1e_dat <- sum_dat %>%
  group_by(month, Size) %>%
  summarize(percent = 100*weighted.mean(telehealth, weight), 
            lci = percent - 1.96*sqrt(sum(weight^2)*percent*(100-percent)), 
            uci = percent + 1.96*sqrt(sum(weight^2)*percent*(100-percent)))
fig_1e<- ggplot(fig_1e_dat, aes(x = month, y = percent, color = Size)) +
  geom_line() +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = Size), color = NA, alpha = 0.2) +
  scale_color_manual(values=c("#cb181d", "#fcae91")) + 
  scale_fill_manual(values=c("#cb181d", "#fcae91")) +
  labs(y = "Telehealth visits per 100 outpatient visits", x = "Visit date") +
  ggtitle("1e") +
  theme_bw()

#Quintiles of size
fig_size_dat <- sum_dat %>%
  group_by(month, size_det) %>%
  summarize(percent = 100*weighted.mean(telehealth, weight), 
            lci = percent - 1.96*sqrt(sum(weight^2)*percent*(100-percent)), 
            uci = percent + 1.96*sqrt(sum(weight^2)*percent*(100-percent)))
fig_size_dat$size_det <- factor(fig_size_dat$size_det, levels=c(1:5), 
                                labels=c("<344", "344-904", "904-1,818", "1,818-4,308", "4,308-23,267"))
ggplot(fig_size_dat, aes(x = month, y = percent, color = size_det)) +
  geom_line() +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = size_det), 
              alpha = 0.2, color = NA) +
  labs(y = "Telehealth visits per 100 outpatient visits", x = "Visit date") +
  theme_bw()

#Combine figures into one
ggarrange(fig_1a, 
          ggarrange(fig_1b, fig_1c, nrow = 2, align = "h", widths = c(1.5,2)), 
          ggarrange(fig_1d, fig_1e, nrow = 2, align = "h", widths = c(1.5,2)),
          ncol = 3, heights = c(1.5, 1, 1))

#Figure by region
fig_appendix <- sum_dat %>%
  group_by(month, Region) %>%
  summarize(percent = 100*weighted.mean(telehealth, weight), 
            lci = percent - 1.96*sqrt(sum(weight^2)*percent*(100-percent)), 
            uci = percent + 1.96*sqrt(sum(weight^2)*percent*(100-percent)))
ggplot(fig_appendix, aes(x = month, y = percent, color = Region)) +
  geom_line() +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = Region), 
              alpha = 0.2, color = NA) +
  labs(y = "Telehealth visits per 100 visits", x = "Procedure date") +
  theme_bw()
ggsave("D:/Users/anna_gage/Documents/Healthjump analysis/Plots/Fig_appendix_regions.png")

#Figure of under 65
fig_u65 <- pivot_longer(sum_dat, cols = c("u65_telehealth", "telehealth"), names_to = "Age", values_to = "telehealth") %>%
  group_by(month, Age) %>%
  summarize(percent = 100*weighted.mean(telehealth, weight, na.rm = TRUE), 
            lci = percent - 1.96*sqrt(sum(weight^2)*percent*(100-percent)), 
            uci = percent + 1.96*sqrt(sum(weight^2)*percent*(100-percent))) %>%
  mutate(Age = ifelse(Age=="telehealth", "All", "Under 65"))
ggplot(fig_u65, aes(x = month, y = percent, color = Age)) +
  geom_line() +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = Age), 
              alpha = 0.2, color = NA) +
  labs(y = "Telehealth visits per 100 outpatient visits", x = "Visit date") +
  theme_bw()

#Figure of mental health care
fig_mentalhealth <- subset(sum_dat, count_mental>0) %>%
  pivot_longer(cols = c("mental_telehealth", "telehealth"), names_to = "Type", values_to = "telehealth") %>%
  group_by(month, Type) %>%
  summarize(percent = 100*weighted.mean(telehealth, weight, na.rm = TRUE), 
            lci = percent - 1.96*sqrt(sum(weight^2)*percent*(100-percent)), 
            uci = percent + 1.96*sqrt(sum(weight^2)*percent*(100-percent))) %>%
  mutate(Type = ifelse(Type=="telehealth", "All", "Mental health care"))
ggplot(fig_mentalhealth, aes(x = month, y = percent, color = Type)) +
  geom_line() +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = Type), 
              alpha = 0.2, color = NA) +
  labs(y = "Telehealth visits per 100 outpatient visits", x = "Visit date") +
  theme_bw()

#Figure of telehealth use by parity status (unadjusted)
# sum_dat <- read.csv("D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_client_imp.csv")
# sum_dat <- subset(sum_dat, imp==1) %>%
#   mutate(weight = mpatients/sum(mpatients), 
#          month = as.Date(month, format = '%Y-%m-%d'))
# fig2_sum <- sum_dat %>%
#   group_by(month, parity_group) %>%
#   summarize(percent = weighted.mean(telehealth, weight), 
#             lci = percent - 1.96*sqrt(sum(weight^2)*percent*(100-percent)), 
#             uci = percent + 1.96*sqrt(sum(weight^2)*percent*(100-percent)))
# fig2_sum <- fig2_sum %>%
#   mutate(State = ifelse(parity_month==TRUE, "Mandated parity", "Parity not mandated"), 
#          month = as.Date(month, format = '%Y-%m-%d'))
# 
# ggplot(fig2_sum, aes(x = month, y = percent, color = parity_group)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = lci, ymax = uci, fill = parity_group), 
#               alpha = 0.2, color = NA) +
#   labs(y = "Telehealth visits per 100 visits", x = "Procedure date", color = "Parity status", fill = "Parity status") +
#   theme_bw()


####2FE for pre-pandemic states####
sum_client_imp<- read.csv("D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_client_imp.csv")
sum_client_imp$month <- as.Date(sum_client_imp$month, format = '%Y-%m-%d')

#Group and subset to the two groups
sum_subset <- subset(sum_client_imp, parity_group!="Parity from April 2020" & month<"2020-07-01") %>%
  mutate(parity = ifelse(parity_group=="Parity before 2019" | state=="GA", TRUE, FALSE), 
         quarter = as.factor(quarter(month)+ (year(month)-2019)*4), 
         telehealth = telehealth*100) %>%
  group_by(CLIENT_ID, quarter, imp) %>%
  summarize(across(c(telehealth, mpatients, pct_below18, pct_65plus, wnh, bnh, hispanic, change_patients), ~mean(.x)), 
            across(c(parity, PCT_RURAL_2010, POVERTYRATE), ~first(.x))) %>%
  mutate(lpatients = log(mpatients))


imp_results <- data.frame()
for(i in 1:10) {
  sum_subset_imp <- subset(sum_subset, imp== i)
  model <- lm(telehealth ~ parity*quarter + lpatients + pct_below18 + pct_65plus + wnh + bnh + hispanic + change_patients +
                factor(CLIENT_ID), data= sum_subset_imp)
  m_coeffs<- coef(summary(model))[, 1]
  m_se <- coef(summary(model))[, 2]
  m_df <- data.frame(coef = m_coeffs, se = m_se)
  m_df <- cbind(predictor = rownames(m_df), m_df)
  rownames(m_df) <- 1:nrow(m_df)
  m_df<- subset(m_df, grepl("parityTRUE", predictor)) %>%
    separate(predictor, c("parityTRUE", "quarter"), ":") %>%
    mutate(quarter = as.numeric(ifelse(is.na(quarter), 1, gsub("quarter", "", quarter))), 
           imp = i) %>%
    subset(quarter>1)
  imp_results <- rbind(imp_results, m_df)
}

results_pool <- pivot_wider(imp_results, id_cols = c(quarter), names_from = imp, values_from = c(coef, se))
imp_list <- paste0("coef_", seq(1:10))
results_pool$est_adj <- rowMeans(results_pool[, imp_list])
imp_number<-10 
results_pool <- results_pool %>%
  rowwise() %>%
  mutate(v_b = sum((coef_1-est_adj)^2, (coef_2-est_adj)^2, (coef_3-est_adj)^2, (coef_4-est_adj)^2, 
                   (coef_5-est_adj)^2, (coef_6-est_adj)^2, (coef_7-est_adj)^2, (coef_8-est_adj)^2, 
                   (coef_9-est_adj)^2, (coef_10-est_adj)^2)/(imp_number-1),
         v_w = sum((se_1)^2, (se_2)^2, (se_3)^2, 
                   (se_4)^2, (se_5)^2, (se_6)^2, 
                   (se_7)^2, (se_8)^2, (se_9)^2, (se_10)^2)/imp_number, 
         v_total = v_w + v_b + v_b/imp_number, se_pooled = sqrt(v_total), 
         lci = est_adj - 1.96*se_pooled,
         uci = est_adj + 1.96*se_pooled)

results_pool$quarter <- factor(results_pool$quarter, levels=c(2:6), labels=c("Q2 2019", "Q3 2019", "Q4 2019", "Q1 2020", "Q2 2020"))

ggplot(results_pool, aes(x = quarter, y = est_adj)) +
  geom_point(size = 2)  +
  geom_errorbar(aes(ymin = lci, ymax = uci), width=.1) +
  labs(y = "Percent Telehealth", x = "Quarter", 
       title = "Effects of Parity in States with Mandated Parity \nPrior to Pandemic Compared to Control Group") +
  theme_bw()

####Panel match#####
sum_client_imp<- read.csv("D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_client_imp.csv")
sum_client_imp$month <- as.Date(sum_client_imp$month, format = '%Y-%m-%d')
#Reformat for imptuations
panel_match <- sum_client_imp %>%
  group_by(CLIENT_ID) %>%
  mutate(new_id = cur_group_id())
panel_match <- panel_match %>%
  mutate(telehealth = 100*telehealth, lpatients = log(mpatients), treatment = ifelse(parity_month == FALSE | run_month ==1, 0, 1), 
         run_month = as.integer(run_month), month_count = n()) %>%
  subset(parity_group!="Parity from April 2020")
imp_number<- 10
moderators <- c("rural", "small", "poverty", "more_wnh")
results <- data.frame()
#Loop through iterations and do PM on each
for(i in 1:imp_number) { 
  panel_match_imp <- as.data.frame(subset(panel_match, imp == i))
  pm<- PanelMatch(lag = 1, time.id = "run_month", unit.id = "new_id", treatment = "treatment",
                  refinement.method = "ps.weight", data = panel_match_imp, 
                  covs.formula = ~ lpatients +  index + pct_below18 + pct_65plus + wnh + bnh + hispanic + PCT_RURAL_2010 + POVERTYRATE + change_patients, 
                  lead = c(0, 3, 6, 9, 12), qoi = "att", outcome.var = "telehealth")
  pm.results<- PanelEstimate(sets = pm, data= panel_match_imp, number.iterations = 100)
  
  result_imp <- as.data.frame(summary(pm.results), verbose = F, bias.corrected = F)
  result_imp$imp <- i
  result_imp$month <- c(0, 3, 6, 9, 12)
  result_imp$moderator <-"overall"
  result_imp$level <- 0
  results<- rbind(results, result_imp)
  
  # for(moderator in moderators) {
  #   pm.est<- PanelEstimate(sets = pm, moderator = moderator, data= panel_match_imp, number.iterations = 100)
  #   result_a <- as.data.frame(summary(pm.est$`0`), verbose = F, bias.corrected = F)
  #   result_b <- as.data.frame(summary(pm.est$`1`), verbose = F, bias.corrected = F)
  #   result_a$level <- 0
  #   result_b$level <- 1
  #   data <- rbind(result_a, result_b)
  #   data$imp <- i
  #   data$month<- c(0, 3, 6, 9, 12, 0, 3, 6, 9, 12)
  #   data$moderator<- moderator
  #   results<- rbind(results, data)
  # }
}
results_pool <- pivot_wider(results, id_cols = c(month, moderator, level), names_from = imp, values_from = c(summary.estimate, summary.std.error))
imp_list <- paste0("summary.estimate_", seq(1:imp_number))
results_pool$est_adj <- rowMeans(results_pool[, imp_list])
results_pool <- results_pool %>%
  rowwise() %>%
  mutate(v_b = sum((summary.estimate_1-est_adj)^2, (summary.estimate_2-est_adj)^2, (summary.estimate_3-est_adj)^2, (summary.estimate_4-est_adj)^2, 
                   (summary.estimate_5-est_adj)^2, (summary.estimate_6-est_adj)^2, (summary.estimate_7-est_adj)^2, (summary.estimate_8-est_adj)^2, 
                   (summary.estimate_9-est_adj)^2, (summary.estimate_10-est_adj)^2)/(imp_number-1),
         v_w = sum((summary.std.error_1)^2, (summary.std.error_2)^2, (summary.std.error_3)^2, 
                   (summary.std.error_4)^2, (summary.std.error_5)^2, (summary.std.error_6)^2, 
                   (summary.std.error_7)^2, (summary.std.error_8)^2, (summary.std.error_9)^2, (summary.std.error_10)^2)/imp_number, 
         v_total = v_w + v_b + v_b/imp_number, se_pooled = sqrt(v_total), 
         lci = est_adj - 1.96*se_pooled,
         uci = est_adj + 1.96*se_pooled)
ggplot(subset(results_pool, moderator == "overall"), aes(x = month, y = est_adj)) +
  geom_point(size = 2)  +
  geom_errorbar(aes(ymin = lci, ymax = uci), width=.1) +
  geom_hline(yintercept=0, linetype = "dashed") +
  labs(y = "Percent Telehealth", x = "Months since parity mandate", 
       title = "") +
  scale_x_continuous(breaks = seq(0,12,3)) +
  theme(aspect.ratio = 1) +
  theme_bw()

DisplayTreatment(unit.id = "new_id", time.id = "run_month", treatment = "treatment", data = panel_match_imp, dense.plot = TRUE, 
                 hide.y.tick.label = TRUE, hide.x.tick.label = TRUE, ylab = "Network", xlab = "Month", 
                 color.of.treated = "#F8766D", color.of.untreated = "#619CFF") 

#balance_scatter(pm, data= panel_match_imp, covariates = c("mpatients", "wnh", "bnh", "pct_65plus", "pct_below18", "PCT_RURAL_2010", "POVERTYRATE"))
#pm.results<- PanelEstimate(sets = pm, data= sum_client_1, number.iterations = 100)
#plot(pm.results, xlab = "Month", ylab = "Percent Telehealth")

####Treatment heterogeneity####

#Race/ethnicity
plot_data<- subset(results_pool, moderator=="more_wnh") %>%
  mutate(Race.Ethnicity = ifelse(level==0, 
                                 "Networks with \nmore racial \n& ethnic minority \npatients", 
                                 "Networks with \nat least 75% \nWhite Non-Hispanic \npatients"))
fig_4b<- ggplot(plot_data, aes(x = month, y = est_adj, color = Race.Ethnicity)) +
  geom_point(position = position_dodge(0.3), size = 2)  +
  geom_errorbar(aes(ymin = lci, ymax = uci), width=.1, position = position_dodge(0.3)) +
  geom_hline(yintercept=0, linetype = "dashed") +
  labs(y = "Percent Telehealth", x = "Months since parity mandate", 
       title = "4b") +
  scale_x_continuous(breaks = seq(0,12,3)) +
  theme_bw()

#Rural/Urban

plot_data<- subset(results_pool, moderator=="rural") %>%
  mutate(Location = ifelse(level==0, "Urban", "Rural \n(Zip3 over \n25% rural)"))
fig_4c<- ggplot(plot_data, aes(x = month, y = est_adj, color = Location)) +
  geom_point(position = position_dodge(0.3), size = 2)  +
  geom_errorbar(aes(ymin = lci, ymax = uci), width=.1, position = position_dodge(0.3)) +
  geom_hline(yintercept=0, linetype = "dashed") +
  labs(y = "Percent Telehealth", x = "Months since parity mandate", 
       title = "4c") +
  scale_x_continuous(breaks = seq(0,12,3)) +
  theme_bw()

#Poverty

plot_data<- subset(results_pool, moderator=="poverty") %>%
  mutate(Poverty = ifelse(level==0, "Less poverty", "More poverty \n(Over 15% of Zip \nunder poverty \nthreshold)"))
fig_4d<- ggplot(plot_data, aes(x = month, y = est_adj, color = Poverty)) +
  geom_point(position = position_dodge(0.3), size = 2)  +
  geom_errorbar(aes(ymin = lci, ymax = uci), width=.1, position = position_dodge(0.3)) +
  geom_hline(yintercept=0, linetype = "dashed") +
  labs(y = "Percent Telehealth", x = "Months since parity mandate", 
       title = "4d") +
  scale_x_continuous(breaks = seq(0,12,3)) +
  theme_bw()

#Size
plot_data<- subset(results_pool, moderator=="small") %>%
  mutate(Size = ifelse(level==0, "1300 or more \npatients per \nmonth", "Under 1300 \npatients per \nmonth"))
fig_4e<- ggplot(plot_data, aes(x = month, y = est_adj, color = Size)) +
  geom_point(position = position_dodge(0.3), size = 2)  +
  geom_errorbar(aes(ymin = lci, ymax = uci), width=.1, position = position_dodge(0.3)) +
  geom_hline(yintercept=0, linetype = "dashed") +
  labs(y = "Percent Telehealth", x = "Months since parity mandate", 
       title = "4e") +
  scale_x_continuous(breaks = seq(0,12,3)) +
  theme_bw()


#Combine plot#
ggarrange(fig_4a, fig_4b, fig_4c, fig_4d, fig_4e, 
          ncol = 3, nrow = 2)

####Under 65 and mental health care####
sum_client_imp<- read.csv("D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_client_imp.csv")
sum_client_imp$month <- as.Date(sum_client_imp$month, format = '%Y-%m-%d')
#Reformat for imptuations
panel_match <- sum_client_imp %>%
  group_by(CLIENT_ID) %>%
  mutate(new_id = cur_group_id())
panel_match <- panel_match %>%
  mutate(u65_telehealth = 100*u65_telehealth, mental_telehealth = 100*mental_telehealth, lpatients = log(mpatients), 
         treatment = ifelse(parity_month == FALSE, 0, 1), 
         run_month = as.integer(run_month), month_count = n()) 
imp_number<- 10
outcomes<- c("u65_telehealth", "mental_telehealth")
results <- data.frame()
#Loop through iterations and do PM on each
for(i in 1:imp_number) {
  for(j in 1:length(outcomes)) {
    outcome<- outcomes[j]
    panel_match_imp <- as.data.frame(subset(panel_match, imp == i))
    pm<- PanelMatch(lag = 1, time.id = "run_month", unit.id = "new_id", treatment = "treatment",
                    refinement.method = "ps.weight", data = panel_match_imp, 
                    covs.formula = ~ lpatients + pct_below18 + pct_65plus + wnh + bnh + hispanic + PCT_RURAL_2010 + POVERTYRATE + change_patients, 
                    lead = c(0, 3, 6, 9, 12), qoi = "att", outcome.var = outcome)
    pm.results<- PanelEstimate(sets = pm, data= panel_match_imp, number.iterations = 100)
    
    result_imp <- as.data.frame(summary(pm.results), verbose = F, bias.corrected = F)
    result_imp$imp <- i
    result_imp$month <- c(0, 3, 6, 9, 12)
    result_imp$outcome <- outcome
    results<- rbind(results, result_imp)
  }
}
results_pool <- pivot_wider(results, id_cols = c(month, outcome), names_from = imp, values_from = c(summary.estimate, summary.std.error))
imp_list <- paste0("summary.estimate_", seq(1:imp_number))
results_pool$est_adj <- rowMeans(results_pool[, imp_list])
results_pool <- results_pool %>%
  rowwise() %>%
  mutate(v_b = sum((summary.estimate_1-est_adj)^2, (summary.estimate_2-est_adj)^2, (summary.estimate_3-est_adj)^2, (summary.estimate_4-est_adj)^2, 
                   (summary.estimate_5-est_adj)^2, (summary.estimate_6-est_adj)^2, (summary.estimate_7-est_adj)^2, (summary.estimate_8-est_adj)^2, 
                   (summary.estimate_9-est_adj)^2, (summary.estimate_10-est_adj)^2)/(imp_number-1),
         v_w = sum((summary.std.error_1)^2, (summary.std.error_2)^2, (summary.std.error_3)^2, 
                   (summary.std.error_4)^2, (summary.std.error_5)^2, (summary.std.error_6)^2, 
                   (summary.std.error_7)^2, (summary.std.error_8)^2, (summary.std.error_9)^2, (summary.std.error_10)^2)/imp_number, 
         v_total = v_w + v_b + v_b/imp_number, se_pooled = sqrt(v_total), 
         lci = est_adj - 1.96*se_pooled,
         uci = est_adj + 1.96*se_pooled)
fig_u65<- ggplot(subset(results_pool, outcome == "u65_telehealth"), aes(x = month, y = est_adj)) +
  geom_point(size = 2)  +
  geom_errorbar(aes(ymin = lci, ymax = uci), width=.1) +
  geom_hline(yintercept=0, linetype = "dashed") +
  labs(y = "Percent Telehealth", x = "Months since parity mandate", 
       title = "Effect among patients under 65") +
  scale_x_continuous(breaks = seq(0,12,3)) +
  theme(aspect.ratio = 1) +
  theme_bw()
fig_mental<- ggplot(subset(results_pool, outcome == "mental_telehealth"), aes(x = month, y = est_adj)) +
  geom_point(size = 2)  +
  geom_errorbar(aes(ymin = lci, ymax = uci), width=.1) +
  geom_hline(yintercept=0, linetype = "dashed") +
  labs(y = "Percent Telehealth", x = "Months since parity mandate", 
       title = "Effect among mental health care") +
  scale_x_continuous(breaks = seq(0,12,3)) +
  theme(aspect.ratio = 1) +
  theme_bw()

####State level####
sum_client_imp<- read.csv("D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_client_imp.csv")
sum_client_imp$month <- as.Date(sum_client_imp$month, format = '%Y-%m-%d')

#Group and subset
sum_subset <- sum_client_imp %>%
  mutate(parity_group = ifelse(state=="GA", "Parity from April 2020", parity_group), 
         quarter = as.factor(quarter(month)+ (year(month)-2019)*4), 
         telehealth = telehealth*100, parity_combined = ifelse(parity_group=="Never", 0, 1)) %>%
  group_by(state, quarter, imp) %>%
  summarize(across(c(telehealth,pct_below18, pct_65plus, wnh, bnh, hispanic, change_patients, index, PCT_RURAL_2010, POVERTYRATE), ~weighted.mean(.x, mpatients, na.rm = TRUE)), 
            across(c(parity_group, parity_combined), ~first(.x)), 
            lpatients = log(sum(mpatients, na.rm = TRUE))) %>%
  subset(parity_group!="Parity another date")

imp_results <- data.frame()
for(i in 1:10) {
  sum_subset_imp <- subset(sum_subset, imp== i)
  model <- lm(telehealth ~ parity_group*quarter + lpatients + pct_below18 + pct_65plus + wnh + bnh + hispanic + change_patients +
                index + factor(state), data= sum_subset_imp)
  m_coeffs<- coef(summary(model))[, 1]
  m_se <- coef(summary(model))[, 2]
  m_df <- data.frame(coef = m_coeffs, se = m_se)
  m_df <- cbind(predictor = rownames(m_df), m_df)
  rownames(m_df) <- 1:nrow(m_df)
  m_df<- subset(m_df, grepl("parity_group", predictor)) %>%
    separate(predictor, c("parity_group", "quarter"), ":") %>%
    mutate(quarter = as.numeric(ifelse(is.na(quarter), 1, gsub("quarter", "", quarter))), 
           imp = i, parity_group = gsub("parity_group", "", parity_group)) %>%
    subset(quarter>1)
  imp_results <- rbind(imp_results, m_df)
}

for(i in 1:10) {
  sum_subset_imp <- subset(sum_subset, imp== i)
  model <- lm(telehealth ~ parity_combined*quarter + lpatients + pct_below18 + pct_65plus + wnh + bnh + hispanic + change_patients +
                index + factor(state), data= sum_subset_imp)
  m_coeffs<- coef(summary(model))[, 1]
  m_se <- coef(summary(model))[, 2]
  m_df <- data.frame(coef = m_coeffs, se = m_se)
  m_df <- cbind(predictor = rownames(m_df), m_df)
  rownames(m_df) <- 1:nrow(m_df)
  m_df<- subset(m_df, grepl("parity_combined", predictor)) %>%
    separate(predictor, c("parity_combined", "quarter"), ":") %>%
    mutate(quarter = as.numeric(ifelse(is.na(quarter), 1, gsub("quarter", "", quarter))), 
           imp = i, parity_group = "Parity at or before April 2020") %>%
    subset(quarter>1) %>%
    select(-parity_combined)
  m_df <- m_df[,c(5,1,2,3,4)]
  imp_results <- rbind(imp_results, m_df)
}

results_pool <- pivot_wider(imp_results, id_cols = c(quarter, parity_group), names_from = imp, values_from = c(coef, se))
imp_list <- paste0("coef_", seq(1:10))
results_pool$est_adj <- rowMeans(results_pool[, imp_list])
imp_number<-10 
results_pool <- results_pool %>%
  rowwise() %>%
  mutate(v_b = sum((coef_1-est_adj)^2, (coef_2-est_adj)^2, (coef_3-est_adj)^2, (coef_4-est_adj)^2, 
                   (coef_5-est_adj)^2, (coef_6-est_adj)^2, (coef_7-est_adj)^2, (coef_8-est_adj)^2, 
                   (coef_9-est_adj)^2, (coef_10-est_adj)^2)/(imp_number-1),
         v_w = sum((se_1)^2, (se_2)^2, (se_3)^2, 
                   (se_4)^2, (se_5)^2, (se_6)^2, 
                   (se_7)^2, (se_8)^2, (se_9)^2, (se_10)^2)/imp_number, 
         v_total = v_w + v_b + v_b/imp_number, se_pooled = sqrt(v_total), 
         lci = est_adj - 1.96*se_pooled,
         uci = est_adj + 1.96*se_pooled)

results_pool$quarter <- factor(results_pool$quarter, levels=c(2:17), 
                               labels=c("Q2 2019", "Q3 2019", "Q4 2019", "Q1 2020", 
                                        "Q2 2020", "Q3 2020", "Q4 2020", "Q1 2021",
                                        "Q2 2021", "Q3 2021", "Q4 2021", "Q1 2022",
                                        "Q2 2022", "Q3 2022", "Q4 2022", "Q1 2023"))
results_pool$Parity.Mandate <- results_pool$parity_group

ggplot(results_pool, aes(x = quarter, y = est_adj, color = Parity.Mandate)) +
  geom_point(size = 2, position = position_dodge(0.5))  +
  geom_errorbar(aes(ymin = lci, ymax = uci), width=.1, position = position_dodge(0.5)) +
  labs(y = "Percent Telehealth", x = "Quarter", 
       title = "Association of telehealth parity mandates with state level telemedicine use") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 70, hjust=1))


####2 Way FE sensitivity####
sum_client_imp<- read.csv("D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_client_imp.csv")
sum_client_imp$month <- as.Date(sum_client_imp$month, format = '%Y-%m-%d')

#Group and subset
sum_subset <- sum_client_imp %>%
  mutate(parity_group = ifelse(state=="GA", "Parity from April 2020", parity_group), 
         quarter = as.factor(quarter(month)+ (year(month)-2019)*4), 
         telehealth = telehealth*100, parity_combined = ifelse(parity_group=="Never", 0, 1)) %>%
  group_by(CLIENT_ID, quarter, imp) %>%
  summarize(across(c(telehealth, mpatients, pct_below18, pct_65plus, wnh, bnh, hispanic, change_patients, index), ~mean(.x)), 
            across(c(parity_group, parity_combined, PCT_RURAL_2010, POVERTYRATE), ~first(.x))) %>%
  mutate(lpatients = log(mpatients)) %>%
  subset(parity_group!="Parity another date")


imp_results <- data.frame()
for(i in 1:10) {
  sum_subset_imp <- subset(sum_subset, imp== i)
  model <- lm(telehealth ~ parity_group*quarter + lpatients + pct_below18 + pct_65plus + wnh + bnh + hispanic + change_patients +
                index + factor(CLIENT_ID), data= sum_subset_imp)
  m_coeffs<- coef(summary(model))[, 1]
  m_se <- coef(summary(model))[, 2]
  m_df <- data.frame(coef = m_coeffs, se = m_se)
  m_df <- cbind(predictor = rownames(m_df), m_df)
  rownames(m_df) <- 1:nrow(m_df)
  m_df<- subset(m_df, grepl("parity_group", predictor)) %>%
    separate(predictor, c("parity_group", "quarter"), ":") %>%
    mutate(quarter = as.numeric(ifelse(is.na(quarter), 1, gsub("quarter", "", quarter))), 
           imp = i, parity_group = gsub("parity_group", "", parity_group)) %>%
    subset(quarter>1)
  imp_results <- rbind(imp_results, m_df)
}

for(i in 1:10) {
  sum_subset_imp <- subset(sum_subset, imp== i)
  model <- lm(telehealth ~ parity_combined*quarter + lpatients + pct_below18 + pct_65plus + wnh + bnh + hispanic + change_patients +
                index + factor(CLIENT_ID), data= sum_subset_imp)
  m_coeffs<- coef(summary(model))[, 1]
  m_se <- coef(summary(model))[, 2]
  m_df <- data.frame(coef = m_coeffs, se = m_se)
  m_df <- cbind(predictor = rownames(m_df), m_df)
  rownames(m_df) <- 1:nrow(m_df)
  m_df<- subset(m_df, grepl("parity_combined", predictor)) %>%
    separate(predictor, c("parity_combined", "quarter"), ":") %>%
    mutate(quarter = as.numeric(ifelse(is.na(quarter), 1, gsub("quarter", "", quarter))), 
           imp = i, parity_group = "Parity at or \nbefore April 2020") %>%
    subset(quarter>1) %>%
    select(-parity_combined)
  m_df <- m_df[,c(5,1,2,3,4)]
  imp_results <- rbind(imp_results, m_df)
}

results_pool <- pivot_wider(imp_results, id_cols = c(quarter, parity_group), names_from = imp, values_from = c(coef, se))
imp_list <- paste0("coef_", seq(1:10))
results_pool$est_adj <- rowMeans(results_pool[, imp_list])
imp_number<-10 
results_pool <- results_pool %>%
  rowwise() %>%
  mutate(v_b = sum((coef_1-est_adj)^2, (coef_2-est_adj)^2, (coef_3-est_adj)^2, (coef_4-est_adj)^2, 
                   (coef_5-est_adj)^2, (coef_6-est_adj)^2, (coef_7-est_adj)^2, (coef_8-est_adj)^2, 
                   (coef_9-est_adj)^2, (coef_10-est_adj)^2)/(imp_number-1),
         v_w = sum((se_1)^2, (se_2)^2, (se_3)^2, 
                   (se_4)^2, (se_5)^2, (se_6)^2, 
                   (se_7)^2, (se_8)^2, (se_9)^2, (se_10)^2)/imp_number, 
         v_total = v_w + v_b + v_b/imp_number, se_pooled = sqrt(v_total), 
         lci = est_adj - 1.96*se_pooled,
         uci = est_adj + 1.96*se_pooled)

results_pool$quarter <- factor(results_pool$quarter, levels=c(2:17), 
                               labels=c("Q2 2019", "Q3 2019", "Q4 2019", "Q1 2020", 
                                        "Q2 2020", "Q3 2020", "Q4 2020", "Q1 2021",
                                        "Q2 2021", "Q3 2021", "Q4 2021", "Q1 2022",
                                        "Q2 2022", "Q3 2022", "Q4 2022", "Q1 2023"))
results_pool$Parity.Mandate <- results_pool$parity_group

fig_4a <- ggplot(subset(results_pool, Parity.Mandate=="Parity at or \nbefore April 2020"), aes(x = quarter, y = est_adj)) +
  geom_point(size = 2)  +
  geom_errorbar(aes(ymin = lci, ymax = uci), width=.1) +
  labs(y = "Percent Telehealth", x = "Quarter", 
       title = "4a") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 70, hjust=1))

ggplot(results_pool, aes(x = quarter, y = est_adj, color = Parity.Mandate)) +
  geom_point(size = 2, position = position_dodge(0.5))  +
  geom_errorbar(aes(ymin = lci, ymax = uci), width=.1, position = position_dodge(0.5)) +
  labs(y = "Percent Telehealth", x = "Quarter", 
       title = "Association of parity mandates with telemedicine use by parity start date") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 70, hjust=1))

####2 Way FE sensitivity under 65####
sum_client_imp<- read.csv("D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_client_imp.csv")
sum_client_imp$month <- as.Date(sum_client_imp$month, format = '%Y-%m-%d')

#Group and subset
sum_subset <- sum_client_imp %>%
  mutate(parity_group = ifelse(state=="GA", "Parity from April 2020", parity_group), 
         quarter = as.factor(quarter(month)+ (year(month)-2019)*4), 
         telehealth = u65_telehealth*100, parity_combined = ifelse(parity_group=="Never", 0, 1)) %>%
  group_by(CLIENT_ID, quarter, imp) %>%
  summarize(across(c(telehealth, mpatients, pct_below18, pct_65plus, wnh, bnh, hispanic, change_patients, index), ~mean(.x)), 
            across(c(parity_group, parity_combined, PCT_RURAL_2010, POVERTYRATE), ~first(.x))) %>%
  mutate(lpatients = log(mpatients)) %>%
  subset(parity_group!="Parity another date")

imp_results <- data.frame()
for(i in 1:10) {
  sum_subset_imp <- subset(sum_subset, imp== i)
  model <- lm(telehealth ~ parity_group*quarter + lpatients + pct_below18 + pct_65plus + wnh + bnh + hispanic + change_patients +
                index + factor(CLIENT_ID), data= sum_subset_imp)
  m_coeffs<- coef(summary(model))[, 1]
  m_se <- coef(summary(model))[, 2]
  m_df <- data.frame(coef = m_coeffs, se = m_se)
  m_df <- cbind(predictor = rownames(m_df), m_df)
  rownames(m_df) <- 1:nrow(m_df)
  m_df<- subset(m_df, grepl("parity_group", predictor)) %>%
    separate(predictor, c("parity_group", "quarter"), ":") %>%
    mutate(quarter = as.numeric(ifelse(is.na(quarter), 1, gsub("quarter", "", quarter))), 
           imp = i, parity_group = gsub("parity_group", "", parity_group)) %>%
    subset(quarter>1)
  imp_results <- rbind(imp_results, m_df)
}

for(i in 1:10) {
  sum_subset_imp <- subset(sum_subset, imp== i)
  model <- lm(telehealth ~ parity_combined*quarter + lpatients + pct_below18 + pct_65plus + wnh + bnh + hispanic + change_patients +
                index + factor(CLIENT_ID), data= sum_subset_imp)
  m_coeffs<- coef(summary(model))[, 1]
  m_se <- coef(summary(model))[, 2]
  m_df <- data.frame(coef = m_coeffs, se = m_se)
  m_df <- cbind(predictor = rownames(m_df), m_df)
  rownames(m_df) <- 1:nrow(m_df)
  m_df<- subset(m_df, grepl("parity_combined", predictor)) %>%
    separate(predictor, c("parity_combined", "quarter"), ":") %>%
    mutate(quarter = as.numeric(ifelse(is.na(quarter), 1, gsub("quarter", "", quarter))), 
           imp = i, parity_group = "Parity at or before April 2020") %>%
    subset(quarter>1) %>%
    select(-parity_combined)
  m_df <- m_df[,c(5,1,2,3,4)]
  imp_results <- rbind(imp_results, m_df)
}

results_pool <- pivot_wider(imp_results, id_cols = c(quarter, parity_group), names_from = imp, values_from = c(coef, se))
imp_list <- paste0("coef_", seq(1:10))
results_pool$est_adj <- rowMeans(results_pool[, imp_list])
imp_number<-10 
results_pool <- results_pool %>%
  rowwise() %>%
  mutate(v_b = sum((coef_1-est_adj)^2, (coef_2-est_adj)^2, (coef_3-est_adj)^2, (coef_4-est_adj)^2, 
                   (coef_5-est_adj)^2, (coef_6-est_adj)^2, (coef_7-est_adj)^2, (coef_8-est_adj)^2, 
                   (coef_9-est_adj)^2, (coef_10-est_adj)^2)/(imp_number-1),
         v_w = sum((se_1)^2, (se_2)^2, (se_3)^2, 
                   (se_4)^2, (se_5)^2, (se_6)^2, 
                   (se_7)^2, (se_8)^2, (se_9)^2, (se_10)^2)/imp_number, 
         v_total = v_w + v_b + v_b/imp_number, se_pooled = sqrt(v_total), 
         lci = est_adj - 1.96*se_pooled,
         uci = est_adj + 1.96*se_pooled)

results_pool$quarter <- factor(results_pool$quarter, levels=c(2:17), 
                               labels=c("Q2 2019", "Q3 2019", "Q4 2019", "Q1 2020", 
                                        "Q2 2020", "Q3 2020", "Q4 2020", "Q1 2021",
                                        "Q2 2021", "Q3 2021", "Q4 2021", "Q1 2022",
                                        "Q2 2022", "Q3 2022", "Q4 2022", "Q1 2023"))
results_pool$Parity.Mandate <- results_pool$parity_group

ggplot(results_pool, aes(x = quarter, y = est_adj, color = Parity.Mandate)) +
  geom_point(size = 2, position = position_dodge(0.5))  +
  geom_errorbar(aes(ymin = lci, ymax = uci), width=.1, position = position_dodge(0.5)) +
  labs(y = "Percent Telehealth", x = "Quarter", 
       title = "Association of telehealth parity mandates with under 65 population telemedicine use") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 70, hjust=1))

####2 Way FE sensitivity treatment heterogeneity####
sum_client_imp<- read.csv("D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_client_imp.csv")
sum_client_imp$month <- as.Date(sum_client_imp$month, format = '%Y-%m-%d')

#Group and subset
sum_subset <- sum_client_imp %>%
  mutate(parity_group = ifelse(state=="GA", "Parity from April 2020", parity_group),
         quarter = as.factor(quarter(month)+ (year(month)-2019)*4), 
         telehealth = telehealth*100, parity_combined = ifelse(parity_group=="Never", 0, 1)) %>%
  group_by(CLIENT_ID, quarter, imp) %>%
  summarize(across(c(telehealth, mpatients, pct_below18, pct_65plus, wnh, bnh, hispanic, change_patients,
                     rural, small, poverty, more_wnh, index), ~mean(.x)), 
            across(c(parity_group, parity_combined, PCT_RURAL_2010, POVERTYRATE), ~first(.x))) %>%
  mutate(lpatients = log(mpatients)) %>%
  subset(parity_group!="Parity another date")


moderators <- c("rural", "small", "poverty", "more_wnh")
imp_results <- data.frame()

for (moderator in moderators) {
  for(i in 1:10) {
    sum_subset_imp <- subset(sum_subset, imp== i)
    formula <- as.formula(paste("telehealth ~ parity_combined*quarter*", moderator, " + lpatients + pct_below18 + pct_65plus + wnh + bnh + hispanic + change_patients + index + factor(CLIENT_ID)"))
    model <- lm(formula, data= sum_subset_imp)
    m_coeffs<- coef(summary(model))[, 1]
    m_se <- coef(summary(model))[, 2]
    m_df <- data.frame(
      group = moderator, 
      coef = m_coeffs, se = m_se)
    m_df <- cbind(predictor = rownames(m_df), m_df)
    rownames(m_df) <- 1:nrow(m_df)
    m_df<- subset(m_df, grepl("parity_combined", predictor)) %>%
      mutate(value = ifelse(grepl(moderator, predictor), 1, 0)) %>%
      separate(predictor, c("parity_combined", "quarter"), ":") %>%
      mutate(quarter = as.numeric(ifelse(is.na(quarter), 1, gsub("quarter", "", quarter))), 
             imp = i) %>%
      subset(quarter>1) 
    imp_results <- rbind(imp_results, m_df)
  }
}

results_pool <- pivot_wider(imp_results, id_cols = c(quarter, group, value), names_from = imp, values_from = c(coef, se))
imp_list <- paste0("coef_", seq(1:10))
results_pool$est_adj <- rowMeans(results_pool[, imp_list])
imp_number<-10 
results_pool <- results_pool %>%
  rowwise() %>%
  mutate(v_b = sum((coef_1-est_adj)^2, (coef_2-est_adj)^2, (coef_3-est_adj)^2, (coef_4-est_adj)^2, 
                   (coef_5-est_adj)^2, (coef_6-est_adj)^2, (coef_7-est_adj)^2, (coef_8-est_adj)^2, 
                   (coef_9-est_adj)^2, (coef_10-est_adj)^2)/(imp_number-1),
         v_w = sum((se_1)^2, (se_2)^2, (se_3)^2, 
                   (se_4)^2, (se_5)^2, (se_6)^2, 
                   (se_7)^2, (se_8)^2, (se_9)^2, (se_10)^2)/imp_number, 
         v_total = v_w + v_b + v_b/imp_number, se_pooled = sqrt(v_total), 
         lci = est_adj - 1.96*se_pooled,
         uci = est_adj + 1.96*se_pooled)

results_pool$quarter <- factor(results_pool$quarter, levels=c(2:17), 
                               labels=c("Q2 2019", "Q3 2019", "", "Q1 2020", 
                                        "", "Q3 2020", "", "Q1 2021",
                                        "", "Q3 2021", "", "Q1 2022",
                                        "", "Q3 2022", "", "Q1 2023"))
results_pool <- results_pool %>%
  mutate(modifier = case_when(group == "rural" & value == 1 ~ "Rural \n(Zip3 over \n25% rural)", 
                              group == "rural" & value == 0 ~ "Urban", 
                              group == "small" & value == 1 ~ "Under 1300 \npatients per \nmonth",
                              group == "small" & value == 0 ~ "1300 or more \npatients per \nmonth",
                              group == "poverty" & value == 1 ~ "More poverty \n(Over 15% of Zip \nunder poverty \nthreshold)",
                              group == "poverty" & value == 0 ~ "Less poverty",
                              group == "more_wnh" & value == 1 ~ "Network >75% \nWhite \nNon-Hispanic \npatients",
                              group == "more_wnh" & value == 0 ~ "Networks more \nracial & \nethnic minority \npatients"))

results_pool$Location <- results_pool$modifier
results_pool$Race.Ethnicity <- results_pool$modifier
results_pool$Poverty <- results_pool$modifier
results_pool$Size <- results_pool$modifier

fig_4b<- ggplot(subset(results_pool, group=="more_wnh"), aes(x = quarter, y = est_adj, color = Race.Ethnicity)) +
  geom_point(size = 2, position = position_dodge(0.5))  +
  geom_errorbar(aes(ymin = lci, ymax = uci), width=.1, position = position_dodge(0.5)) +
  scale_color_manual(values=c("#2171b5", "#bdd7e7")) +
  labs(y = "Percent Telehealth", x = "Quarter", 
       title = "4b") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 70, hjust=1))
fig_4c<- ggplot(subset(results_pool, group=="rural"), aes(x = quarter, y = est_adj, color = Location)) +
  geom_point(size = 2, position = position_dodge(0.5))  +
  geom_errorbar(aes(ymin = lci, ymax = uci), width=.1, position = position_dodge(0.5)) +
  scale_color_manual(values=c("#bae4b3", "#238b45")) + 
  labs(y = "Percent Telehealth", x = "Quarter", 
       title = "4c") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 70, hjust=1))
fig_4d<- ggplot(subset(results_pool, group=="poverty"), aes(x = quarter, y = est_adj, color = Poverty)) +
  geom_point(size = 2, position = position_dodge(0.5))  +
  geom_errorbar(aes(ymin = lci, ymax = uci), width=.1, position = position_dodge(0.5)) +
  scale_color_manual(values=c("#6a51a3", "#cbc9e2")) + 
  labs(y = "Percent Telehealth", x = "Quarter", 
       title = "4d") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 70, hjust=1))
fig_4e<- ggplot(subset(results_pool, group=="small"), aes(x = quarter, y = est_adj, color = Size)) +
  geom_point(size = 2, position = position_dodge(0.5))  +
  geom_errorbar(aes(ymin = lci, ymax = uci), width=.1, position = position_dodge(0.5)) +
  labs(y = "Percent Telehealth", x = "Quarter", title = "4e") +
  scale_color_manual(values=c("#cb181d", "#fcae91")) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 70, hjust=1))

#Combine plot#
ggarrange(fig_4a, 
          ggarrange(fig_4b, fig_4c, nrow = 2, align = "h", widths = c(1.5,2)), 
          ggarrange(fig_4d, fig_4e, nrow = 2, align = "h", widths = c(1.5,2)),
          ncol = 3, heights = c(1.5, 1, 1))

####Unadjusted figure of 'other' states####
sum_dat <- read.csv("D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_client_imp.csv")
sum_dat_group <- subset(sum_dat, imp==1) %>%
  mutate(weight = mpatients/sum(mpatients), 
         month = as.Date(month, format = '%Y-%m-%d'), 
         state_group = ifelse(parity_group =="Parity another date", state, parity_group)) %>%
  group_by(state_group, month) %>%
  summarize(percent = 100*weighted.mean(telehealth, weight), 
            lci = percent - 1.96*sqrt(sum(weight^2)*percent*(100-percent)), 
            uci = percent + 1.96*sqrt(sum(weight^2)*percent*(100-percent))) %>%
  subset(state_group!="Never")
ggplot(sum_dat_group, aes(x = month, y = percent, color = state_group)) +
  geom_line() +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = state_group), 
              alpha = 0.2, color = NA) +
  labs(y = "Telehealth visits per 100 outpatient visits", x = "Visit date") +
  theme_bw()+
  geom_vline(xintercept = as.numeric(as.Date(c("2020-01-01", "2021-07-01", "2022-04-01", "2022-01-01", "2021-06-01", "2020-04-01", "2020-08-01"))), linetype = 4, 
             color = c("#F8766D", "#CD9600", "#7CAE00", "#00BE67", "#00BFC4", "#C77CFF", "#FF61CC"))

#Other state adjustment
sum_client_imp<- read.csv("D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_client_imp.csv")
sum_client_imp$month <- as.Date(sum_client_imp$month, format = '%Y-%m-%d')
sum_client_imp$parity_group <- ifelse(sum_client_imp$state=="NY", "Never", sum_client_imp$parity_group)
sum_client_imp$parity_group <- ifelse(sum_client_imp$state=="MD", "Never", sum_client_imp$parity_group)
sum_client_imp$parity_group <- ifelse(sum_client_imp$state=="OK", "Never", sum_client_imp$parity_group)
sum_client_imp$parity_group <- ifelse(sum_client_imp$state=="OR", "Never", sum_client_imp$parity_group)
sum_client_imp$parity_group <- ifelse(sum_client_imp$state=="GA", "Parity from April 2020", sum_client_imp$parity_group)

#Group and subset
sum_subset <- subset(sum_client_imp, parity_group!="Parity another date" & month<"2021-06-01") %>%
  mutate(quarter = as.factor(quarter(month)+ (year(month)-2019)*4), 
         telehealth = telehealth*100, parity_combined = ifelse(parity_group=="Never", 0, 1)) %>%
  group_by(CLIENT_ID, quarter, imp) %>%
  summarize(across(c(telehealth, mpatients, pct_below18, pct_65plus, wnh, bnh, hispanic, change_patients, index), ~mean(.x)), 
            across(c(parity_group, parity_combined, PCT_RURAL_2010, POVERTYRATE), ~first(.x))) %>%
  mutate(lpatients = log(mpatients))


imp_results <- data.frame()
for(i in 1:10) {
  sum_subset_imp <- subset(sum_subset, imp== i)
  model <- lm(telehealth ~ parity_group*quarter + lpatients + pct_below18 + pct_65plus + wnh + bnh + hispanic + change_patients +
                index + factor(CLIENT_ID), data= sum_subset_imp)
  m_coeffs<- coef(summary(model))[, 1]
  m_se <- coef(summary(model))[, 2]
  m_df <- data.frame(coef = m_coeffs, se = m_se)
  m_df <- cbind(predictor = rownames(m_df), m_df)
  rownames(m_df) <- 1:nrow(m_df)
  m_df<- subset(m_df, grepl("parity_group", predictor)) %>%
    separate(predictor, c("parity_group", "quarter"), ":") %>%
    mutate(quarter = as.numeric(ifelse(is.na(quarter), 1, gsub("quarter", "", quarter))), 
           imp = i, parity_group = gsub("parity_group", "", parity_group)) %>%
    subset(quarter>1)
  imp_results <- rbind(imp_results, m_df)
}

for(i in 1:10) {
  sum_subset_imp <- subset(sum_subset, imp== i)
  model <- lm(telehealth ~ parity_combined*quarter + lpatients + pct_below18 + pct_65plus + wnh + bnh + hispanic + change_patients +
                index + factor(CLIENT_ID), data= sum_subset_imp)
  m_coeffs<- coef(summary(model))[, 1]
  m_se <- coef(summary(model))[, 2]
  m_df <- data.frame(coef = m_coeffs, se = m_se)
  m_df <- cbind(predictor = rownames(m_df), m_df)
  rownames(m_df) <- 1:nrow(m_df)
  m_df<- subset(m_df, grepl("parity_combined", predictor)) %>%
    separate(predictor, c("parity_combined", "quarter"), ":") %>%
    mutate(quarter = as.numeric(ifelse(is.na(quarter), 1, gsub("quarter", "", quarter))), 
           imp = i, parity_group = "Parity at or before April 2020") %>%
    subset(quarter>1) %>%
    select(-parity_combined)
  m_df <- m_df[,c(5,1,2,3,4)]
  imp_results <- rbind(imp_results, m_df)
}

results_pool <- pivot_wider(imp_results, id_cols = c(quarter, parity_group), names_from = imp, values_from = c(coef, se))
imp_list <- paste0("coef_", seq(1:10))
results_pool$est_adj <- rowMeans(results_pool[, imp_list])
imp_number<-10 
results_pool <- results_pool %>%
  rowwise() %>%
  mutate(v_b = sum((coef_1-est_adj)^2, (coef_2-est_adj)^2, (coef_3-est_adj)^2, (coef_4-est_adj)^2, 
                   (coef_5-est_adj)^2, (coef_6-est_adj)^2, (coef_7-est_adj)^2, (coef_8-est_adj)^2, 
                   (coef_9-est_adj)^2, (coef_10-est_adj)^2)/(imp_number-1),
         v_w = sum((se_1)^2, (se_2)^2, (se_3)^2, 
                   (se_4)^2, (se_5)^2, (se_6)^2, 
                   (se_7)^2, (se_8)^2, (se_9)^2, (se_10)^2)/imp_number, 
         v_total = v_w + v_b + v_b/imp_number, se_pooled = sqrt(v_total), 
         lci = est_adj - 1.96*se_pooled,
         uci = est_adj + 1.96*se_pooled)

results_pool$quarter <- factor(results_pool$quarter, levels=c(2:17), 
                               labels=c("Q2 2019", "Q3 2019", "Q4 2019", "Q1 2020", 
                                        "Q2 2020", "Q3 2020", "Q4 2020", "Q1 2021",
                                        "Q2 2021", "Q3 2021", "Q4 2021", "Q1 2022",
                                        "Q2 2022", "Q3 2022", "Q4 2022", "Q1 2023"))
results_pool$Parity.Mandate <- results_pool$parity_group

ggplot(results_pool, aes(x = quarter, y = est_adj, color = Parity.Mandate)) +
  geom_point(size = 2, position = position_dodge(0.5))  +
  geom_errorbar(aes(ymin = lci, ymax = uci), width=.1, position = position_dodge(0.5)) +
  labs(y = "Percent Telehealth", x = "Quarter", 
       title = "Associaiton of telehmedicine parity mandates with telemedicine use") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 70, hjust=1))
