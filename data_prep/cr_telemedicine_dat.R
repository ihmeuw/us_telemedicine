#####Create Telemedicine dataset####
##Anna Gage, annagage@uw.edu
##10.07.2023

#Script is meant to run within the Amazon Workspace environment provided by the Covid-19 research database, https://covid19researchdatabase.org/
#Queries will not work outside of this environment.

rm(list = ls())
library(RODBC)
library(data.table)
library(tidyr)
library(dplyr)
library(mice)
library(arrow)
library(lubridate)

#Connect to healthjump database
myconn <-odbcConnect("HJ_Source_Sch", uid = "anna_gage", pwd = )

#Facilities list
#Pull facility list
client_dem <- sqlQuery(myconn, "select * from
  htg_healthjump_prod.healthjump_00142.client_dem"
)
write.csv(client_dem,"D:/Users/anna_gage/Documents/Healthjump analysis/Data/client_dem.csv")
client_dem <- read.csv("D:/Users/anna_gage/Documents/Healthjump analysis/Data/client_dem.csv")

facilities_list <- strsplit(client_dem$CLIENT_ID, split = " , ")
#Split to stay within the memory limits
fac_half_1 <- facilities_list[c(1:175)]
fac_half_2 <- facilities_list[c(176:350)]
fac_half_3 <- facilities_list[c(351:498)]

facilities_list <- paste0("'", facilities_list, "'", collapse = ", ")
#Patient pull
#Patient inclusion criteria
# 1. Patient at one of the included networks
# 2. Has info in the demographic table
# 3. Has at least one visit to facility between 2019 and 2023 of any type
# 4. Not missing gender, birth year, or zipcode (after some filling in)

patient_pull<- sqlQuery(myconn, paste0("select
      dem.client_id, dem.client_patient_id, dem.race, dem.ethnicity,
      dem.zip3, left(dem.dob,4) as birth_year,
      dem.gender, dem.primary_language,
      count (distinct dem.client_id, dem.client_patient_id,  proc.date) as visit_count
    from
      covid19_prod.healthjump.demographic dem
    inner join
       covid19_prod.healthjump.procedure proc
        on dem.client_id = proc.client_id
        and dem.client_patient_id = proc.patient_id
    where
    (proc.date LIKE '2019%' OR proc.date LIKE '2020%' OR proc.date LIKE '2021%' OR proc.date LIKE '2022%' OR proc.date LIKE '2023%') and
    dem.client_id in (", facilities_list, ")
    group by
    dem.client_id, dem.client_patient_id, dem.race, dem.ethnicity, zip3, birth_year, gender, primary_language"))

write_feather(patient_pull,"D:/Users/anna_gage/Documents/Healthjump analysis/Data/patient_pull.arrow")
patient_pull <- read_feather("D:/Users/anna_gage/Documents/Healthjump analysis/Data/patient_pull.arrow")

#Some patients are duplicated without zips. Keep the one with zips
patient_pull <- patient_pull%>%
  arrange(ZIP3, .na_position = "last") %>%
  distinct(CLIENT_ID, CLIENT_PATIENT_ID, .keep_all = TRUE)

#Organize dem variables
patient_dem <- patient_pull %>%
  mutate(race= as.factor(case_when(RACE == "1002-5"  ~ "aian",
                                   (RACE == "2028-9" | RACE == "2076-8") ~ "aapi",
                                   RACE == "2054-5" ~ "black",
                                   RACE == "2106-3" ~ "white")),
         hispanic =as.factor(case_when(ETHNICITY == "H" ~ 1,
                                       ETHNICITY == "N" ~ 0)),
         gender = as.factor(GENDER), 
         lang_eng = PRIMARY_LANGUAGE %in% "eng",
         lang_spa = PRIMARY_LANGUAGE %in% "spa",
         lang_asi = PRIMARY_LANGUAGE %in% c("bod", "hin", "hmn", "ind", "jpn", "khm", "kor", "nep", "pan", "tgl", "tha", "urd", "vie", "zho"),
         zip3 = as.integer(ZIP3))
patient_dem <- select(patient_dem, -ZIP3)
patient_dem <- as.data.table(patient_dem)

#Address missing zips. First, re-include zips then impute as the network's 
# most common zip. If still zip is unavailable, drop patient
orig_zips <- read_feather("D:/Users/anna_gage/Documents/Healthjump analysis/Data/orig_zips.arrow")
orig_zips <- orig_zips %>%
  arrange(ZIP3, .na_position = "last") %>%
  distinct(CLIENT_ID, CLIENT_PATIENT_ID, .keep_all = TRUE)

patient_dem <- left_join(patient_dem, orig_zips) %>%
  mutate(zip3 = ifelse(is.na(zip3), as.integer(ZIP3), zip3))
patient_dem <- left_join(patient_dem, client_dem, by="CLIENT_ID") %>%
  mutate(zip3 = ifelse(is.na(zip3.x), zip3.y, zip3.x)) %>%
  select(-c(zip3.x, zip3.y))

#Drop missing zip, gender or birth year
patient_dem <- patient_dem[!is.na(zip3) & gender!="U" & !is.na(BIRTH_YEAR)]
patient_dem <-  select(patient_dem, -c(RACE, ETHNICITY, GENDER, PRIMARY_LANGUAGE, ZIP3, state, patients, X))
drop_patients <- length(patient_pull$CLIENT_ID)-length(patient_dem$CLIENT_ID)
rm(patient_pull, orig_zips)

gc()

#Inspect missingness
sapply(patient_dem, function(x) sum(is.na(x)))


#5. Set up imputation 
#prediction matrix: indicator variables aren't included in imputation

predM <- make.predictorMatrix(data = patient_dem)
predM[, c("zip3", "CLIENT_PATIENT_ID")] <-0

impMethod <- vector("character", 20)
impMethod[5] <- "polyreg"
impMethod[6] <- "logreg"

#6. Run mice
imp <- mice(patient_dem, method = impMethod, predictorMatrix = predM, maxit =1, m = 10)
gc()
completed <- complete(imp, 'long', include = FALSE)

rm(imp, patient_dem)
gc()

#7. Create combined race/ethnicity variable
completed <- select(completed, -c("PCTNONHISPANICWHITE", "PCTNONHISPANICBLACK", "PCTASIAN", "PCTHISPANIC", "PCTUNDER18", "PCT65PLUS",
                                  "POVERTYRATE", "PCT_RURAL_2010", "PCTOTHER"))

patient_dem <- patient_dem %>%
  mutate(re = (case_when(race == "aian" & hispanic == 0 ~ "aian",
                         race == "aapi" & hispanic == 0 ~ "aapi",
                         race =="black" & hispanic == 0 ~ "bnh", 
                         race =="white" & hispanic == 0 ~ "wnh",
                         hispanic == 1 ~ "hispanic")))

gc()
write_feather(completed, "D:/Users/anna_gage/Documents/Healthjump analysis/Data/completed.arrow")

rm(completed)

completed <- read_feather("D:/Users/anna_gage/Documents/Healthjump analysis/Data/completed.arrow")
completed <- as.data.table(completed)

#Summarize into telehealth by client/month/re
for(i in 1:10) {
  gc()
imp <- completed[.imp == i]
write_feather(imp, paste0("D:/Users/anna_gage/Documents/Healthjump analysis/Data/completed_", i, ".arrow"))
}

rm(completed, imp)

#####Pull in procedures####

#Pull inpatient codes
public_conn <-odbcConnect("public_data", uid = "anna_gage", pwd = "nQZH989zmSBgKN!")
inpt_codes<-sqlQuery(public_conn, "select * from public_data_prod.cms_inp.hcpcs_data")
setDT(inpt_codes)
inpt_codes<- unique(inpt_codes)
inpt_codes<- strsplit(inpt_codes$HCPCS_CODE, split = ",")
inpt_codes <- paste0("'", inpt_codes, c('99221', '99222', '99223', '99231', '99232', '99233', '99234', '99235', '99236', '99238', '99239', 'G0316', 
                                        '99281', '99282', '99283', '99284', '99285'), "'", collapse = ", ")

#Pull in mental health codes
share_conn <- odbcConnect("AG_out", uid = "anna_gage", pwd = "nQZH989zmSBgKN!")
mental_list <- sqlQuery(share_conn,  "select * from HTG_HEALTHJUMP_PROD.HEALTHJUMP_00142.MH_DX")
mental_list <- distinct(mental_list) %>%
  subset(ACAUSE !="ACAUSE")
mental_list<- mental_list$ICD_CODE
mental_list <- paste0("'", mental_list, "'", collapse = ", ")

proc_pull<- sqlQuery(myconn, paste0("select
      proc.client_id, patient_id, date, 
      max(cast(
        case when modifier_code1 in ('95', 'GT', '93', 'FQ', 'FR') OR modifier_code2 in ('95', 'GT', '93', 'FQ', 'FR')
                OR modifier_code3 in ('95', 'GT', '93', 'FQ', 'FR') OR modifier_code4 in ('95', 'GT', '93', 'FQ', 'FR')
                    then 1
                    else 0
               end as int)) as telehealth, 
      max(cast(
        case when dx1_code in (", mental_list, ")
                    then 1
                    else 0
               end as int)) as mentalhealth
    from
      covid19_prod.healthjump.procedure proc
    inner join
      covid19_prod.healthjump.demographic dem
      on dem.client_id = proc.client_id
      and dem.client_patient_id = proc.patient_id
    where
      (date LIKE '2019%' OR date LIKE '2020%' OR date LIKE '2021%' OR date LIKE '2022%' OR date LIKE '2023%')
      and proc.client_id in (", facilities_list, ")
      and proc.cpt4_hcpcs_code not in (", inpt_codes, ")
    group by
        proc.client_id, patient_id, date"))

gc()

proc_pull <- proc_pull %>%
  mutate(month = case_when(nchar(DATE)==8 ~ as.Date(paste0(substring(DATE, 1,6), '01'), format = '%Y%m%d'),
                           nchar(DATE)==10 ~ as.Date(paste0(substring(DATE, 1,8), '01'), format = '%Y-%m-%d')),
         year = year(month)) %>%
  select(-DATE)

write_feather(proc_pull, "D:/Users/anna_gage/Documents/Healthjump analysis/Data/proc_pull.arrow")
gc()

####Collapse dataset####

#Only race needs to go through all the imputations. Do that separately, then merge the other vars on the collapsed data set
proc_pull <- read_feather("D:/Users/anna_gage/Documents/Healthjump analysis/Data/proc_pull.arrow")

sum_re_imp<- data.frame()
for(i in 1:10) {
  imp <- read_feather(paste0("D:/Users/anna_gage/Documents/Healthjump analysis/Data/completed_", i, ".arrow")) 
  imp <- imp %>%
    distinct(CLIENT_ID, CLIENT_PATIENT_ID, .keep_all = TRUE)
  #imp <- subset(imp, CLIENT_ID %in% fac_half_1)
  #imp <- subset(imp, CLIENT_ID %in% fac_half_2)
  imp <- subset(imp, CLIENT_ID %in% fac_half_3)
  gc()
  imp <- left_join(imp, proc_pull, by=c("CLIENT_ID", "CLIENT_PATIENT_ID" = "PATIENT_ID")) 
  gc()
  sum_re_dat <- imp %>%
    group_by(CLIENT_ID, month, re) %>%
    summarize(telehealth = mean(TELEHEALTH), mpatients= n())
  #sum_re_dat$imp <- i
  sum_re_imp <- rbind(sum_re_imp, sum_re_dat)
  rm(imp, sum_re_dat)
  gc()
}
#write_feather(sum_re_imp, "D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_re_imp.arrow")

sum_re_imp_first <- read_feather("D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_re_imp.arrow")
sum_re_imp_third<- rbind(sum_re_imp_first, sum_re_imp)
write_feather(sum_re_imp_third, "D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_re_imp.arrow")


####Collapse other vars besides race/ethnicty####
proc_pull <- read_feather("D:/Users/anna_gage/Documents/Healthjump analysis/Data/proc_pull.arrow")
ind_full <- read_feather("D:/Users/anna_gage/Documents/Healthjump analysis/Data/completed_1.arrow") 
ind_full <- ind_full %>%
  distinct(CLIENT_ID, CLIENT_PATIENT_ID, .keep_all = TRUE)

sum_other_vars <- data.frame()
for(i in 1:3) {
  fac_split <- paste0("fac_half_", i)
  ind <- subset(ind_full, CLIENT_ID %in% get(fac_split))
  ind <- left_join(ind, proc_pull, by=c("CLIENT_ID", "CLIENT_PATIENT_ID" = "PATIENT_ID")) %>%
    mutate(below_18 = ifelse(year-BIRTH_YEAR<18, 1, 0), 
           above_65 = ifelse(year-BIRTH_YEAR>64, 1, 0), 
           female = ifelse(gender=="F", 1, 0), 
           u65_telehealth = ifelse(above_65==0, TELEHEALTH, NA), 
           mental_telehealth = ifelse(MENTALHEALTH ==1, TELEHEALTH, NA))
  sum_dat <- ind %>%
    group_by(CLIENT_ID, month) %>%
    summarize(telehealth = mean(TELEHEALTH), u65_telehealth = mean(u65_telehealth, na.rm=TRUE),
              mental_telehealth = mean(mental_telehealth, na.rm = TRUE),
              mpatients= n(), pct_female = mean(female),
              pct_below18 = mean(below_18), pct_65plus = mean(above_65), 
              count_u65 = mpatients-sum(above_65, na.rm=TRUE), 
              count_mental = sum(MENTALHEALTH, na.rm = TRUE))
  sum_other_vars <- rbind(sum_other_vars, sum_dat)
  rm(ind, sum_dat)
  gc()
}
write_feather(sum_other_vars, "D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_other_vars")

####Collapse from Client/month/re to Client/month, merge with other vars###
sum_re_imp <- read_feather("D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_re_imp.arrow")
prop_re <- sum_re_imp %>%
  group_by(CLIENT_ID, month, imp) %>%
  mutate(re_patients = mpatients/sum(mpatients, na.rm=TRUE)) %>%
  pivot_wider(id_cols = c(CLIENT_ID, month, imp), names_from = re, values_from = re_patients) %>%
  mutate(aian= ifelse(is.na(aian), 0, aian), 
         aapi= ifelse(is.na(aapi), 0, aapi), 
         bnh = ifelse(is.na(bnh), 0, bnh), 
         hispanic = ifelse(is.na(hispanic), 0, hispanic), 
         wnh = ifelse(is.na(wnh), 0, wnh))
sum_client_imp <- sum_re_imp %>%
  group_by(CLIENT_ID, month, imp) %>%
  summarize(telehealth = 100*weighted.mean(telehealth, mpatients),
            mpatients = sum(mpatients)) 
sum_client_imp <- left_join(sum_client_imp, prop_re) %>%
  left_join(client_dem) %>%
  select(-c(mpatients, telehealth)) %>%
  left_join(sum_other_vars)

####Add in the parity classifications and key modifiers####
parity_states<- c("AZ", "AR", "CA", "CO", "CT", "DE", "GA", "HI", "IL", "IA", "KY", "MD", "MA", "MN", "MT", "NV", "NH",
                  "NJ", "NM", "NY", "OK", "OR", "RI", "TN", "VT", "VA", "WA")
sum_client_imp <- sum_client_imp %>%
  subset(month<"2023-04-01") %>%
  mutate(period = as.factor(case_when(month < "2020-03-01" ~ 1, 
                                      month >= "2020-03-01" & month < "2020-07-01" ~ 2, 
                                      month >= "2020-07-01" ~ 3)), 
         run_month = interval(as.Date("2018-12-01"), month) %/% months(1),
         parity = ifelse(state %in% parity_states, TRUE, FALSE),
         parity_month = case_when(state == "GA" & run_month < 13 ~ FALSE,
                                  state %in% c("AZ", "CA", "CT", "IL", "IA", "MA", "MT", "NH", "RI", "VT", "WA") & run_month < 16 ~ FALSE, 
                                  state == "MT" & run_month > 25 ~ FALSE, 
                                  state == "TN" & (run_month < 20 | run_month > 39) ~ FALSE, 
                                  state == "OR" & run_month < 30 ~ FALSE,
                                  state == "MD" & run_month < 31 ~ FALSE, 
                                  state == "NV" & run_month < 34 ~ FALSE, 
                                  state == "OK" & run_month < 37 ~ FALSE, 
                                  state == "NY" & run_month < 41 ~ FALSE, 
                                  TRUE ~ parity), 
         parity_group = case_when(parity == FALSE ~ "Never", 
                                  state %in% c("AR", "CO", "DE", "HI", "KY", "MN", "NJ", "NM", "VA") ~ "Parity before 2019",
                                  state %in% c("AZ", "CA", "CT", "IL", "IA", "MA", "MT", "NH", "RI", "VT", "WA") ~ "Parity from April 2020", 
                                  state %in% c("GA", "TN", "OR", "MD", "NV", "OK", "NY") ~ "Parity another date"), 
         rural = ifelse(PCT_RURAL_2010>25, 1, 0), 
         poverty = ifelse(POVERTYRATE>15, 1, 0))%>%
  group_by(CLIENT_ID, imp) %>%
  mutate(small = ifelse(mean(mpatients)<1300, 1, 0), 
         more_wnh = ifelse(mean(wnh)>0.75, 1, 0))

#Re/month/imp level
sum_re_month_imp <- sum_re_imp %>%
  group_by(re, month, imp) %>%
  summarize(telehealth = 100*weighted.mean(telehealth, mpatients),
            mpatients = sum(mpatients),
            se = sqrt(telehealth*(100-telehealth)/sum(mpatients))) %>% 
  subset(month<"2023-04-01") %>%
  mutate(Race.Ethnicity = case_when(re =="aian" ~ "AI/AN", re =="aapi"~ "AAPI", 
                                    re =="bnh" ~ "Black", re =="wnh" ~ "White", re =="hispanic" ~ "Hispanic"))

write.csv(sum_re_month_imp, "D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_month_imp.csv")

#2019 characteristics
sum_2019_imp <- sum_client_imp %>%
  subset(month<"2020-01-01") %>%
  group_by(CLIENT_ID, imp) %>%
  summarize(across(c(telehealth:mental_telehealth, pct_female:pct_65plus, aapi:wnh), ~weighted.mean(.x, mpatients)), 
            across(c(state, zip3), ~first(.x)), 
            mpatients = sum(mpatients))

write.csv(sum_2019_imp, "D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_2019.csv")

sum_2019 <- select(sum_2019_imp, c(CLIENT_ID, imp, mpatients, wnh, bnh)) %>%
  rename(patients_2019 = mpatients, wnh_2019 = wnh, bnh_2019 = bnh)
sum_client_imp <- left_join(sum_client_imp, sum_2019) %>%
  mutate(change_patients = ifelse(month<"2020-01-01", 1, mpatients/(patients_2019/12)), 
         change_wnh = ifelse(month<"2020-01-01", 1, wnh/(wnh_2019)), 
         change_bnh = ifelse(month<"2020-01-01", 1, bnh/(bnh_2019)))
write.csv(sum_client_imp, "D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_client_imp.csv")  


####State level analysis####
sum_client_imp<- read.csv("D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_client_imp.csv") 

state_sum <- sum_client_imp %>%
  group_by(month, state, imp) %>%
  summarize(across(c(telehealth, pct_below18, pct_65plus, pct_female, aapi, aian, bnh, wnh, hispanic,
                     PCT_RURAL_2010, POVERTYRATE, change_patients, small, poverty, rural), ~weighted.mean(.x, mpatients)),
            across(c(parity, parity_group, run_month, parity_month), ~first(.x)), 
            mpatients = sum(mpatients))
write.csv(state_sum, "D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_state_imp.csv")  

#State mandates
#From https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(23)00461-0/fulltext#supplementaryMaterial
state_mandates <- data.frame(unique(state_sum$state)) %>%
  rename(state = unique.state_sum.state.) %>%
  mutate(index = case_when(state == "AL" ~ 0.33, 
                           state == "AR" ~ 0.39,
                           state == "AZ" ~ 0.70,
                           state == "CA" ~ 0.95,
                           state == "CO" ~ 0.65,
                           state == "CT" ~ 0.73, 
                           state == "DE" ~ 0.55,
                           state == "FL" ~ 0.22,
                           state == "GA" ~ 0.29,
                           state == "IA" ~ 0.24,
                           state == "ID" ~ 0.27, 
                           state == "IL" ~ 0.81,
                           state == "IN" ~ 0.41,
                           state == "KS" ~ 0.24,
                           state == "KY" ~ 0.54,
                           state == "LA" ~ 0.48, 
                           state == "MA" ~ 0.75,
                           state == "MD" ~ 0.48,
                           state == "ME" ~ 0.71,
                           state == "MI" ~ 0.73,
                           state == "MO" ~ 0.15,
                           state == "MN" ~ 0.61, 
                           state == "MS" ~ 0.39,
                           state == "MT" ~ 0.28,
                           state == "NC" ~ 0.67,
                           state == "NE" ~ 0.20,
                           state == "NH" ~ 0.41, 
                           state == "NJ" ~ 0.52,
                           state == "NM" ~ 0.86,
                           state == "NY" ~ 0.63,
                           state == "OH" ~ 0.51,
                           state == "OK" ~ 0.08, 
                           state == "OR" ~ 0.83,
                           state == "PA" ~ 0.57,
                           state == "RI" ~ 0.62,
                           state == "SC" ~ 0.29,
                           state == "TN" ~ 0.25, 
                           state == "TX" ~ 0.47,
                           state == "UT" ~ 0.35,
                           state == "VA" ~ 0.52,
                           state == "WA" ~ 1.01,
                           state == "WV" ~ 0.51, 
                           state == "WY" ~ 0.38,
                           state == "PR" ~ 0.50))

write.csv(state_mandates, "D:/Users/anna_gage/Documents/Healthjump analysis/Data/ihme_mandate_stringency.csv")

sum_client_imp <- left_join(sum_client_imp, state_mandates) %>%
  mutate(index = ifelse(as.Date(month) < "2020-03-01", 0 , index))
write.csv(sum_client_imp, "D:/Users/anna_gage/Documents/Healthjump analysis/Data/sum_client_imp.csv")  
