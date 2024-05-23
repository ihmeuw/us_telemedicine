################################################################################
## Author: Megan Knight                                                       ##
## Date: 10/24/2023                                                           ##
## Purpose: Pull counts of patient-days for in-person visits by cause         ##
## grouping for visualizations.                                               ##
################################################################################
## libraries 
pacman::p_load(RODBC, data.table, stringr, ggplot2)

## snowflake permissions
hj_data <- odbcConnect ("hj_data", uid="megan_knight", pwd="R@ngerD@nger4$")
icd_dx_map_out <- odbcConnect ("icd_dx_map_out", uid="megan_knight", pwd="R@ngerD@nger4$")

################################################################################
## Prep dx codes                                                              ##
## Cite: https://jamanetwork.com/journals/jama/article-abstract/2774396       ##
################################################################################
## read in dx codes
icd_dx <- sqlQuery(icd_dx_map_out, "SELECT * FROM PUBLIC_DATA_PROD.ICD_CODES.ICD_MAP")
setDT(icd_dx)

## read in dx map 
icd_causelist <- sqlQuery(hj_data, "SELECT * FROM PUBLIC_DATA_PROD.CAUSELIST.CAUSELIST")
setDT(icd_causelist)

## drop duplicates that were included in the upload 
icd_dx <- unique(icd_dx)

## merge tables 
icd_dx <- merge(icd_causelist, icd_dx, by = 'ACAUSE')

## disaggregate risk factors 
icd_dx[FAMILY_NAME %like% 'Risk factors', FAMILY_NAME := CAUSE_NAME]

## update mapping for COVID-19
icd_dx[ICD_NAME %like% 'Coronavirus' | ICD_NAME %like% 'coronavirus' | ICD_NAME %like% 'COVID-19', c('ACAUSE', 'CAUSE_NAME', 'FAMILY_NAME') := list('lri_corona','COVID-19', 'COVID-19')]

## add covdes from Health Jump
hj_covid_codes <- data.table(ACAUSE = rep('lri_corona', 5),
                             CAUSE_NAME = rep('COVID-19', 5),
                             FAMILY = rep('fam_ri', 5),
                             FAMILY_NAME = rep('COVID-19', 5),
                             GBD_LEVEL_USED = rep(3, 5),
                             GBD_CAUSE_AGG = rep('', 5), 
                             CODE_SYSTEM = rep('icd10', 5), 
                             ICD_CODE = c('U07100','U07101','U0710','U07103','U07104'),
                             ICD_NAME = rep('ICD found empirically in data from ... HJ', 5))
icd_dx <- rbind(icd_dx, hj_covid_codes)

################################################################################
## Pull telemedicine and outpatient visits for sample                         ##
################################################################################
## pull facilities in sample from Anna
## TODO: confirm with Anna
facilities <- sqlQuery(hj_data, "SELECT CLIENT_ID 
         FROM HTG_HEALTHJUMP_PROD.HEALTHJUMP_00142.CLIENT_DEM")

## prep facilities for sql querying
facilities <- as.data.frame(facilities[-1,])
setnames(facilities, old = 'facilities[-1, ]', new = 'CLIENT_ID')
facilities <- strsplit(facilities$CLIENT_ID, split = " , " )
facilities <- paste0("'", facilities, "'", collapse = ", ")

## pull inpatient codes from CMS
inpt_codes <- sqlQuery(hj_data, "SELECT *
                     FROM PUBLIC_DATA_PROD.CMS_INP.HCPCS_DATA")

## prep inpateint codes for sql querying 
setDT(inpt_codes)
inpt_codes <- unique(inpt_codes)
inpt_codes <- strsplit(inpt_codes$HCPCS_CODE, split = " , ")
inpt_codes <- paste0("'", paste0(inpt_codes,  c('99221', '99222', '99223','99231','99232','99233','99234','99235','99236','99238','99239', 'G0316', '99281', '99282', '99283', '99284', '99285')), "'", collapse = ", ")

ptdays_by_cause <- function(date){
  ## query by month so my rstudio won't crash
  df <- sqlQuery(hj_data, paste0("SELECT A.DX1_CODE, A.PATIENT_ID, A.DATE, B.GENDER, B.DOB
                                  FROM COVID19_PROD.HEALTHJUMP.PROCEDURE A
                             LEFT JOIN COVID19_PROD.HEALTHJUMP.DEMOGRAPHIC B
                                    ON A.client_id = B.client_id AND 
                                       A.patient_id = B.client_patient_id 
               WHERE A.DATE >= '", date, "01' AND A.DATE <= '", date, "31' 
               AND A.CLIENT_ID IN (",  facilities, ")
                 AND (A.PLACE_OF_SERVICE != '2' OR 
                      A.MODIFIER_CODE1 != 'GT' OR 
                      A.MODIFIER_CODE1 != '95' OR 
                      A.MODIFIER_CODE1 != '93' OR
                      A.MODIFIER_CODE1 != 'FQ' OR 
                      A.MODIFIER_CODE1 != 'FR' OR 
                      
                      A.MODIFIER_CODE2 != 'GT' OR 
                      A.MODIFIER_CODE2 != '95' OR 
                      A.MODIFIER_CODE2 != '93' OR
                      A.MODIFIER_CODE2 != 'FQ' OR 
                      A.MODIFIER_CODE2 != 'FR' OR 
                      
                      A.MODIFIER_CODE3 != 'GT' OR 
                      A.MODIFIER_CODE3 != '95' OR 
                      A.MODIFIER_CODE3 != '93' OR
                      A.MODIFIER_CODE3 != 'FQ' OR 
                      A.MODIFIER_CODE3 != 'FR' OR 
                      
                      A.MODIFIER_CODE4 != 'GT' OR 
                      A.MODIFIER_CODE4 != '95' OR 
                      A.MODIFIER_CODE4 != '93' OR
                      A.MODIFIER_CODE4 != 'FQ' OR 
                      A.MODIFIER_CODE4 != 'FR')
                      AND A.CPT4_HCPCS_CODE NOT IN (", inpt_codes, ")"))
  setDT(df)
  
  
  ## clean date
  df[, DATE := str_remove_all(DATE, '-')]
  ## nrow(df[ DATE %in% c(2019, 2020, 2021, 2022, 2023)])/nrow(df) (0.8%)
  df <- df[!(DATE %in% c(2019, 2020, 2021, 2022, 2023))] ## TODO: sensitivity analysis?
  
  ## create PATIENT_DATE indicator
  df[, PATIENT_DATE := paste0(PATIENT_ID, DATE)]
  
  ## list of PATIENT_DATES that contain a missing dx
  miss_ptdays <- unique(df[DX1_CODE == '[REDACTED]' | is.na(DX1_CODE), c('PATIENT_DATE')])
  
  ## weight PATIENT_DATE by cause evenly where there is no missing 
  df_nomiss <- df[!(PATIENT_DATE %in% miss_ptdays$PATIENT_DATE)]
  df_nomiss[, weight := 1/.N, by = c('PATIENT_ID', 'DATE')]
  
  ## re-weight PATIENT_DATE by cause dropping missing where other dx is avaiable 
  df_miss <- df[PATIENT_DATE %in% miss_ptdays$PATIENT_DATE]
  df_miss_rewt <- df_miss[!is.na(DX1_CODE) & DX1_CODE != '[REDACTED]',]
  df_miss_rewt[, weight := 1/.N, by = c('PATIENT_ID', 'DATE')]
  
  ## weight PATIENT_DATE by cause evenly where all dx is missing 
  df_miss <- df_miss[!(PATIENT_DATE %in% df_miss_rewt$PATIENT_DATE)]
  df_miss[, weight := 1/.N, by = c('PATIENT_ID', 'DATE')]
  
  ## combine weighted causes for 
  df_collapsed <- rbind(df_nomiss, df_miss_rewt, df_miss)
  df_collapsed[, PLOT_DATE := as.Date(paste0(substr(DATE, 1, 4), '-', substr(DATE, 5, 6), '-', '01'))]
  
  df_collapsed[, DX1_CODE := gsub(".","", DX1_CODE, fixed = T)]
  df_collapsed[, DX1_CODE := gsub(" ","", DX1_CODE, fixed = T)]
  df_collapsed[, DX1_CODE := toupper(DX1_CODE)]
  
  df_collapsed <- merge(df_collapsed, icd_dx[CODE_SYSTEM == 'icd10', c('ACAUSE', 'CAUSE_NAME', 'FAMILY', 'FAMILY_NAME', 'ICD_NAME', 'ICD_CODE')], by.x = 'DX1_CODE', by.y = 'ICD_CODE', all.x = T)
  setnames(df_collapsed, old = c('ACAUSE', 'CAUSE_NAME', 'FAMILY', 'FAMILY_NAME', 'ICD_NAME'), new = c('ACAUSE_icd10', 'CAUSE_NAME_icd10', 'FAMILY_icd10', 'FAMILY_NAME_icd10', 'ICD_NAME_icd10'))
  df_collapsed <-merge(df_collapsed, icd_dx[CODE_SYSTEM == 'icd9', c('ACAUSE', 'CAUSE_NAME', 'FAMILY', 'FAMILY_NAME', 'ICD_NAME', 'ICD_CODE')], by.x = 'DX1_CODE', by.y = 'ICD_CODE', all.x = T)
  setnames(df_collapsed, old = c('ACAUSE', 'CAUSE_NAME', 'FAMILY', 'FAMILY_NAME', 'ICD_NAME'), new = c('ACAUSE_icd9', 'CAUSE_NAME_icd9', 'FAMILY_icd9', 'FAMILY_NAME_icd9', 'ICD_NAME_icd9'))
  
  ## rely on information from ICD10 rather than ICD9 unless there is missingness or garbage coding 
  df_collapsed[, ACAUSE := ifelse(is.na(ACAUSE_icd10) & !is.na(ACAUSE_icd9), ACAUSE_icd9,
                                    ifelse(ACAUSE_icd10 == '_gc' & !is.na(ACAUSE_icd9) & ACAUSE_icd9 != '_gc', ACAUSE_icd9, ACAUSE_icd10))]
  df_collapsed[, CAUSE_NAME := ifelse(is.na(CAUSE_NAME_icd10) & !is.na(CAUSE_NAME_icd9), CAUSE_NAME_icd9, 
                                        ifelse(CAUSE_NAME_icd10 == '_gc' & !is.na(CAUSE_NAME_icd9) & CAUSE_NAME_icd9 != '_gc', CAUSE_NAME_icd9, CAUSE_NAME_icd10))]
  df_collapsed[, FAMILY := ifelse(is.na(FAMILY_icd10) & !is.na(FAMILY_icd9), FAMILY_icd9, 
                                    ifelse(FAMILY_icd10 == '_gc' & !is.na(FAMILY_icd9) & FAMILY_icd9 != '_gc', FAMILY_icd9, FAMILY_icd10))]
  df_collapsed[, FAMILY_NAME := ifelse(is.na(FAMILY_NAME_icd10) & !is.na(FAMILY_NAME_icd9), FAMILY_NAME_icd9, 
                                         ifelse(FAMILY_NAME_icd10 == '_gc' & !is.na(FAMILY_NAME_icd9) & FAMILY_NAME_icd9 != '_gc', FAMILY_NAME_icd9, FAMILY_NAME_icd10))]
  df_collapsed[, ICD_NAME := ifelse(is.na(ICD_NAME_icd10) & !is.na(ICD_NAME_icd9), ICD_NAME_icd9, 
                                      ifelse(ICD_NAME_icd10 == '_gc' & !is.na(ICD_NAME_icd9) & ICD_NAME_icd9 != '_gc', ICD_NAME_icd9, ICD_NAME_icd10))]
  
  ## aggregate counts of patient-days by date and cause
  df_plot <- df_collapsed[, .(weight = sum(weight)), by = c('FAMILY_NAME', 'PLOT_DATE')]
  df_plot <- df_plot[, .(weight = weight, FAMILY_NAME = FAMILY_NAME, prop = weight/sum(weight)), by = c('PLOT_DATE')]
  
  df_plot[, FAMILY_NAME := ifelse(is.na(FAMILY_NAME), "Unmapped", FAMILY_NAME)]
  
  df_plot[, FAMILY_NAME := factor(FAMILY_NAME, levels = c("Neonatal disorders", 
                                                            "Neglected tropical diseases and malaria", 
                                                            "HIV/AIDS and sexually transmitted infections", 
                                                            "Enteric infections", 
                                                            "Other infectious diseases",
                                                            "Nutritional deficiencies", "Maternal disorders", "Respiratory infections and tuberculosis", "COVID-19", "Congenital birth defects", "Hemoglobinopathies and hemolytic anemias", "Substance use disorders",
                                                            "Kidney diseases", "Oral disorders", "Urinary diseases and male infertility", "Neoplasms", "Gynecological diseases", "Endocrine, metabolic, blood, and immune disorders", "Sense organ diseases", 
                                                            "Digestive diseases", "Skin and subcutaneous diseases", "Cardiovascular diseases", "Neurological disorders", "Diabetes", "Chronic respiratory diseases", "Musculoskeletal disorders", "Mental disorders", 
                                                            "Injuries", "Tobacco intervention", "Treatment of obesity", "Treatment of hyperlipidemia", "Well causes", "Treatment of hypertension", "Unmapped", "Garbage code"))]
  return(df_plot)
}

## run prep code by month
jan2019 <- ptdays_by_cause(date='201901')
feb2019 <- ptdays_by_cause(date='201902')
mar2019 <- ptdays_by_cause(date='201903')
apr2019 <- ptdays_by_cause(date='201904')
may2019 <- ptdays_by_cause(date='201905')
jun2019 <- ptdays_by_cause(date='201906')
jul2019 <- ptdays_by_cause(date='201907')
aug2019 <- ptdays_by_cause(date='201908')
sep2019 <- ptdays_by_cause(date='201909')
oct2019 <- ptdays_by_cause(date='201910')
nov2019 <- ptdays_by_cause(date='201911')
dec2019 <- ptdays_by_cause(date='201912')

jan2020 <- ptdays_by_cause(date='202001')
feb2020 <- ptdays_by_cause(date='202002')
mar2020 <- ptdays_by_cause(date='202003')
apr2020 <- ptdays_by_cause(date='202004')
may2020 <- ptdays_by_cause(date='202005')
jun2020 <- ptdays_by_cause(date='202006')
jul2020 <- ptdays_by_cause(date='202007')
aug2020 <- ptdays_by_cause(date='202008')
sep2020 <- ptdays_by_cause(date='202009')
oct2020 <- ptdays_by_cause(date='202010')
nov2020 <- ptdays_by_cause(date='202011')
dec2020 <- ptdays_by_cause(date='202012')

jan2021 <- ptdays_by_cause(date='202101')
feb2021 <- ptdays_by_cause(date='202102')
mar2021 <- ptdays_by_cause(date='202103')
apr2021 <- ptdays_by_cause(date='202104')
may2021 <- ptdays_by_cause(date='202105')
jun2021 <- ptdays_by_cause(date='202106')
jul2021 <- ptdays_by_cause(date='202107')
aug2021 <- ptdays_by_cause(date='202108')
sep2021 <- ptdays_by_cause(date='202109')
oct2021 <- ptdays_by_cause(date='202110')
nov2021 <- ptdays_by_cause(date='202111')
dec2021 <- ptdays_by_cause(date='202112')

jan2022 <- ptdays_by_cause(date='202201')
feb2022 <- ptdays_by_cause(date='202202')
mar2022 <- ptdays_by_cause(date='202203')
apr2022 <- ptdays_by_cause(date='202204')
may2022 <- ptdays_by_cause(date='202205')
jun2022 <- ptdays_by_cause(date='202206')
jul2022 <- ptdays_by_cause(date='202207')
aug2022 <- ptdays_by_cause(date='202208')
sep2022 <- ptdays_by_cause(date='202209')
oct2022 <- ptdays_by_cause(date='202210')
nov2022 <- ptdays_by_cause(date='202211')
dec2022 <- ptdays_by_cause(date='202212')

jan2023 <- ptdays_by_cause(date='202301')
feb2023 <- ptdays_by_cause(date='202302')
mar2023 <- ptdays_by_cause(date='202303')

## combine all months 
outp <- rbind(jan2019, feb2019, mar2019, apr2019, may2019, jun2019, jul2019, aug2019, sep2019, oct2019, nov2019, dec2019, 
      jan2020, feb2020, mar2020, apr2020, may2020, jun2020, jul2020, aug2020, sep2020, oct2020, nov2020, dec2020,
      jan2021, feb2021, mar2021, apr2021, may2021, jun2021, jul2021, aug2021, sep2021, oct2021, nov2021, dec2021,
      jan2022, feb2022, mar2022, apr2022, may2022, jun2022, jul2022, aug2022, sep2022, oct2022, nov2022, dec2022,
      jan2023, feb2023, mar2023)


## export 
write.csv(outp, 'D:/Users/knight_megan/Documents/results/data/outpatient_ptdays_cause.csv',  row.names = F)
