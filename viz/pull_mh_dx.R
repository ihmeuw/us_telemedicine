################################################################################
## Author: Megan Knight                                                       ##
## Purpose: Save mental health codes so they are accessible for Anna.         ##
################################################################################
## packages
## install.packages('RODBC')
## install.packages('pacman')

## libraries 
pacman::p_load(RODBC, data.table)

## snowflake permissions
hj_data <- odbcConnect ("hj_data", uid="megan_knight", pwd="R@ngerD@nger4$")
icd_dx_map_out <- odbcConnect ("icd_dx_map_out", uid="megan_knight", pwd="R@ngerD@nger4$")
mk_out <- odbcConnect ("mk_out", uid="megan_knight", pwd="R@ngerD@nger4$")

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

## subset to mental health causes 
icd_dx_ment <- icd_dx[FAMILY == 'fam_mental']

## export to public location
sqlSave(mk_out, 
        icd_dx_ment, 
        tablename = 'mh_dx',
        rownames=FALSE, colnames=TRUE, append=TRUE, fast=TRUE)