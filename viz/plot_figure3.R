################################################################################
## Author: Megan Knight                                                       ##
## Date: 10/24/2023                                                           ##
## Purpose: Visualization describing levels of telemedicine and in-person     ##
## care for select causes (fig3). Bonus alternative figure 2 draft.           ##
################################################################################
## libraries 
pacman::p_load(RODBC, data.table, stringr, ggplot2)

## snowflake permissions
hj_data <- odbcConnect ("hj_data", uid="megan_knight", pwd="R@ngerD@nger4$")
icd_dx_map_out <- odbcConnect ("icd_dx_map_out", uid="megan_knight", pwd="R@ngerD@nger4$")

## read outpatient patient-days by cause and type of care
tele <- fread('D:/Users/knight_megan/Documents/results/data/tele_outpatient_ptdays_cause.csv')[, care_type := 'Telemedicine']
inperson <- fread('D:/Users/knight_megan/Documents/results/data/outpatient_ptdays_cause.csv')[, care_type := 'In-person']
df <- rbind(tele, inperson)

## calculate number of "other" patient-days excluding select causes by type of care
tele_other <- merge(tele[, .(total = sum(weight)), by = 'PLOT_DATE'],tele[FAMILY_NAME %in% c('COVID-19', 'Mental disorders', 'Substance use disorder'), .(sub = sum(weight)), by = 'PLOT_DATE'], by = 'PLOT_DATE')
tele_other[, other := total-sub][, care_type := 'Telemedicine']

inperson_other <- merge(inperson[, .(total = sum(weight)), by = 'PLOT_DATE'],inperson[FAMILY_NAME %in% c('COVID-19', 'Mental disorders', 'Substance use disorder'), .(sub = sum(weight)), by = 'PLOT_DATE'], by = 'PLOT_DATE')
inperson_other[, other := total-sub][, care_type := 'In-person']

df_other <- rbind(tele_other, inperson_other)

## plot figure 3
pdf('D:/Users/knight_megan/Documents/results/viz/fig3.pdf', height = 8, width = 8*1.68)
p <- ggplot(data = df[FAMILY_NAME == 'COVID-19'], aes(x = PLOT_DATE, y = weight/1000, fill = care_type)) + 
  geom_bar(stat='identity') + 
  scale_fill_manual(values = c('#f1aed5', '#dd3497')) + 
  labs(x = 'Date', y = 'Percent patient-days (per 1,000)', fill = 'Telemedicine provision \n status', title = 'A. COVID-19') + 
  guides(fill=guide_legend(ncol=1)) + 
  theme_bw() + 
  theme(axis.text = element_text(size = 17), axis.title = element_text(size = 20), title = element_text(size = 24), legend.text=element_text(size=17))

print(p)

p <- ggplot(data = df[FAMILY_NAME == 'Mental disorders'], aes(x = PLOT_DATE, y = weight/1000, fill = care_type)) + 
  geom_bar(stat='identity') + 
  scale_fill_manual(values = c('#f9ba9a', '#e6550d')) + 
  labs(x = 'Date', y = 'Percent patient-days (per 1,000)', fill = 'Telemedicine provision \n status', title = 'A. Mental disorders') + 
  guides(fill=guide_legend(ncol=1)) + 
  theme_bw()  + 
  theme(axis.text = element_text(size = 22), axis.title = element_text(size = 23), title = element_text(size = 26), legend.text=element_text(size=22))
print(p)

p <- ggplot(data = df[FAMILY_NAME == 'Substance use disorders'], aes(x = PLOT_DATE, y = weight/1000, fill = care_type)) + 
  geom_bar(stat='identity') + 
  scale_fill_manual(values = c('#9dc3e5', '#2a679d')) + 
  labs(x = 'Date', y = 'Percent patient-days (per 1,000)', fill = 'Telemedicine provision \n status', title = 'B. Substance use disorders') + 
  guides(fill=guide_legend(ncol=1)) + 
  theme_bw() + 
  theme(axis.text = element_text(size = 22), axis.title = element_text(size = 23), title = element_text(size = 26), legend.text=element_text(size=22))
print(p)

p <- ggplot(data = df_other, aes(x = PLOT_DATE, y = other/1000, fill = care_type)) + 
  geom_bar(stat='identity') + 
  scale_fill_manual(values = c('#cccccc', '#969696')) + 
  labs(x = 'Date', y = 'Percent patient-days (per 1,000)', fill = 'Telemedicine provision \n status', title = 'C. Other causes') + 
  guides(fill=guide_legend(ncol=1)) + 
  theme_bw() + 
  theme(axis.text = element_text(size = 22), axis.title = element_text(size = 23), title = element_text(size = 26), legend.text=element_text(size=22))
print(p)
dev.off()

## calculate average for sorting 
df[, avg := mean(weight), by = 'FAMILY_NAME']

## plot alternative figure 2
## TODO: add colors from figure 2 and horizontal segment describing average (if pursued further)
pdf('D:/Users/knight_megan/Documents/results/viz/fig2_alt.pdf')
ggplot(data = df, aes(x = reorder(FAMILY_NAME, -avg),y=weight/1000)) + 
  geom_jitter()+
  facet_wrap(~care_type, scales = 'free_y', ncol = 1) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = 'Cause', y = 'Number of patient-days (per 1,000)')
dev.off()