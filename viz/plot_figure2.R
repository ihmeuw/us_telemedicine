################################################################################
## Author: Megan Knight                                                       ##
## Purpose: Visualization describing proportion of telemedicine and in-person ##
## care by cause (fig2).                                                      ##   
################################################################################
## libraries 
pacman::p_load(RODBC, data.table, stringr, ggplot2)

## read input data 
outpatient <- fread('D:/Users/knight_megan/Documents/results/data/outpatient_ptdays_cause.csv')
tele <- fread('D:/Users/knight_megan/Documents/results/data/tele_outpatient_ptdays_cause.csv')

df <- rbind(outpatient[, care_type := 'In-Person'], 
            tele[, care_type := 'Telemedicine'])

## collpasing some cause groupings per comments
df[, FAMILY_NAME := ifelse(FAMILY_NAME == "Unmapped" | 
                           FAMILY_NAME == "Garbage code", "Unassigned", 
                    ifelse(FAMILY_NAME == "Well causes", "Well visits", 
                    ifelse(FAMILY_NAME == "Neglected tropical diseases and malaria" | 
                           FAMILY_NAME == "HIV/AIDS and sexually transmitted infections" | 
                           FAMILY_NAME == "Enteric infections" | 
                           FAMILY_NAME == "Other infectious diseases", 'Non-respiratory infectious diseases',
                           FAMILY_NAME)))]

## ordering for plot 
df[, FAMILY_NAME := factor(FAMILY_NAME, levels = c("Neonatal disorders", "Non-respiratory infectious diseases",
                                                   "Nutritional deficiencies", "Maternal disorders", "Respiratory infections and tuberculosis", "COVID-19", "Congenital birth defects", "Hemoglobinopathies and hemolytic anemias", "Substance use disorders",
                                                   "Kidney diseases", "Oral disorders", "Urinary diseases and male infertility", "Neoplasms", "Gynecological diseases", "Endocrine, metabolic, blood, and immune disorders", "Sense organ diseases", 
                                                   "Digestive diseases", "Skin and subcutaneous diseases", "Cardiovascular diseases", "Neurological disorders", "Diabetes", "Chronic respiratory diseases", "Musculoskeletal disorders", "Mental disorders", 
                                                   "Injuries", "Tobacco intervention", "Treatment of obesity", "Treatment of hyperlipidemia", "Well visits", "Treatment of hypertension", "Unassigned"))]


## plot figure 2
p <- ggplot(data = df, aes(x = PLOT_DATE, y = prop*100, fill = FAMILY_NAME)) + 
  geom_bar(stat='identity') + 
  scale_fill_manual(values = c('#a50f15','#de2d26','#fb6a4a','#fcae91','#fee5d9',
                               '#dd3497', 
                               '#15548f', '#205d96','#2a679d','#3570a4','#407aab','#4a83b2','#558db9','#5f96c0','#6aa0c7','#75a9ce','#7fb2d5','#8abcdc','#95c5e3','#9fcfea','#aad8f1','#b4e2f8','#bfebff',
                               '#fd8d3c',
                               '#ffeda0',
                               '#006d2c', '#31a354', '#74c476','#bae4b3', '#edf8e9',
                               '#756bb1')) + 
  facet_wrap(~care_type, nrow = 2) + 
  scale_x_date(date_labels="%b-%Y",date_breaks  ="6 month", expand = c(0, 0), limits = c(min(df$PLOT_DATE), max = max(df$PLOT_DATE))) + 
  labs(x = 'Date', y = 'Percent patient-days', fill = 'Cause name') + 
  theme_bw() + 
  theme(strip.background = element_blank(), 
        strip.text = element_text(size=20), 
        axis.title=element_text(size=20),
        axis.text=element_text(size=12), 
        legend.text=element_text(size=12), legend.title = element_text(size = 20)) + 
  guides(fill = guide_legend(ncol = 1, title.hjust = 0.5, axis.title = element_text(size = 20)))
print(p)

pdf('D:/Users/knight_megan/Documents/results/viz/fig2.pdf', height = 16/1.68, width = 16)
print(p)
dev.off()