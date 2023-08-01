#####Analysis Lille ICU Cohort#####

library(dplyr)
library(tidyverse)
library(caret)
library("ggpubr")
library("ggiraphExtra")
library("ggpmisc")
library(readxl)
theme_set(theme_bw())

####DATA PREPARATION ICU COVID PATIENTS####

Covid_ICU <-  read.csv("INPUT_Lille/Covid_ICU_Lille.csv", header = TRUE, stringsAsFactors = TRUE)

Covid_ICU_grouped <-  Covid_ICU %>% mutate(group = case_when (
  (Covid_ICU$TESTO < 2.8842 & Covid_ICU$LH <= 2)                      ~ "Group1",
  (Covid_ICU$TESTO < 2.8842 & Covid_ICU$LH > 2 & Covid_ICU$LH < 12) ~ "Group2",
  (Covid_ICU$TESTO < 2.8842 & Covid_ICU$LH > 12)                      ~ "Group3",
  (Covid_ICU$TESTO > 2.8842)                  ~ "Group4")
  
)


Covid_ICU_grouped =  Covid_ICU_grouped %>% mutate(group_l2 =
                                                                    case_when(group == "Group1" ~ "Abnormal", 
                                                                              group == "Group2" ~ "Abnormal",
                                                                              group == "Group3" ~ 'Normal',
                                                                              group == "Group4" ~ "Normal")
)

#Add mortality information
Covid_ICU_grouped= Covid_ICU_grouped %>% 
  mutate(Mortality = case_when(DC_CAUSE == 1  ~ "Deceased",
                               is.na(DC_CAUSE) ~ "Survived")
  )

readr::write_excel_csv(Covid_ICU_grouped, "INPUT_Lille/Covid_ICU_grouped.csv")



#####Chi squared test for Figure 1D#####

#Pearson's Chi squared test for mortality

mortality_chi <- data.frame(Group = c("Abnormal", "Normal"), Deceased  = c(12, 4), Survived = c(11, 33))
chisq.test(mortality_chi [,-1], correct = TRUE)

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  mortality_chi[, -1]
# X-squared = 10.384, df = 1, p-value = 0.001271


#####DATA PREPARATION ICU CONTROL PATIENTS#####

#load data
control = read.csv ("INPUT_Lille/Control_data.csv", header = TRUE, stringsAsFactors = TRUE)

control <-  control %>% mutate(group = case_when (
  (control$Testo < 2.8842 & control$LH <= 2)                      ~ "Group1",
  (control$Testo < 2.8842 & control$LH > 2 & control$LH < 12) ~ "Group2",
  (control$Testo < 2.8842 & control$LH > 12)                      ~ "Group3",
  (control$Testo > 2.8842)                  ~ "Group4")
  
)

write.csv(control, "INPUT_Lille/Control_grouped.csv")



##Supplementary Figure 1A : Testosterone levels and CRP correlation#######

testo_CRP = read.csv("INPUT_Lille/Covid_CRP.csv") 

# testo_CRP = testo_CRP %>% mutate(CRP.levels =
#                                                          case_when(CRP <= 15 ~ "Low_CRP", 
#                                                                    CRP >  15 ~ "High_CRP",
#                                                                    
#                                                          )
# )

##Week 1 average
testo_CRP_week1 = read.csv("INPUT_Lille/Covid_CRP_week1_avg.csv", stringsAsFactors = TRUE)

pdf("OUTPUT/Supp_1A_Testo_CRP_spearman.pdf", width = 6, height = 4.25)
ggplot(testo_CRP_week1,aes(x=CRP_Week1_avg, y=TESTOSTERONE, color=GROUP_Norm_AB)) + scale_color_manual(labels = c("Abnormal (n = 26)", "Normal (n = 32)"), values = c("red", "deepskyblue"))+
  geom_point(size = 3, shape = 21, alpha = 0.6) +
  geom_smooth(method=lm, na.rm = TRUE, fullrange= TRUE,
              aes(group=1),colour="black", se = FALSE, linetype = "dashed") + stat_cor( method = "spearman",(aes(group=1))) + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.title=element_blank(),
        text = element_text(size = 12),
        axis.text = element_text(size = 12, colour="black")) +labs(x = "CRP (mg/l)", y = "Testosterone (ng/ml)")
dev.off()

cor.test(testo_CRP_week1$CRP_Week1_avg, testo_CRP_week1$TESTOSTERONE, method = "spearman", exact = FALSE)
# Spearman's rank correlation rho
# 
# data:  testo_CRP_week1$CRP_Week1_avg and testo_CRP_week1$TESTOSTERONE
# S = 32510, p-value = 0.9998
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#          rho 
# -3.07645e-05 


##Supplementary figure 1 B and C 

#Dot plot showing testosterone levels and mortality status for each subject

Groups_deceased <- subset(Covid_ICU_grouped, DC_CAUSE == 1)
Groups_deceased$Mortality = "Deceased"
Groups_deceased[, 'SUBJID'] <- as.factor(Groups_deceased[,'SUBJID'])

Groups_survived <- Covid_ICU_grouped[is.na(Covid_ICU_grouped$DC_CAUSE),] #check NA for patients who survived
Groups_survived $Mortality = "Survived"
Groups_survived[, 'SUBJID'] <- as.factor(Groups_survived[,'SUBJID'])

pdf("OUTPUT/Supp_Fig1b_deceased_dot.pdf", width = 5, height = 5.97)
d = ggplot(Groups_deceased) + geom_point(mapping = aes(x= Week.Prelevement, y=SUBJID, colour=group_l2, size= TESTO)) 
d+ scale_color_manual(values = c("red", "deepskyblue3")) + labs(color ="Group", size ="Testosterone (ng/ml)") + theme(text = element_text(size = 12),
                                                                                                                      axis.text = element_text(size = 12, colour="black"),
                                                                                                                      axis.title.x= element_blank(), 
                                                                                                                      legend.text = element_text (size = 12)
                                                                                                                      )
dev.off()

pdf("OUTPUT/Supp_Fig1b_survived_dot.pdf", width = 5.2, height = 8.9)
s = ggplot(Groups_survived) + geom_point(mapping = aes(x= Week.Prelevement, y=SUBJID, colour=group_l2, size= TESTO)) 
s+ scale_color_manual(values = c("red", "deepskyblue3")) + labs(color ="Group", size ="Testosterone (ng/ml)") + theme(text = element_text(size = 12),
                                                                                                                      axis.text = element_text(size = 12, colour="black"),
                                                                                                                      axis.title.x= element_blank(), 
                                                                                                                      legend.text = element_text (size = 12))
dev.off()

save.image("Gonado_Covid/Lille_ICU_data.RData")
