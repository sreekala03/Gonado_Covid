###ANALYSIS IMPERIAL COLLEGE LONDON COHORT####

library(tidyverse)
library(caret)
library("ggpubr")
library("ggiraphExtra")
library("ggpmisc")
library(readxl)
library(dplyr)

theme_set(theme_bw())

####Figure 2D####
Male_first_sec <- read_excel("INPUT_ICL/Male_first_second.xlsx")

#remove rows 23 and 24 containing PtID 15: Testo value NA

Male_first_sec <- Male_first_sec[-c(23, 24),]

Male_first_sec[, 'PtID'] <- as.factor(Male_first_sec$PtID)


pdf("OUTPUT/Fig_2d_Male_first_sec_testo.pdf", width = 4.7, height = 6.10)
ggplot(Male_first_sec) + geom_point(mapping = aes(x= Visit, y=PtID, colour=group, size= Testo_ngml)) + labs(color ="Group", size ="Testosterone (ng/ml)") + 
  scale_color_manual(values = c("red", "darkorange", "deepskyblue3")) +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size = 12),
        axis.text = element_text(size = 12, colour="black"))
dev.off()

####Figure 2E####

#Calculate deltaT between first and the second visit

Male_Testo_weight <- read_excel("INPUT_ICL/Male_Testo_weight.xlsx")

Male_Testo_weight$T_First_Visit_ <- (Male_Testo_weight$T_First_Visit_)*0.288 #to convert T values to ng/ml
Male_Testo_weight$T_Second_visit <- (Male_Testo_weight$T_Second_visit)*0.288 #to convert T values to ng/ml
Male_Testo_weight$deltaT <-  (Male_Testo_weight$T_Second_visit) - Male_Testo_weight$T_First_Visit_ #calculate delta T
Male_Testo_weight[, 'Visit_1_weight'] <- as.numeric(Male_Testo_weight$Visit_1_weight) 
Male_Testo_weight[, 'Visit_2_weight'] <- as.numeric(Male_Testo_weight$Visit_2_weight)
Male_Testo_weight [, 'Adm_Wt'] <- as.numeric(Male_Testo_weight$Adm_Wt)

write.csv(Male_Testo_weight, "INPUT_ICL/Male_deltaT.csv")

##Read a new dataframe with group information, weight and BMI

Male_deltaT_w <- read.csv("INPUT_ICL/Male_deltaT_w.csv")

pdf("OUTPUT/Fig_2e_Testo_BMI_second.pdf", width = 5.5, height = 3.38)
ggplot(Male_deltaT_w, aes(x= BMI, y= T_Second_visit, color = group_sec_visit)) + scale_color_manual(labels = c("Abnormal (n = 2)", "Normal (n = 21)"), values = c("red", "deepskyblue")) +
 geom_point(position=position_jitter(h=0.2, w=0.1),shape = 21, size = 3) + theme(panel.border = element_blank(),
                                                                                                       panel.grid.major = element_blank(),
                                                                                                       panel.grid.minor = element_blank(),
                                                                                                       axis.line = element_line(colour = "black"),
                                                                                                       legend.title=element_blank(),
                                                                                                       text = element_text(size = 12),
                                                                                                       axis.text = element_text(size = 12, colour="black")) + labs(x = "Body Mass Index (BMI)", y = "Testosterone (ng/ml)") + scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, by = 2)) 
dev.off()


####SUPPLEMENTARY FIGURE 4A####

# Line plot paired

#Rearrange the dataframe for the paired plot

Male_delta_w_paired = read.csv("INPUT_ICL/Male_delta_w_pairedplot.csv")
Male_delta_w_paired= filter(Male_delta_w_paired, !is.na(Subgroup_weight)) 

pdf("OUTPUT/Supp_Fig4a_Change_BW.pdf", width = 4.8, height = 4)
ggplot(Male_delta_w_paired, aes(x=Visit, y=Weight, group= PtID)) +
  geom_point(aes(colour=Subgroup_weight), size=3, shape = 1) +
  geom_line(size=0.5, alpha=0.5, aes(colour=Subgroup_weight)) + scale_y_continuous(limits = c(50, 125), breaks = seq(50, 125, by = 25))+
  ylab('Body Weight (kg)') +
  scale_colour_manual(values=c("red", "darkgray", "deepskyblue"), guide=FALSE) + 
  theme(panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.text = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_blank(),
        text = element_text(size = 12),
        axis.text = element_text(size = 12, colour="black"),
        axis.title.x= element_blank())
dev.off()

####SUPPLEMENTARY FIGURE 4B####

pdf("OUTPUT/Supp_Fig4b_delta_testo_weight_vist2_1_spearman.pdf", width = 5.7, height = 4.5)

ggplot(Male_deltaT_w,aes(x = Visit_2_1_weight, y = deltaT) ) +
 geom_point(position=position_jitter(h = 0.2, w= 0.1),shape = 21, size = 4, show.legend = FALSE, aes(colour = factor(Subgroup_weight)))  + scale_color_manual(values = c("red", "darkgray", "deepskyblue")) +
  scale_x_continuous(limits = c(-40, 40), breaks = seq(-40, 40, by = 10))+
  geom_smooth(method=lm, na.rm = TRUE, fullrange= TRUE, se = FALSE, size= 0.15, linetype="dashed") + stat_cor(method = "spearman",label.x.npc = "centre") + geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) + theme_minimal() +
  theme(panel.border = element_blank(), legend.text = element_blank(),
        legend.title=element_blank(),
        text = element_text(size = 12),
        axis.text = element_text(size = 12, colour="black")) + labs(x = "Visit 2-1 Weight (kg) ", y = "Visit 2-1 Testosterone (ng/ml)")
dev.off()


save.image("Gonado_Covid/ICL_male_data.RData")


