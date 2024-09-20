
## loading packages
library("tidyverse")
library("gtsummary")
library("dplyr")
library("ggplot2")
library("qgraph")
library("bootnet")
library("NetworkComparisonTest")
library("psychonetrics")
library("mgm")



setwd("/Users/siyamakbayrami")


#Data cleaning
ORG_DF<-read.csv("for expriement.csv")
intialdata<-ORG_DF[-c(2446:5906), ]
summary(ORG_DF)
intialdata_new<-replace(intialdata, intialdata == -99, NA)
summary(intialdata_new)
intialdata_new[intialdata_new < 0] <- NA
summary(intialdata_new)
intialdatamissingvalues_df<- as.data.frame(colSums(is.na(intialdata_new)))
intialdatavalues_df<- as.data.frame(colSums(!is.na(intialdata_new)))
missing_values_in_MoL <- is.na(intialdata_new$SeekingMeaning) | is.na(intialdata_new$PresenceMeaning)
data_complete_after_MoL <- intialdata_new[!missing_values_in_MoL, ]
summary(data_complete_after_MoL)
missingvalues_in_data_complete_df<- as.data.frame(colSums(is.na(data_complete_after_MoL)))
namesofvariab<-as.data.frame(colnames(data_complete_after_MoL))
list<-cbind(missingvalues_in_data_complete_df,namesofvariab)
names(sort(list, decreasing = TRUE))
sorted<-list[order(list[, 1]), ]

data_complete_after_MoL<-subset(data_complete_after_MoL, select = -optional)
write.csv(data_complete_after_MoL,"DF with complete cases in ML.csv", row.names = F)
write.csv(ORG_DF, "Raw dataset.csv", row.names = F)


###########################ERI scale
data_complete_after_MoL<-read.csv("DF with complete cases in ML.csv")

summary(data_complete_after_MoL)
data_completeERI <- data_complete_after_MoL %>%
  mutate(eri_1 = as.numeric(case_when(Effrew1 == 1 ~ 4,
                                      Effrew1 == 2 ~ 3,
                                      Effrew1 == 3 ~ 2,
                                      Effrew1 == 4 ~ 1,
                                      TRUE ~ NA_real_)),
         eri_2 = as.numeric(case_when(Effrew2 == 1 ~ 4,
                                      Effrew2 == 2 ~ 3,
                                      Effrew2 == 3 ~ 2,
                                      Effrew2 == 4 ~ 1,
                                      TRUE ~ NA_real_)),
         eri_3 = as.numeric(case_when(Effrew3 == 1 ~ 4,
                                      Effrew3 == 2 ~ 3,
                                      Effrew3 == 3 ~ 2,
                                      Effrew3 == 4 ~ 1,
                                      TRUE ~ NA_real_)),
         eri_4 = as.numeric(case_when(Effrew4 == 1 ~ 4,
                                      Effrew4 == 2 ~ 3,
                                      Effrew4 == 3 ~ 2,
                                      Effrew4 == 4 ~ 1,
                                      TRUE ~ NA_real_)),
         eri_5 = as.numeric(case_when(Effrew5 == 1 ~ 1,
                                      Effrew5 == 2 ~ 2,
                                      Effrew5 == 3 ~ 3,
                                      Effrew5 == 4 ~ 4,
                                      TRUE ~ NA_real_)),
         eri_6 = as.numeric(case_when(Effrew6 == 1 ~ 1,
                                      Effrew6 == 2 ~ 2,
                                      Effrew6 == 3 ~ 3,
                                      Effrew6 == 4 ~ 4,
                                      TRUE ~ NA_real_)),
         eri_7 = as.numeric(case_when(Effrew7 == 1 ~ 1,
                                      Effrew7 == 2 ~ 2,
                                      Effrew7 == 3 ~ 3,
                                      Effrew7 == 4 ~ 4,
                                      TRUE ~ NA_real_)),
         eri_8 = as.numeric(case_when(Effrew8 == 1 ~ 4,
                                      Effrew8 == 2 ~ 3,
                                      Effrew8 == 3 ~ 2,
                                      Effrew8 == 4 ~ 1,
                                      TRUE ~ NA_real_)),
         eri_9 = as.numeric(case_when(Effrew9 == 1 ~ 4,
                                      Effrew9 == 2 ~ 3,
                                      Effrew9 == 3 ~ 2,
                                      Effrew9 == 4 ~ 1,
                                      TRUE ~ NA_real_)),
         eri_10 = as.numeric(case_when(Effrew10 == 1 ~ 4,
                                       Effrew10 == 2 ~ 3,
                                       Effrew10 == 3 ~ 2,
                                       Effrew10 == 4 ~ 1,
                                       TRUE ~ NA_real_)))

data_all_eri_sum <- data_completeERI %>%
  mutate(eri_e_sum = rowSums(.[c("eri_1", "eri_2", "eri_3")], na.rm = TRUE),
         eri_r_sum = rowSums(.[c("eri_4", "eri_5", "eri_6", "eri_7", "eri_8", "eri_9", "eri_10")], na.rm = TRUE),
         eri_ratio = eri_e_sum / (eri_r_sum * 0.42857),
         eri_e_mean = rowSums(.[c("eri_1", "eri_2", "eri_3")], na.rm = TRUE) / 3,
         eri_r_mean = rowSums(.[c("eri_4", "eri_5", "eri_6", "eri_7", "eri_8", "eri_9", "eri_10")], na.rm = TRUE) / 7)
data_all_eri_sum$eri_ratio_quintile <- as.factor(ntile(data_all_eri_sum$eri_ratio, 5))


######################GRIT

data_all_grit <- data_all_eri_sum %>%
  mutate(grit_1 = as.numeric(case_when(GRIT1 == 1 ~ 1,
                                       GRIT1 == 2 ~ 2,
                                       GRIT1 == 3 ~ 3,
                                       GRIT1 == 4 ~ 4,
                                       GRIT1 == 5 ~ 5,
                                       TRUE ~ NA_real_)),
         grit_2 = as.numeric(case_when(GRIT2 == 1 ~ 5,
                                       GRIT2 == 2 ~ 4,
                                       GRIT2 == 3 ~ 3,
                                       GRIT2 == 4 ~ 2,
                                       GRIT2 == 5 ~ 1,
                                       TRUE ~ NA_real_)),
         grit_5 = as.numeric(case_when(GRIT3 == 1 ~ 1,
                                       GRIT3 == 2 ~ 2,
                                       GRIT3 == 3 ~ 3,
                                       GRIT3 == 4 ~ 4,
                                       GRIT3 == 5 ~ 5,
                                       TRUE ~ NA_real_)),
         grit_4 = as.numeric(case_when(GRIT4 == 1 ~ 5,
                                       GRIT4 == 2 ~ 4,
                                       GRIT4 == 3 ~ 3,
                                       GRIT4 == 4 ~ 2,
                                       GRIT4 == 5 ~ 1,
                                       TRUE ~ NA_real_)),
         grit_6 = as.numeric(case_when(GRIT5 == 1 ~ 1,
                                       GRIT5 == 2 ~ 2,
                                       GRIT5 == 3 ~ 3,
                                       GRIT5 == 4 ~ 4,
                                       GRIT5 == 5 ~ 5,
                                       TRUE ~ NA_real_)),
         grit_7 = as.numeric(case_when(GRIT6 == 1 ~ 5,
                                       GRIT6 == 2 ~ 4,
                                       GRIT6 == 3 ~ 3,
                                       GRIT6 == 4 ~ 2,
                                       GRIT6 == 5 ~ 1,
                                       TRUE ~ NA_real_)),
         grit_3 = as.numeric(case_when(GRIT9 == 1 ~ 1,
                                       GRIT9 == 2 ~ 2,
                                       GRIT9 == 3 ~ 3,
                                       GRIT9 == 4 ~ 4,
                                       GRIT9 == 5 ~ 5,
                                       TRUE ~ NA_real_)),
         grit_8 = as.numeric(case_when(GRIT8 == 1 ~ 5,
                                       GRIT8 == 2 ~ 4,
                                       GRIT8 == 3 ~ 3,
                                       GRIT8 == 4 ~ 2,
                                       GRIT8 == 5 ~ 1,
                                       TRUE ~ NA_real_)))
#### for calculating totale score of GRIT, add up all the points for the items and divide by 8

data_all_grit_sum <- data_all_grit %>%
  mutate(grit_sum = rowSums(select(., starts_with("grit_"))) / 8)

##################warwick_ edinbrough_scale
data_all_wemwbs_1 <- data_all_grit_sum %>%
  mutate(optimistic = as.numeric(case_when(Warwick1 == 1 ~ 5,
                                           Warwick1 == 2 ~ 4,
                                           Warwick1 == 3 ~ 3,
                                           Warwick1 == 4 ~ 2,
                                           Warwick1 == 5 ~ 1,
                                           TRUE ~ NA_real_)),
         useful = as.numeric(case_when(Warwick2 == 18 ~ 5,
                                       Warwick2 == 19 ~ 4,
                                       Warwick2 == 20 ~ 3,
                                       Warwick2 == 21 ~ 2,
                                       Warwick2 == 22 ~ 1,
                                       TRUE ~ NA_real_)),
         relaxed = as.numeric(case_when(Warwich3 == 1 ~ 5,
                                        Warwich3 == 2 ~ 4,
                                        Warwich3 == 3 ~ 3,
                                        Warwich3 == 4 ~ 2,
                                        Warwich3 == 5 ~ 1,
                                        TRUE ~ NA_real_)),
         effective = as.numeric(case_when(Warwick4 == 1 ~ 5,
                                          Warwick4 == 2 ~ 4,
                                          Warwick4 == 3 ~ 3,
                                          Warwick4 == 4 ~ 2,
                                          Warwick4 == 5 ~ 1,
                                          TRUE ~ NA_real_)),
         clearr = as.numeric(case_when(Warwick5 == 1 ~ 5,
                                       Warwick5 == 2 ~ 4,
                                       Warwick5 == 3 ~ 3,
                                       Warwick5 == 4 ~ 2,
                                       Warwick5 == 5 ~ 1,
                                       TRUE ~ NA_real_)),
         closeness = as.numeric(case_when(Warwick6 == 1 ~ 5,
                                          Warwick6 == 2 ~ 4,
                                          Warwick6 == 3 ~ 3,
                                          Warwick6 == 4 ~ 2,
                                          Warwick6 == 5 ~ 1,
                                          TRUE ~ NA_real_)),
         makingmind = as.numeric(case_when(Warwick7 == 1 ~ 5,
                                           Warwick7 == 2 ~ 4,
                                           Warwick7 == 3 ~ 3,
                                           Warwick7 == 4 ~ 2,
                                           Warwick7 == 5 ~ 1,
                                           TRUE ~ NA_real_)))
data_all_wemwbs_2 <- data_all_wemwbs_1 %>%
  rowwise() %>%
  mutate(sw_wemwbs = as.numeric(case_when(is.na(optimistic) | is.na(useful) | is.na(relaxed) | is.na(effective) |
                                            is.na(clearr) | is.na(closeness) | is.na(makingmind) ~ NA_real_,
                                          TRUE ~ sum(optimistic, useful, relaxed, effective, clearr, closeness, makingmind))))
data_all_wemwbs_3 <- data_all_wemwbs_2 %>%
  mutate(sw_wemwbs_met = case_when(sw_wemwbs == 7 ~ 7,
                                   sw_wemwbs == 8 ~ 9.51,
                                   sw_wemwbs == 9 ~ 11.25,
                                   sw_wemwbs == 10 ~ 12.4,
                                   sw_wemwbs == 11 ~ 13.33,
                                   sw_wemwbs == 12 ~ 14.08,
                                   sw_wemwbs == 13 ~ 14.75,
                                   sw_wemwbs == 14 ~ 15.32,
                                   sw_wemwbs == 15 ~ 15.84,
                                   sw_wemwbs == 16 ~ 16.36,
                                   sw_wemwbs == 17 ~ 16.88,
                                   sw_wemwbs == 18 ~ 17.43,
                                   sw_wemwbs == 19 ~ 17.98,
                                   sw_wemwbs == 20 ~ 18.59,
                                   sw_wemwbs == 21 ~ 19.25,
                                   sw_wemwbs == 22 ~ 19.98,
                                   sw_wemwbs == 23 ~ 20.73,
                                   sw_wemwbs == 24 ~ 21.54,
                                   sw_wemwbs == 25 ~ 22.35,
                                   sw_wemwbs == 26 ~ 23.21,
                                   sw_wemwbs == 27 ~ 24.11,
                                   sw_wemwbs == 28 ~ 25.03,
                                   sw_wemwbs == 29 ~ 26.02,
                                   sw_wemwbs == 30 ~ 27.03,
                                   sw_wemwbs == 31 ~ 28.13,
                                   sw_wemwbs == 32 ~ 29.31,
                                   sw_wemwbs == 33 ~ 30.7,
                                   sw_wemwbs == 34 ~ 32.55,
                                   sw_wemwbs == 35 ~ 35,
                                   TRUE ~ NA_real_),
         swemwbs = as.factor(case_when(sw_wemwbs_met < 20.99 ~ "low",
                                       sw_wemwbs_met >20.99 & sw_wemwbs_met < 30.7 ~ "medium",
                                       sw_wemwbs_met > 30.6 ~ "high",
                                       TRUE ~ as.character(NA))))
# reversing items in one direction for interpreting results

data_all_wemwbs_3 <- data_all_wemwbs_3 %>%
  mutate(
    Overal_Physical_Health_Rate = as.numeric(case_when(
      PhysH_Rate == 1 ~ 5,
      PhysH_Rate == 2 ~ 4,
      PhysH_Rate == 3 ~ 3,
      PhysH_Rate == 4 ~ 2,
      PhysH_Rate == 5 ~ 1,
      TRUE ~ NA_real_
    )),
    Overal_Mental_Health_Rate = as.numeric(case_when(
      Overall_Ment_Health == "Excellent" ~ 5,
      Overall_Ment_Health == "Very good" ~ 4,
      Overall_Ment_Health == "Good" ~ 3,
      Overall_Ment_Health == "Fair" ~ 2,
      Overall_Ment_Health == "Poor" ~ 1,
      TRUE ~ NA_real_
    )),
    Study_Envir_Calm = as.numeric(case_when(
      Q3_1 == 1 ~ 4,
      Q3_1 == 2 ~ 3,
      Q3_1 == 3 ~ 2,
      Q3_1 == 4 ~ 1,
      TRUE ~ NA_real_
    )),
    Study_Envir_Togetherness = as.numeric(case_when(
      Q3_2 == 1 ~ 4,
      Q3_2 == 2 ~ 3,
      Q3_2 == 3 ~ 2,
      Q3_2 == 4 ~ 1,
      TRUE ~ NA_real_
    )),
    Study_Envir_ColleagueSupp = as.numeric(case_when(
      Q3_3 == 1 ~ 4,
      Q3_3 == 2 ~ 3,
      Q3_3 == 3 ~ 2,
      Q3_3 == 4 ~ 1,
      TRUE ~ NA_real_
    )),
    Study_Envir_Bad_Day = as.numeric(case_when(
      Q3_4 == 1 ~ 4,
      Q3_4 == 2 ~ 3,
      Q3_4 == 3 ~ 2,
      Q3_4 == 4 ~ 1,
      TRUE ~ NA_real_
    )),
    Study_Envi_with_Teachers = as.numeric(case_when(
      Q3_5 == 1 ~ 4,
      Q3_5 == 2 ~ 3,
      Q3_5 == 3 ~ 2,
      Q3_5 == 4 ~ 1,
      TRUE ~ NA_real_
    )),
    Study_Envi_Collegues_Enjoy = as.numeric(case_when(
      Q3_6 == 1 ~ 4,
      Q3_6 == 2 ~ 3,
      Q3_6 == 3 ~ 2,
      Q3_6 == 4 ~ 1,
      TRUE ~ NA_real_
    )),
    swemwbs_numeric= as.numeric(case_when(
      swemwbs== "low" ~ 1,
      swemwbs== "medium" ~ 2,
      swemwbs== "high" ~ 3,
      TRUE ~ NA_real_
    )),
    EmoProbEver_Dep= as.numeric(case_when(
      EmoProbEver_1== 2~0,
      EmoProbEver_1== 1~1,
      TRUE ~ NA_real_
    )),
    EmoProbEver_Bip= as.numeric(case_when(
      EmoProbEver_2== 2 ~ 0,
      EmoProbEver_2== 1 ~ 1,
      TRUE ~ NA_real_
    )),
    EmoProbEver_Panic= as.numeric(case_when(
      EmoProbEver_3== 2 ~ 0,
      EmoProbEver_3== 1 ~ 1,
      TRUE ~ NA_real_
    )),
    EmoProbEver_Anx= as.numeric(case_when(
      EmoProbEver_4== 2 ~ 0,
      EmoProbEver_4== 1 ~1,
      TRUE ~ NA_real_
    )),
    EmoProbEver_Other= as.numeric(case_when(
      EmoProbEver_5== 2 ~ 0,
      EmoProbEver_5== 1 ~ 1,
      TRUE ~ NA_real_
    )),
    TxtTypeEver_Psy= as.numeric(case_when(
      TxtTypeEver_1== 2 ~ 0,
      TxtTypeEver_1== 1 ~ 1,
      TRUE ~ NA_real_
    )),
    TxtTypeEver_Med= as.numeric(case_when(
      TxtTypeEver_2== 2 ~ 0,
      TxtTypeEver_2== 1 ~ 1,
      TRUE ~ NA_real_
    )),
    TxtTypeEver_Other= as.numeric(case_when(
      TxtTypeEver_3== 2 ~ 0,
      TxtTypeEver_3== 1 ~ 1,
      TRUE ~ NA_real_
    )),
    SocSup_FreqLonely= as.numeric(case_when(
      SocSup_FreqLonely== 1 ~ 5,
      SocSup_FreqLonely== 2 ~ 4,
      SocSup_FreqLonely== 3 ~ 3,
      SocSup_FreqLonely== 4 ~ 2,
      SocSup_FreqLonely== 5 ~ 1,
      TRUE ~ NA_real_
    )),
    Pain_Inter= as.numeric(case_when(
      Pain_Inter== 1 ~ 5,
      Pain_Inter== 2 ~ 4,
      Pain_Inter== 3 ~ 3,
      Pain_Inter== 4 ~ 2,
      Pain_Inter== 5 ~ 1,
      TRUE ~ NA_real_
    )),
    PhysMent_Inter= as.numeric(case_when(
      PhysMent_Inter== 1 ~ 5,
      PhysMent_Inter== 2 ~ 4,
      PhysMent_Inter== 3 ~ 3,
      PhysMent_Inter== 4 ~ 2,
      PhysMent_Inter== 5 ~ 1,
      TRUE ~ NA_real_
    ))

  )

##renaming variables
data_all_wemwbs_3<- data_all_wemwbs_3 %>%
  rename(leisure_time = Q4.0, ML_clusters= TSCA_MoL)

#compelete cases
missing_values_in_df <- rowSums(is.na(data_all_wemwbs_3)) > 0
data_complete_thesis <- data_all_wemwbs_3[!missing_values_in_df, ]
summary(data_complete_thesis)

write.csv(data_complete_thesis,"thesis.csv", row.names = F)
write.csv(DescData,"Descriptive dataset.csv",row.names = F)


########### Selecting  variables
data_complete_thesis<-read.csv("thesis.csv")
summary(data_complete_thesis)
colnames(data_complete_thesis)

selectedvariables<- c("age","Gender","Marital","SexOrient","Paren_Edu","Par_lang","Study","PresenceMeaning",
                      "SeekingMeaning", "ML_clusters","TRT_FLG_ADHD","TRT_FLG_Depr",
                      "TRT_FLG_Anx","TRT_FLG_PTSD","TRT_FLG_SocAnx","TRT_FLG_Drug","TRT_FLG_Ideat_Freq30day",
                      "TRT_FLG_Ideat_12mActOn","TRT_FLG_NSSI","Overal_Physical_Health_Rate",
                      "Overal_Mental_Health_Rate","leisure_time","Q81" , "PhysMent_Inter","Pain_Inter","EmoProbEver_Panic",
                      "TxtTypeEver_Psy", "TxtTypeEver_Med", "SocSup_Loved", "SocSup_FreqLonely", "Study_Envir_Calm",
                      "Study_Envi_with_Teachers", "Study_Envi_Collegues_Enjoy", "eri_ratio_quintile",
                      "grit_sum","swemwbs_numeric")



Final_DF_NETWORK<- data_complete_thesis[selectedvariables]
colnames(Final_DF_NETWORK)
summary(Final_DF_NETWORK)

write.csv(Final_DF_NETWORK,"Network_dataset.csv", row.names = F)




###### Loading data set and preparing for Descriptive
Desc_dataset<-read.csv("Descriptive dataset.csv")
summary(Desc_dataset)
Desc_dataset$Marital <- factor(Desc_dataset$Marital,
                               levels = c(1:5),
                               labels = c("Married", "Separated","Divorced",
                                          "Widowed","Never married" ) )
Desc_dataset$Gender<- factor(Desc_dataset$Gender, levels = c(1,2),
                             labels = c("Male", "Female"))
Desc_dataset$SexOrient<- factor(Desc_dataset$SexOrient, levels = c(1:6),
                                labels = c("straight", "Homosexual",
                                           "Bisexual", "Asexual", "Not Sure",
                                           "Other"))
Desc_dataset$Paren_Edu <- factor(Desc_dataset$Paren_Edu, levels = c(1:7),
                                 labels = c("None", "Elementary school",
                                            "Secondary school",
                                            "Some_Post-secondary education",
                                            "University graduate","Doctoral degree","Don’t know"))
Desc_dataset$Par_lang<- factor(Desc_dataset$Par_lang, levels = c("SV", "EN"),
                               labels = c("Swedish","English"))
Desc_dataset$Study<- factor(Desc_dataset$Study, levels = c(1:4), labels =
                              c("Full-time", "Part-time", "Non-degree", "Other"))

Desc_dataset$age <- cut(Desc_dataset$age,
                        breaks = c(17, 29, 36),
                        labels = c("18-29", "+30"),
                        right = FALSE)
Desc_dataset$ML_clusters<- factor(Desc_dataset$ML_clusters, levels = c(1:3), labels = c("Low Presence vs High Seeking",
                                                                                        "High Presence vs Low Seeking",
                                                                                        "High Presence vs High Seeking"))


###Descriptive Analysis

Descrip_var<- c("age","Gender","Marital", "SexOrient","Paren_Edu","Study","ML_clusters")


tbl_summary(Desc_dataset[Descrip_var])

tbl_summary(Desc_dataset[Descrip_var], by= "ML_clusters",missing = "ifany",
            missing_text = "NAs" ) %>% add_overall()

#  chi-square test and print results
perform_chi_square <- function(variable_name) {

  contingency_table <- table(Desc_dataset$ML_clusters, Desc_dataset[[variable_name]])


  chi_square_result <- chisq.test(contingency_table)

  cat("\nChi-square test for", variable_name, ":\n")
  print(chi_square_result)
}

Descrip_var<- c("age","Gender","Marital", "SexOrient","Paren_Edu","Study")

# Perform chi-square tests for each demographic variable
for (variable in Descrip_var) {
  perform_chi_square(variable)
}



###Writing Datasets without and with descriptive variables


Netd<-Final_DF_NETWORK[,-c(1:7)]

write.csv(Desc_dataset,"descriptive dataset.csv",row.names = F)
write.csv(Netd,"Network.csv",row.names = F)



#####Network Analyses




############ Overal network analysis
Netd<- read.csv("Network.csv")
Netd<-  Netd[,-3]
Net<-data.matrix(Netd)
summary(Net)
colnames(Net)



#### legends and layout

Node_Overall<- c("Presence of Meaning","Seeking of Meaning","ADHD Treatment Flag" ,"Depression Treatment Flag", "Anxiety Treatment Flag",
                 "PTSD Treatment Flag", "Social Anxiety Treatment Flag","Drug Treatment Flag",
                 "Suicidal Ideation Frequency 30-day  Treatment Flag","Suicidal Ideation Action 12-month  Treatment Flag","NSSI Treatment Flag"  ,
                 "Overal Physical Health Self Rate", "Overal Mental Health Self Rate" ,  "Meaningfulness of Leisure Time",
                 "Treatment Interest", "Physical-Mental Interference","Pain  Interference" ,
                 "Emotion Problem Panic (Lifetime)" ,"Psychological Tretment","Medication Tretment" ,
                 "Social Support-Feeling loved" ,"Social Support-Feeling lonely" , "Study Environment (Calm)"  ,
                 "Study Environment (Teachers)","Study Environment Collegues Enjoyment" , "Effort-Reward Imbalance" ,
                 "Grit","Well-eing")
Label_names_Ove<- c("PML","SML","ADHDTxF","DeprTxF","AnxTxF","PTSDTxF",
                    "SocAnxTxF","DRTxF","Ideat30dTxF","Ideat12mActTxF","NSSITxF", "PhysHlth",
                    "MentHlth","LeisureMean", "TxInterest", "PhysMentInterf", "PainInterf",
                    "PanicLT","PsychTxLT","MentTxLT","FeelLoved", "FeelLonely","StEnvCalm",
                    "StEnvTeacher", "StEnvColleagues","ERI",  "Grit", "Wellbeing")


### grouping variabels----> legend and layout
groups_overall<- list(
  "Wisdom"= c(1:2,15,19,27),
  "Emotion"= c(3:11,18,28),
  "Body"= c(12:13,16:17),
  "Social"= c(14,20:26))

### type of variables
type<- c("g","g","c","c","c","c","c","c","c","c","c","g","g","c","c",
         "g","g","c","c","c","g","g","g","g","g","g","g","g")
# Initialize an empty vector to store the levels
level <- numeric(length(type))

# Iterate over each element in the type vector
for (i in seq_along(type)) {
  if (type[i] == "g") {  # Continuous variable
    level[i] <- 1
  } else if (type[i] == "c") {  # Categorical variable
    level[i] <- length(unique(Netd[[names(Netd)[i]]]))
  }
}


print(level)


###### Using estimateNetwork function


NGraph_Overall<-estimateNetwork(Net, type= type, default = "mgm", verbose = F, criterion =
                                  "EBIC",  rule = "AND", threshold = "LW",
                                level = level, tuning= .5
)
NGraph_Overall


graph_o<-NGraph_Overall$graph



graph_Overall<- qgraph(graph_o, groups= groups_overall,labels= Label_names_Ove, nodeNames=Node_Overall,
                       layout= "spring", legend= T,
                       palette= "pastel",theme="colorblind", vsize = 7,
                       legend.mode= "style1", legend.cex= .3, negDashed= T,
                       layoutOffset= c(-.21,0), label.scale= T, edge.labels=F)
centralityPlot(graph_o, include = "all", orderBy = "Strength", labels = Label_names_Ove)

pdf("Overall Network Structure.pdf", width = 25, height = 25)
dev.new()

#### Bootstrap for overall network
#Bootstrap for edges
Bootstrap_edges_Overall<- bootnet(NGraph_Overall, nBoots = 1000,  nCores=10,
                                  type = "nonparametric")
summary(Bootstrap_edges_Overall)
plot(Bootstrap_edges_Overall,statistics= "strength", plot= "area",order = "sample",decreasing = T, labels = F)


#Bootstrap for centrality
Bootstrap_Centrality_Overall<- bootnet(NGraph_Overall, nBoots = 1000,  nCores=10,
                                       type = "case")
summary(Bootstrap_Centrality_Overall)
plot(Bootstrap_Centrality_Overall,statistics= "strength", order = "sample",decreasing = T, labels = T)
corStability(Bootstrap_Centrality_Overall)


########################################################classifying clusters
Final_DF_NETWORK<-read.csv("Network.csv")
cluster1_dataset <- Final_DF_NETWORK[Final_DF_NETWORK$ML_clusters == 1, ]
cluster2_dataset <- Final_DF_NETWORK[Final_DF_NETWORK$ML_clusters == 2, ]
cluster3_dataset <- Final_DF_NETWORK[Final_DF_NETWORK$ML_clusters == 3, ]
write.csv(cluster1_dataset, "cluster1_dataset.csv", row.names = FALSE)
write.csv(cluster2_dataset, "cluster2_dataset.csv", row.names = FALSE)
write.csv(cluster3_dataset, "cluster3_dataset.csv", row.names = FALSE)


###### Network for Cluster 1
### loading dataset and removing cluster column

Final_DF_NETWORK<-read.csv("cluster1_dataset.csv")

Final_DF_NETWORK_C1<- Final_DF_NETWORK[,-c(1:3)]
summary(Final_DF_NETWORK_C1)


Net_cluster1<-data.matrix(Final_DF_NETWORK_C1)
summary(Net_cluster1)

colnames(Final_DF_NETWORK_C1)



type<- c("c","c","c","c","c","c","c","c","c","g","g","c","c",
         "g","g","c","c","c","g","g","g","g","g","g","g","g")



# Initialize an empty vector to store the levels
level <- numeric(length(type))

# Iterate over each element in the type vector
for (i in seq_along(type)) {
  if (type[i] == "g") {  # Continuous variable
    level[i] <- 1
  } else if (type[i] == "c") {  # Categorical variable
    level[i] <- length(unique(Final_DF_NETWORK_C1[[names(Final_DF_NETWORK_C1)[i]]]))
  }
}

# Print the levels vector
print(level)


NGraph_cluster1<-estimateNetwork(Net_cluster1, type= type, default = "mgm", criterion =
                                   "EBIC", level = level,tuning = .5)



#### Bootstrap cluster 1
Bootstraps_cluster1<- bootnet(NGraph_cluster1, type= "case", nCores=10, nBoots = 1000)

boot_plot_Cluster1 <- plot(Bootstraps_cluster1, order = "sample",labels = T)


Bootstrap_C1_edge<- bootnet(NGraph_cluster1, nBoots = 1000,  nCores=10, type = "nonparametric")

Bootstrap_C1_edge_plot<-plot(Bootstrap_C1_edge,statistics= "strength", plot= "area", order = "sample",decreasing = T, labels = F)

CorStab_C1<-corStability(Bootstraps_cluster1,statistics = "all")

centralityPlot(NGraph_cluster1$graph, include = "all", orderBy = "Strength")

##### Network for Cluster 2

Final_DF_NETWORK<-read.csv("cluster2_dataset.csv")

Final_DF_NETWORK_C2<- Final_DF_NETWORK[,-c(1:3)]

summary(Final_DF_NETWORK_C2)





Net_cluster2<-data.matrix(Final_DF_NETWORK_C2)
summary(Net_cluster2)

colnames(Final_DF_NETWORK_C2)


type<- c("c","c","c","c","c","c","c","c","c","g","g","c","c",
         "g","g","c","c","c","g","g","g","g","g","g","g","g")



# Initialize an empty vector to store the levels
level <- numeric(length(type))

# Iterate over each element in the type vector
for (i in seq_along(type)) {
  if (type[i] == "g") {  # Continuous variable
    level[i] <- 1
  } else if (type[i] == "c") {  # Categorical variable
    level[i] <- length(unique(Final_DF_NETWORK_C2[[names(Final_DF_NETWORK_C2)[i]]]))
  }
}

# Print the levels vector
print(level)

NGraph_cluster2<-estimateNetwork(Net_cluster2, type= type, default = "mgm", criterion =
                                   "EBIC", level = level,tuning = 0.5)



#### bootstraps cluster 2
Bootstraps_cluster2<- bootnet(NGraph_cluster2, type= "case", nCores=10, nBoots = 1000)

boot_plot_Cluster2 <- plot(Bootstraps_cluster2, order = "sample", labels = T)

CorStab_C2<-corStability(Bootstraps_cluster2)


Bootstraps_cluster2_edge<- bootnet(NGraph_cluster2, type= "nonparametric", nCores=10, nBoots = 1000)

plot(Bootstraps_cluster2_edge,statistics= "strength",plot = "area", order = "sample",decreasing = T, labels = F)





################# cluster 3


Final_DF_NETWORK<-read.csv("cluster3_dataset.csv")

Final_DF_NETWORK_C3<- Final_DF_NETWORK[,-c(1:3)]

summary(Final_DF_NETWORK_C3)





Net_cluster3<-data.matrix(Final_DF_NETWORK_C3)
summary(Net_cluster3)

colnames(Final_DF_NETWORK_recode)



type<- c("c","c","c","c","c","c","c","c","c","g","g","c","c",
         "g","g","c","c","c","g","g","g","g","g","g","g","g")



# Initialize an empty vector to store the levels
level <- numeric(length(type))

# Iterate over each element in the type vector
for (i in seq_along(type)) {
  if (type[i] == "g") {  # Continuous variable
    level[i] <- 1
  } else if (type[i] == "c") {  # Categorical variable
    level[i] <- length(unique(Final_DF_NETWORK_C3[[names(Final_DF_NETWORK_C3)[i]]]))
  }
}

# Print the levels vector
print(level)

NGraph_cluster3<-estimateNetwork(Net_cluster3, type= type, default = "mgm", criterion =
                                   "EBIC", level = level,tuning = 0.5)



####Bootstraps cluster 3



Bootstraps_cluster3<- bootnet(NGraph_cluster3, type= "case", nCores=10, nBoots = 1000)
boot_plot_Cluster3 <- plot(Bootstraps_cluster3,statistics= "strength", order = "sample",labels = T)

CorStab_C3<-corStability(Bootstraps_cluster3)

summary(Bootstraps_cluster3)


Bootstraps_cluster3_edge<- bootnet(NGraph_cluster3, type= "nonparametric", nCores=10, nBoots = 1000)

plot(Bootstraps_cluster3_edge,statistics= "strength",plot = "area", order = "sample",decreasing = T, labels = F)


############### Network Graphs and Centrality Plots

##### Legend


Node_clusters<- c("ADHD Treatment Flag" ,"Depression Treatment Flag", "Anxiety Treatment Flag",
                  "PTSD Treatment Flag", "Social Anxiety Treatment Flag","Drug Treatment Flag",
                  "Suicidal Ideation Frequency 30-day  Treatment Flag","Suicidal Ideation Action 12-month  Treatment Flag","NSSI Treatment Flag"  ,
                  "Overal Physical Health Self Rate", "Overal Mental Health Self Rate" ,  "Meaningfulness of Leisure Time",
                  "Treatment Interest", "Physical-Mental Interference","Pain  Interference" ,
                  "Emotion Problem Panic (Lifetime)" ,"Psychological Tretment","Medication Tretment" ,
                  "Social Support-Feeling loved" ,"Social Support-Feeling lonely" , "Study Environment (Calm)"  ,
                  "Study Environment (Teachers)","Study Environment Collegues Enjoyment" , "Effort-Reward Imbalance" ,
                  "Grit","Well-eing")

### Node Labesls

Label_names_Clu<- c("ADHDTxF","DeprTxF","AnxTxF","PTSDTxF",
                    "SocAnxTxF","DRTxF","Ideat30dTxF","Ideat12mActTxF","NSSITxF", "PhysHlth",
                    "MentHlth","LeisureMean", "TxInterest", "PhysMentInterf", "PainInterf",
                    "PanicLT","PsychTxLT","MentTxLT","FeelLoved", "FeelLonely","StEnvCalm",
                    "StEnvTeacher", "StEnvColleagues","ERI",  "Grit", "Wellbeing")



#### Groups


groups_Clu<-list(
  "Wisdom"= c(13,17,25),
  "Emotion"= c(1:9,16,26),
  "Body"= c(10:11,14:15),
  "Social"= c(12,18:24))



graph_C1<- NGraph_cluster1$graph
graph_C2<- NGraph_cluster2$graph
graph_C3<- NGraph_cluster3$graph

## Same layout for comparison of networks


#' Creating hyperparameter *max_value*
max_value <- max(
  max(abs(graph_C1)), # from network 1
  max(abs(graph_C2)),
  max(abs(graph_C3))
)

max_value
#' Creating hyperparameter *net_layout*
net_layout <- averageLayout(NGraph_cluster1,
                            NGraph_cluster2,
                            NGraph_cluster3,
                            layout = "spring")



### Cluster 1 Network and Centrality plot


graph_Cluster1<- qgraph(graph_C1, groups= groups_Clu ,labels= Label_names_Clu,
                        legend=F, nodeNames= Node_clusters, layout= net_layout,
                        maximum = max_value,palette= "pastel",theme="colorblind",
                        vsize = 8,legend.mode= "style1", label.cex= 1.8,legend.cex= .32,
                        edge.labels=F,negDashed= T,layoutOffset= c(0,0),label.scale= T)
legend("topleft","Low presence and High seeking", cex = .8)
centralityPlot(graph_Cluster1, include = "all", orderBy = "Strength")

##### Cluster 2 Network and Centrality plot



graph_Cluster2<- qgraph(graph_C2, groups= groups_Clu ,labels= Label_names_Clu,
                        legend=F, nodeNames= Node_clusters, label.cex= 1.8,layout= net_layout,
                        maximum = max_value,palette= "pastel",theme="colorblind",
                        vsize = 8,legend.mode= "style1", legend.cex= .32,
                        edge.labels=F,negDashed= T,layoutOffset= c(0,0),label.scale= T)

legend("topleft","high presence and low seeking", cex = .8)
centralityPlot(graph_Cluster2, include = "all", orderBy = "Strength")


###### Cluster 3 Network and Centrality plot



graph_Cluster3<- qgraph(graph_C3, groups= groups_Clu ,labels= Label_names_Clu,
                        legend=F, nodeNames= Node_clusters,  label.cex= 1.8,layout= net_layout,
                        maximum = max_value,palette= "pastel",theme="colorblind",
                        vsize = 8,legend.mode= "style1", legend.cex= .32,
                        edge.labels=F,negDashed= T,layoutOffset= c(0,0),label.scale= T)
legend("topleft","High presence and High seeking", cex = .8)

centralityPlot(graph_Cluster3, include = "all", orderBy = "Strength")

######comparison of clusters strength centrality

pdf("comparing strength centrality plot.pdf", width = 25, height = 9)
plot(cenC1$value[53:78], type = "b",
     family= "sans",
     las= 1, lwd=1,cex=1, xaxt="n", xlab="Nodes", ylab = "Strength Centrality",col="green")
axis(side = 1, labels = Label_names_Clu, at= c(1:26))
lines(cenC2$value[53:78], type = "b", col= "red")
lines(cenC3$value[53:78], type = "b", col="blue")
legend("top",legend = c("Meaning Moratorium", "Meaning Foreclosure",
                        "Meaning Achievement"),horiz=T, lty= c("solid","solid","solid"), col = c("green","red","blue"))
dev.off()


#### Network Comparison

set.seed(123) # random permutation seed

#### comparison 1 vs 2
NCT_C1_C2<-NCT(NGraph_cluster1,NGraph_cluster2,it= 1000,
               test.centrality = T,test.edges = T, centrality = "strength",
               p.adjust.methods="BH", edges = "all")
NCT_C1_C2$nwinv.pval
NCT_C1_C2$glstrinv.pval
NCT_C1_C2$glstrinv.real
NCT_C1_C2$diffcen.real
plot(NCT_C1_C2)

M_C1_C2<-
  max(
    abs(c(NGraph_cluster1$graph) - c(NGraph_cluster2$graph))
  )

cat("The biggest edge difference is:", M_C1_C2)

S_C1_C2 <-
  abs(
    sum(
      abs(c(NGraph_cluster1$graph)) -
        abs(c(NGraph_cluster2$graph))
    )
  )/2

cat("Strength difference between the two networks is:", S_C1_C2)

###### Comparison C2 vs 3
NCT_C2_C3<-NCT(NGraph_cluster2,NGraph_cluster3,it= 1000,
               abs = T,test.centrality = T,test.edges = T, centrality = c("closeness",
                                                                          "betweenness",
                                                                          "strength",
                                                                          "expectedInfluence"),
               p.adjust.methods="BH", edges = "all")


NCT_C2_C3$nwinv.pval
NCT_C2_C3$glstrinv.pval
NCT_C2_C3$glstrinv.real
NCT_C2_C3$diffcen.real
NCT_C2_C3$einv.real
NCT_C2_C3$einv.pvals

plot(NCT_C2_C3)


M_C2_C3<-
  max(
    abs(c(NGraph_cluster2$graph) - c(NGraph_cluster3$graph))
  )

cat("The biggest edge difference is:", M_C2_C3)

S_C2_C3 <-
  abs(
    sum(
      abs(c(NGraph_cluster2$graph)) -
        abs(c(NGraph_cluster3$graph))
    )
  )/2

cat("Strength difference between the two networks is:", S_C2_C3)




##### C1 vs C3

NCT_C1_C3<-NCT(NGraph_cluster1,NGraph_cluster3,it= 1000,
               abs = T,test.centrality = T,test.edges = T, centrality = c("closeness",
                                                                          "betweenness",
                                                                          "strength",
                                                                          "expectedInfluence"),
               p.adjust.methods="BH", edges = "all")

NCT_C1_C3$nwinv.pval
NCT_C1_C3$glstrinv.pval
NCT_C1_C3$glstrinv.real
NCT_C1_C3$diffcen.real
NCT_C1_C3$einv.real
NCT_C1_C3$einv.pvals

plot(NCT_C1_C3)

M_C1_C3<-
  max(
    abs(c(NGraph_cluster1$graph) - c(NGraph_cluster3$graph))
  )

cat("The biggest edge difference is:", M_C1_C3)

S_C1_C2 <-
  abs(
    sum(
      abs(c(NGraph_cluster1$graph)) -
        abs(c(NGraph_cluster3$graph))
    )
  )/2

cat("Strength difference between the two networks is:", S_C2_C3)


###### END OF Data Analysis
