#### loading required packages
library("shiny")
library("readxl")
library("survival")
library("tidyverse")
library("reshape2")
library("ggplot2") 
library("dplyr", warn.conflicts=FALSE)   
library("reshape2") # Reformmating dataframes
library("grid")
library("plotly") # Allows us to make the swimmer plot interactive
library("knitr")
library("survminer")
library("gginnards")

#loading datasets
clinical_data <- read_excel("D:/RShniy/RShinyDashboards/Survival Analysis/Data/clinical_data.xlsx")
swimmer_data <- read.csv("D:/RShniy/RShinyDashboards/Survival Analysis/Data/swimmer_data.csv")

#Kaplan Choice Metrics
clin_df_names = names(clinical_data) #get column names
kaplan_metric_choices = c(clin_df_names[9], clin_df_names[5], clin_df_names[7], clin_df_names[6],
                             clin_df_names[8]) #ordered options for KPI selection


#data prep for kaplan model
modified_clinical <- clinical_data %>% 
  mutate(period = ifelse(`Vital Status`=="Dead", days_to_death, days_to_last_follow_up)) %>% 
  mutate(censor = ifelse(`Vital Status`=="Dead", 1, 0))



#defining not in
`%!in%` <- Negate(`%in%`)
