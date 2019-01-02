# **********************************************************************************************
# Some Visualisations
# INPUT: data frame
# OUTPUT : graphs
# **********************************************************************************************

# Installing packages
install.packages("magrittr")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("miscset")

# Loading packages
library(magrittr)
library(tidyverse)
library(dplyr)
library(miscset)

# Churning with regards to Gender
ggplot(df_reg) + geom_bar(aes(x=gender, fill = Churn), position = "dodge") + 
  scale_fill_manual("Churn", values = c("Green", "Blue"))

# Number of people from each gender that churned
df_reg %>% group_by(gender,Churn) %>% summarise(n=n()) %>% mutate(freq = n / sum(n))

# Churning with regards to Senior Citizens
ggplot(df_reg) + geom_bar(aes(x=SeniorCitizen, fill = Churn), position = "dodge") + 
  scale_fill_manual("Churn", values = c("Pink", "Purple"))

# Total number of senior citizens who churned
df_reg %>% group_by(SeniorCitizen,Churn) %>% summarise(n=n()) %>% mutate(freq = n / sum(n))

# Churning with regards to Partner
ggplot(df_reg) + geom_bar(aes(x=Partner, fill = Churn), position = "dodge") + 
  scale_fill_manual("Churn", values = c("Black", "Grey"))

# Total number of senior citizens who churned
df_reg %>% group_by(Partner,Churn) %>% summarise(n=n()) %>% mutate(freq = n / sum(n))

# Churning with regards to Dependents
ggplot(df_reg) + geom_bar(aes(x=Dependents, fill = Churn), position = "dodge") + 
  scale_fill_manual("Churn", values = c("red", "yellow"))

# Total number of senior citizens who churned
df_reg %>% group_by(Dependents,Churn) %>% summarise(n=n()) %>% mutate(freq = n / sum(n))

# Churning with regards to Tenure
ggplot(df_reg) + geom_bar(aes(x=tenure_group, fill = Churn), position = "dodge") + 
  scale_fill_manual("Churn", values = c("red", "yellow"))

# Churning with regards to Contract Type
ggplot(df_reg) + geom_bar(aes(x=Contract, fill = Churn), position = "dodge") + 
  scale_fill_manual("Churn", values = c("black", "green"))

# Churning with regards to Internet Service
ggplot(df_reg) + geom_bar(aes(x=InternetService, fill = Churn), position = "dodge") + 
  scale_fill_manual("Churn", values = c("grey", "purple"))

# Churning with regards to Payment Method
ggplot(df_reg) + geom_bar(aes(x=PaymentMethod, fill = Churn), position = "dodge") + 
  scale_fill_manual("Churn", values = c(rainbow(2)))

# Churning with regards to Online Security
ggplot(df_reg) + geom_bar(aes(x=OnlineSecurity, fill = Churn), position = "dodge") + 
  scale_fill_manual("Churn", values = c("brown", "pink"))

# Comparing total charges for each subset of people who churn

# Senior Citizens
df_reg %>% dplyr::select(SeniorCitizen, TotalCharges, Churn) %>%
  filter(SeniorCitizen==1, Churn=="Yes") %>%
  summarise(n=n(), total = sum(TotalCharges))

# With no partners
df_reg %>% dplyr::select(Partner, TotalCharges, Churn) %>%
  filter(Partner=="No", Churn=="Yes") %>%
  summarise(n=n(), total = sum(TotalCharges))

# With no Dependents
df_reg %>% dplyr::select(Dependents, TotalCharges, Churn) %>%
  filter(Dependents=="No", Churn=="Yes") %>%
  summarise(n=n(), total = sum(TotalCharges))

# Churn with respect to no dependents 
df_dependents <- df_reg %>% filter(Dependents=="No")

# Plotting churn with no dependents
ggplotGrid(ncol = 2, lapply(c("PhoneService","MultipleLines","InternetService"), 
                            function(col)
                            {
                              ggplot(df_dependents,aes_string(col)) + geom_bar(aes(fill=Churn), position = "dodge") + 
                                scale_fill_manual("Churn", values = c("Brown", "Yellow"))
                            }))

ggplotGrid(ncol = 2, lapply(c("OnlineSecurity","OnlineBackup","DeviceProtection","TechSupport"), 
                            function(col)
                            {
                              ggplot(df_dependents,aes_string(col)) + geom_bar(aes(fill=Churn), position = "dodge") + 
                                scale_fill_manual("Churn", values = c("Brown", "Yellow"))
                            }))

ggplotGrid(ncol = 2, lapply(c("StreamingTV","StreamingMovies","Contract","PaperlessBilling"), 
                            function(col)
                            {
                              ggplot(df_dependents,aes_string(col)) + geom_bar(aes(fill=Churn), position = "dodge") + 
                                scale_fill_manual("Churn", values = c("Brown", "Yellow"))
                            }))

# Payment method preference affecting churn
ggplot(df_dependents) + geom_bar(aes(x=PaymentMethod, fill=Churn), position = "dodge") +
  scale_fill_manual("Churn", values = c("Blue", "Magenta"))

