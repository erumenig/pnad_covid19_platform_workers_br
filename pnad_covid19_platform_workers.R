# << Dictionary >>

# << Sex and colour >>                                "A003" | "A004"     tabela
# << Schooling >>                                     "A005"              tabela
# << Income and sex >>                                "C01012" | "C003"   svyboxplot
# << Labour journey                                   "C008"              svyhist
# << Income by state (UF) >>                          "C01012" | UF       graph
# << Emergencial Aid (Covid-19                        "D0051"             svyhist
# << Delta income x Emergencial Aid >>                "D0053" | "C01012"  tabela
# << Plat: platform workers | PEA: Active workers
# The guide for this scrip was: << https://rpubs.com/gabriel-assuncao-ibge/covid >>

library(COVIDIBGE)
library(survey) # statistical
library(dplyr) # pacote geral para %>% pipe
library(grid) # necessary for survey package
library(Matrix) # necessary for survey package
library(survival) # necessary for survey package
library(srvyr) # permite usar dplyr para objetos survey
library(ggplot2) # graphs
library(tidyr)
library(hexbin)
library(ggsurvey)


# Getting help with the terms
# help("get_covid")

# Pivotal data for analysis
relevante_data <- c("UF", "V1022", "V1023", "A001A", "A001B3", "A003", "A004", "A005", "C007A", 
                    "C007B", "C007C", "C007D", "C008", "C009", "C01012", "C011A",
                    "C011A11", "C012", "C014", "D0011", "D0013", "D0021", "D0023", 
                    "D0031", "D0033", "D0041", "D0043", "D0051", "D0053", "D0061", 
                    "D0063", "D0071", "D0073", "F001", "F0021", "F0022", "F0061", 
                    "F006")

# Months: Labels = F show numbers | Labels = T description
pnadcovid_may <- get_covid(year=2020, month=5, vars=relevante_data, labels=T, design=T, deflator=T)
#pnadcovid_june <- get_covid(year=2020, month=6, vars=relevante_data, labels=T, design=T, deflator=T)
#pnadcovid_july <- get_covid(year=2020, month=7, vars=relevante_data, labels=T, design=T, deflator=T)
#pnadcovid_aug <- get_covid(year=2020, month=8, vars=relevante_data, labels=T, design=T, deflator=T)
#pnadcovid_sept <- get_covid(year=2020, month=9, vars=relevante_data, labels=T, design=T, deflator=T)
#pnadcovid_oct <- get_covid(year=2020, month=10, vars=relevante_data, labels=T, design=T, deflator=T)
pnadcovid_nov <- get_covid(year=2020, month=11, vars=relevante_data, labels=T, design=T, deflator=T)
#####################################

# driversplat
drivers_may <- subset(pnadcovid_may, C007C %in% "Motorista (de aplicativo, de taxi, de van, de mototáxi, de ônibus)" &
                        C007B %in% ("Não"))
#drivers_june <- subset(pnadcovid_june, C007C %in% "Motorista (de aplicativo, de taxi, de van, de mototáxi, de ônibus)" &
#                        C007B %in% ("Não"))
#drivers_july <- subset(pnadcovid_july, C007C %in% "Motorista (de aplicativo, de taxi, de van, de mototáxi, de ônibus)" &
#                        C007B %in% ("Não"))
#drivers_aug <- subset(pnadcovid_aug, C007C %in% "Motorista (de aplicativo, de taxi, de van, de mototáxi, de ônibus)" &
#                       C007B %in% ("Não"))
#drivers_sept <- subset(pnadcovid_sept, C007C %in% "Motorista (de aplicativo, de taxi, de van, de mototáxi, de ônibus)" &
#                        C007B %in% ("Não"))
#drivers_oct <- subset(pnadcovid_oct, C007C %in% "Motorista (de aplicativo, de taxi, de van, de mototáxi, de ônibus)" &
#                       C007B %in% ("Não"))
drivers_nov <- subset(pnadcovid_nov, C007C %in% "Motorista (de aplicativo, de taxi, de van, de mototáxi, de ônibus)" &
                        C007B %in% ("Não"))
#####################################
# deliveryplat
# drivers_plat << only platform drivers & informal workers >>
delivery_may <- subset(pnadcovid_may, C007C %in% "Entregador de mercadorias (de restaurante, de farmácia, de loja, Uber Eats, IFood, Rappy etc.)" &
                         C007B %in% ("Não"))
#delivery_june <- subset(pnadcovid_june, C007C %in% "Entregador de mercadorias (de restaurante, de farmácia, de loja, Uber Eats, IFood, Rappy etc.)" &
#                         C007B %in% ("Não"))
#delivery_july <- subset(pnadcovid_july, C007C %in% "Entregador de mercadorias (de restaurante, de farmácia, de loja, Uber Eats, IFood, Rappy etc.)" &
#                         C007B %in% ("Não"))
#delivery_aug <- subset(pnadcovid_aug, C007C %in% "Entregador de mercadorias (de restaurante, de farmácia, de loja, Uber Eats, IFood, Rappy etc.)" &
#                        C007B %in% ("Não"))
#delivery_sept <- subset(pnadcovid_sept, C007C %in% "Entregador de mercadorias (de restaurante, de farmácia, de loja, Uber Eats, IFood, Rappy etc.)" &
#                         C007B %in% ("Não"))
#delivery_oct <- subset(pnadcovid_oct, C007C %in% "Entregador de mercadorias (de restaurante, de farmácia, de loja, Uber Eats, IFood, Rappy etc.)" &
#                        C007B %in% ("Não"))
delivery_nov <- subset(pnadcovid_nov, C007C %in% "Entregador de mercadorias (de restaurante, de farmácia, de loja, Uber Eats, IFood, Rappy etc.)" &
                         C007B %in% ("Não"))

#####################################
# PEA Driver << formal drivers >>
pea_drivers_may <- subset(pnadcovid_may, C007C %in% "Motorista (de aplicativo, de taxi, de van, de mototáxi, de ônibus)" &
                        C007B %in% ("Sim, tem carteira de trabalho assinada") |
                        C007B %in% ("Sim, é servidor público estatutário"))

#pea_drivers_june <- subset(pnadcovid_june, C007C %in% "Motorista (de aplicativo, de taxi, de van, de mototáxi, de ônibus)" &
#                            C007B %in% ("Sim, tem carteira de trabalho assinada") |
#                            C007B %in% ("Sim, é servidor público estatutário"))
#pea_drivers_july <- subset(pnadcovid_july, C007C %in% "Motorista (de aplicativo, de taxi, de van, de mototáxi, de ônibus)" &
#                            C007B %in% ("Sim, tem carteira de trabalho assinada") |
#                            C007B %in% ("Sim, é servidor público estatutário"))
#pea_drivers_aug <- subset(pnadcovid_aug, C007C %in% "Motorista (de aplicativo, de taxi, de van, de mototáxi, de ônibus)" &
#                           C007B %in% ("Sim, tem carteira de trabalho assinada") |
#                           C007B %in% ("Sim, é servidor público estatutário"))
#pea_drivers_sept <- subset(pnadcovid_sept, C007C %in% "Motorista (de aplicativo, de taxi, de van, de mototáxi, de ônibus)" &
#                            C007B %in% ("Sim, tem carteira de trabalho assinada") |
#                            C007B %in% ("Sim, é servidor público estatutário"))
#pea_drivers_oct <- subset(pnadcovid_oct, C007C %in% "Motorista (de aplicativo, de taxi, de van, de mototáxi, de ônibus)" &
#                           C007B %in% ("Sim, tem carteira de trabalho assinada") |
#                           C007B %in% ("Sim, é servidor público estatutário"))
pea_drivers_nov <- subset(pnadcovid_nov, C007C %in% "Motorista (de aplicativo, de taxi, de van, de mototáxi, de ônibus)" &
                            C007B %in% ("Sim, tem carteira de trabalho assinada") |
                            C007B %in% ("Sim, é servidor público estatutário"))
#####################################

# PEA Delivery << formal delivery workers >>
pea_delivery_may <- subset(pnadcovid_may, C007C %in% "Entregador de mercadorias (de restaurante, de farmácia, de loja, Uber Eats, IFood, Rappy etc.)" |
                             C007C %in% ("Motoboy,") &
                             C007B %in% ("Sim, tem carteira de trabalho assinada") |
                             C007B %in% ("Sim, é servidor público estatutário"))

#pea_delivery_june <- subset(pnadcovid_june, C007C %in% "Entregador de mercadorias (de restaurante, de farmácia, de loja, Uber Eats, IFood, Rappy etc.)" |
#                            C007C %in% ("Motoboy,") &
#                            C007B %in% ("Sim, tem carteira de trabalho assinada") |
#                            C007B %in% ("Sim, é servidor público estatutário"))
#pea_delivery_july <- subset(pnadcovid_july, C007C %in% "Entregador de mercadorias (de restaurante, de farmácia, de loja, Uber Eats, IFood, Rappy etc.)" |
#                            C007C %in% ("Motoboy,") &
#                            C007B %in% ("Sim, tem carteira de trabalho assinada") |
#                            C007B %in% ("Sim, é servidor público estatutário"))
#pea_delivery_aug <- subset(pnadcovid_aug, C007C %in% "Entregador de mercadorias (de restaurante, de farmácia, de loja, Uber Eats, IFood, Rappy etc.)" |
#                            C007C %in% ("Motoboy,") &
#                            C007B %in% ("Sim, tem carteira de trabalho assinada") |
#                            C007B %in% ("Sim, é servidor público estatutário"))
#pea_delivery_sept <- subset(pnadcovid_sept, C007C %in% "Entregador de mercadorias (de restaurante, de farmácia, de loja, Uber Eats, IFood, Rappy etc.)" |
#                            C007C %in% ("Motoboy,") &
#                            C007B %in% ("Sim, tem carteira de trabalho assinada") |
#                            C007B %in% ("Sim, é servidor público estatutário"))
#pea_delivery_oct <- subset(pnadcovid_oct, C007C %in% "Entregador de mercadorias (de restaurante, de farmácia, de loja, Uber Eats, IFood, Rappy etc.)" |
#                            C007C %in% ("Motoboy,") &
#                            C007B %in% ("Sim, tem carteira de trabalho assinada") |
#                            C007B %in% ("Sim, é servidor público estatutário"))
pea_delivery_nov <- subset(pnadcovid_nov, C007C %in% "Entregador de mercadorias (de restaurante, de farmácia, de loja, Uber Eats, IFood, Rappy etc.)" |
                             C007C %in% ("Motoboy,") &
                             C007B %in% ("Sim, tem carteira de trabalho assinada") |
                             C007B %in% ("Sim, é servidor público estatutário"))
###################################################################################
# Defining WD and exporting DF for:
# SEX_RACE | Drivers PLAT
if(file.exists("~/sexrace_driverplat")){
  cat("The folder already exists")  
} else {
  dir.create("~/sexrace_driverplat")
}

setwd("~/sexrace_driverplat")

driver_sexoraca_may <- svytotal(x=~interaction(A003,A004), design=drivers_may, na.rm=TRUE)
#driver_sexoraca_june <- svytotal(x=~interaction(A003,A004), design=drivers_june, na.rm=TRUE)
#driver_sexoraca_july <- svytotal(x=~interaction(A003,A004), design=drivers_july, na.rm=TRUE)
#driver_sexoraca_aug <- svytotal(x=~interaction(A003,A004), design=drivers_aug, na.rm=TRUE)
#driver_sexoraca_sept <- svytotal(x=~interaction(A003,A004), design=drivers_sept, na.rm=TRUE)
#driver_sexoraca_oct <- svytotal(x=~interaction(A003,A004), design=drivers_oct, na.rm=TRUE)
driver_sexoraca_nov <- svytotal(x=~interaction(A003,A004), design=drivers_nov, na.rm=TRUE)

write.table(stat(driver_sexoraca_may),file="drivers_sexoraca_may.csv",sep=";")
#write.table(stat(driver_sexoraca_june),file="drivers_sexoraca_june.csv",sep=";")
#write.table(stat(driver_sexoraca_july),file="drivers_sexoraca_july.csv",sep=";")
#write.table(stat(driver_sexoraca_aug),file="drivers_sexoraca_aug.csv",sep=";")
#write.table(stat(driver_sexoraca_sept),file="drivers_sexoraca_sept.csv",sep=";")
#write.table(stat(driver_sexoraca_oct),file="drivers_sexoraca_oct.csv",sep=";")
write.table(stat(driver_sexoraca_nov),file="drivers_sexoraca_nov.csv",sep=";")

# Delivery PLAT
if(file.exists("~/sexrace_deliveryplat")){
  cat("The folder already exists")  
} else {
  dir.create("~/sexrace_deliveryplat")
}

setwd("~/sexrace_deliveryplat")

delivery_sexoraca_may <- svytotal(x=~interaction(A003,A004), design=delivery_may, na.rm=TRUE)
#delivery_sexoraca_june <- svytotal(x=~interaction(A003,A004), design=delivery_june, na.rm=TRUE)
#delivery_sexoraca_july <- svytotal(x=~interaction(A003,A004), design=delivery_july, na.rm=TRUE)
#delivery_sexoraca_aug <- svytotal(x=~interaction(A003,A004), design=delivery_aug, na.rm=TRUE)
#delivery_sexoraca_sept <- svytotal(x=~interaction(A003,A004), design=delivery_sept, na.rm=TRUE)
#delivery_sexoraca_oct <- svytotal(x=~interaction(A003,A004), design=delivery_oct, na.rm=TRUE)
delivery_sexoraca_nov <- svytotal(x=~interaction(A003,A004), design=delivery_nov, na.rm=TRUE)

write.table(stat(delivery_sexoraca_may),file="delivery_sexoraca_may.csv",sep=";")
#write.table(stat(delivery_sexoraca_june),file="delivery_sexoraca_june.csv",sep=";")
#write.table(stat(delivery_sexoraca_july),file="delivery_sexoraca_july.csv",sep=";")
#write.table(stat(delivery_sexoraca_aug),file="delivery_sexoraca_aug.csv",sep=";")
#write.table(stat(delivery_sexoraca_sept),file="delivery_sexoraca_sept.csv",sep=";")
#write.table(stat(delivery_sexoraca_oct),file="delivery_sexoraca_oct.csv",sep=";")
write.table(stat(delivery_sexoraca_nov),file="delivery_sexoraca_nov.csv",sep=";")

# PEA Drivers
if(file.exists("~/sexrace_driverpea")){
  cat("The folder already exists")  
} else {
  dir.create("~/sexrace_driverpea")
}

setwd("~/sexrace_driverpea")

pea_driver_sexoraca_may <- svytotal(x=~interaction(A003,A004), design=pea_drivers_may, na.rm=TRUE)
#pea_driver_sexoraca_june <- svytotal(x=~interaction(A003,A004), design=pea_drivers_june, na.rm=TRUE)
#pea_driver_sexoraca_july <- svytotal(x=~interaction(A003,A004), design=pea_drivers_july, na.rm=TRUE)
#pea_driver_sexoraca_aug <- svytotal(x=~interaction(A003,A004), design=pea_drivers_aug, na.rm=TRUE)
#pea_driver_sexoraca_sept <- svytotal(x=~interaction(A003,A004), design=pea_drivers_sept, na.rm=TRUE)
#pea_driver_sexoraca_oct <- svytotal(x=~interaction(A003,A004), design=pea_drivers_oct, na.rm=TRUE)
pea_driver_sexoraca_nov <- svytotal(x=~interaction(A003,A004), design=pea_drivers_nov, na.rm=TRUE)

write.table(stat(pea_driver_sexoraca_may),file="pea_drivers_sexoraca_may.csv",sep=";")
#write.table(stat(pea_driver_sexoraca_june),file="pea_drivers_sexoraca_june.csv",sep=";")
#write.table(stat(pea_driver_sexoraca_july),file="pea_drivers_sexoraca_july.csv",sep=";")
#write.table(stat(pea_driver_sexoraca_aug),file="pea_drivers_sexoraca_aug.csv",sep=";")
#write.table(stat(pea_driver_sexoraca_sept),file="pea_drivers_sexoraca_sept.csv",sep=";")
#write.table(stat(pea_driver_sexoraca_oct),file="pea_drivers_sexoraca_oct.csv",sep=";")
write.table(stat(pea_driver_sexoraca_nov),file="pea_drivers_sexoraca_nov.csv",sep=";")

# PEA Delivery
if(file.exists("~/sexrace_deliverypea")){
  cat("The folder already exists")  
} else {
  dir.create("~/sexrace_deliverypea")
}

setwd("~/sexrace_deliverypea")

pea_delivery_sexoraca_may <- svytotal(x=~interaction(A003,A004), design=pea_delivery_may, na.rm=TRUE)
#pea_delivery_sexoraca_june <- svytotal(x=~interaction(A003,A004), design=pea_delivery_june, na.rm=TRUE)
#pea_delivery_sexoraca_july <- svytotal(x=~interaction(A003,A004), design=pea_delivery_july, na.rm=TRUE)
#pea_delivery_sexoraca_aug <- svytotal(x=~interaction(A003,A004), design=pea_delivery_aug, na.rm=TRUE)
#pea_delivery_sexoraca_sept <- svytotal(x=~interaction(A003,A004), design=pea_delivery_sept, na.rm=TRUE)
#pea_delivery_sexoraca_oct <- svytotal(x=~interaction(A003,A004), design=pea_delivery_oct, na.rm=TRUE)
pea_delivery_sexoraca_nov <- svytotal(x=~interaction(A003,A004), design=pea_delivery_nov, na.rm=TRUE)

write.table(stat(pea_delivery_sexoraca_may),file="pea_delivery_sexoraca_may.csv",sep=";")
#write.table(stat(pea_delivery_sexoraca_june),file="pea_delivery_sexoraca_june.csv",sep=";")
#write.table(stat(pea_delivery_sexoraca_july),file="pea_delivery_sexoraca_july.csv",sep=";")
#write.table(stat(pea_delivery_sexoraca_aug),file="pea_delivery_sexoraca_aug.csv",sep=";")
#write.table(stat(pea_delivery_sexoraca_sept),file="pea_delivery_sexoraca_sept.csv",sep=";")
#write.table(stat(pea_delivery_sexoraca_oct),file="pea_delivery_sexoraca_oct.csv",sep=";")
write.table(stat(pea_delivery_sexoraca_nov),file="pea_delivery_sexoraca_nov.csv",sep=";")

###################################################################################
# SCHOOLING
# Drivers PLAT
if(file.exists("~/school_driverplat")){
  cat("The folder already exists")  
} else {
  dir.create("~/school_driverplat")
}

setwd("~/school_driverplat")

school_driver_may <- svytotal(x=~stat(A005), design=drivers_may, na.rm = T)
#school_driver_june <- svytotal(x=~stat(A005), design=drivers_june, na.rm = T)
#school_driver_july <- svytotal(x=~stat(A005), design=drivers_july, na.rm = T)
#school_driver_aug <- svytotal(x=~stat(A005), design=drivers_aug, na.rm = T)
#school_driver_sept <- svytotal(x=~stat(A005), design=drivers_sept, na.rm = T)
#school_driver_oct <- svytotal(x=~stat(A005), design=drivers_oct, na.rm = T)
school_driver_nov <- svytotal(x=~stat(A005), design=drivers_nov, na.rm = T)

write.table(stat(school_driver_may), file = "school_driver_may.cvs", sep = ";")
#write.table(stat(school_driver_june), file = "school_driver_june.cvs", sep = ";")
#write.table(stat(school_driver_july), file = "school_driver_july.cvs", sep = ";")
#write.table(stat(school_driver_aug), file = "school_driver_aug.cvs", sep = ";")
#write.table(stat(school_driver_sept), file = "school_driver_sept.cvs", sep = ";")
#write.table(stat(school_driver_oct), file = "school_driver_oct.cvs", sep = ";")
write.table(stat(school_driver_nov), file = "school_driver_nov.cvs", sep = ";")

# Delivery PLAT
if(file.exists("~/school_deliveryplat")){
  cat("The folder already exists")  
} else {
  dir.create("~/school_deliveryplat")
}

setwd("~/school_deliveryplat")

school_delivery_may <- svytotal(x=~stat(A005), design=delivery_may, na.rm = T)
#school_delivery_june <- svytotal(x=~stat(A005), design=delivery_june, na.rm = T)
#school_delivery_july <- svytotal(x=~stat(A005), design=delivery_july, na.rm = T)
#school_delivery_aug <- svytotal(x=~stat(A005), design=delivery_aug, na.rm = T)
#school_delivery_sept <- svytotal(x=~stat(A005), design=delivery_sept, na.rm = T)
#school_delivery_oct <- svytotal(x=~stat(A005), design=delivery_oct, na.rm = T)
school_delivery_nov <- svytotal(x=~stat(A005), design=delivery_nov, na.rm = T)

write.table(stat(school_delivery_may), file = "school_delivery_may.cvs", sep = ";")
#write.table(stat(school_delivery_june), file = "school_delivery_june.cvs", sep = ";")
#write.table(stat(school_delivery_july), file = "school_delivery_july.cvs", sep = ";")
#write.table(stat(school_delivery_aug), file = "school_delivery_aug.cvs", sep = ";")
#write.table(stat(school_delivery_sept), file = "school_delivery_sept.cvs", sep = ";")
#write.table(stat(school_delivery_oct), file = "school_delivery_oct.cvs", sep = ";")
write.table(stat(school_delivery_nov), file = "school_delivery_nov.cvs", sep = ";")

# Drivers PEA
if(file.exists("~/school_driverpea")){
  cat("The folder already exists")  
} else {
  dir.create("~/school_driverpea")
}

setwd("~/school_driverpea")

pea_school_driver_may <- svytotal(x=~stat(A005), design=pea_drivers_may, na.rm = T)
#pea_school_driver_june <- svytotal(x=~stat(A005), design=pea_drivers_june, na.rm = T)
#pea_school_driver_july <- svytotal(x=~stat(A005), design=pea_drivers_july, na.rm = T)
#pea_school_driver_aug <- svytotal(x=~stat(A005), design=pea_drivers_aug, na.rm = T)
#pea_school_driver_sept <- svytotal(x=~stat(A005), design=pea_drivers_sept, na.rm = T)
#pea_school_driver_oct <- svytotal(x=~stat(A005), design=pea_drivers_oct, na.rm = T)
pea_school_driver_nov <- svytotal(x=~stat(A005), design=pea_drivers_nov, na.rm = T)

write.table(stat(pea_school_driver_may), file = "pea_school_driver_may.cvs", sep = ";")
#write.table(stat(pea_school_driver_june), file = "pea_school_driver_june.cvs", sep = ";")
#write.table(stat(pea_school_driver_july), file = "pea_school_driver_july.cvs", sep = ";")
#write.table(stat(pea_school_driver_aug), file = "pea_school_driver_aug.cvs", sep = ";")
#write.table(stat(pea_school_driver_sept), file = "pea_school_driver_sept.cvs", sep = ";")
#write.table(stat(pea_school_driver_oct), file = "pea_school_driver_oct.cvs", sep = ";")
write.table(stat(pea_school_driver_nov), file = "pea_school_driver_nov.cvs", sep = ";")

# Delivery PEA
if(file.exists("~/school_deliverypea")){
  cat("The folder already exists")  
} else {
  dir.create("~/school_deliverypea")
}

setwd("~/school_deliverypea")

pea_school_delivery_may <- svytotal(x=~stat(A005), design=pea_delivery_may, na.rm = T)
#pea_school_delivery_june <- svytotal(x=~stat(A005), design=pea_delivery_june, na.rm = T)
#pea_school_delivery_july <- svytotal(x=~stat(A005), design=pea_delivery_july, na.rm = T)
#pea_school_delivery_aug <- svytotal(x=~stat(A005), design=pea_delivery_aug, na.rm = T)
#pea_school_delivery_sept <- svytotal(x=~stat(A005), design=pea_delivery_sept, na.rm = T)
#pea_school_delivery_oct <- svytotal(x=~stat(A005), design=pea_delivery_oct, na.rm = T)
pea_school_delivery_nov <- svytotal(x=~stat(A005), design=pea_delivery_nov, na.rm = T)

write.table(stat(pea_school_delivery_may), file = "pea_school_delivery_may.cvs", sep = ";")
#write.table(stat(pea_school_delivery_june), file = "pea_school_delivery_june.cvs", sep = ";")
#write.table(stat(pea_school_delivery_july), file = "pea_school_delivery_july.cvs", sep = ";")
#write.table(stat(pea_school_delivery_aug), file = "pea_school_delivery_aug.cvs", sep = ";")
#write.table(stat(pea_school_delivery_sept), file = "pea_school_delivery_sept.cvs", sep = ";")
#write.table(stat(pea_school_delivery_oct), file = "pea_school_delivery_oct.cvs", sep = ";")
write.table(stat(pea_school_delivery_nov), file = "pea_school_delivery_nov.cvs", sep = ";")


#########################################################################################
# GRAPHS
#########################################################################################
if(file.exists("~/images")){
  cat("The folder already exists")
} else{
  dir.create("~/images")
}

# Colour dictionary
# Driver PLAT       aquamarine3
# Driver PEA        honeydew
# Delivery PLAT     lightgoldenrod
# Delivery PEA      ligthyellow
######################################
# Sex | Income (BoxPlot)

# MAY
# DriversPLAT
svyboxplot(formula=C01012~A003, design=drivers_may, na.rm =T,
           ylim=c(0, 8000), 
           ylab="Renda mensal (R$)",
           xlab="Motoristas parceiros",
           col="aquamarine3")

# DriverPEA
svyboxplot(formula=C01012~A003, design=pea_drivers_may, na.rm =T,
           ylim=c(0, 8000), 
           ylab="Renda mensal (R$)", 
           xlab="Motoristas PEA",
           col="honeydew")

# DeliveryPLAT
svyboxplot(formula=C01012~A003, design=delivery_may, na.rm =T,
           ylim=c(0, 8000), 
           ylab="Renda mensal (R$)", 
           xlab="Entregadores parceiros",
           col="lightgoldenrod")

# DeliveryPEA
# There is no data for PEA Delivery Workers in May
# svyboxplot(formula=C01012~A003, design=pea_delivery_may, na.rm =T,
#          ylim=c(0, 8000),
#           ylab="Renda semanal (R$)", 
#           xlab= "Entregadores PEA",
#           col="ligthyellow")

# NOV
# DriversPLAT
svyboxplot(formula=C01012~A003, design=drivers_nov, na.rm =T,
           ylim=c(0, 8000), 
           ylab="Renda mensal (R$)",
           xlab="Motoristas parceiros",
           col="aquamarine3")

# DriverPEA
svyboxplot(formula=C01012~A003, design=pea_drivers_nov, na.rm =T,
           ylim=c(0, 8000), 
           ylab="Renda mensal (R$)", 
           xlab="Motoristas PEA",
           col="honeydew")

# DeliveryPLAT
svyboxplot(formula=C01012~A003, design=delivery_nov, na.rm =T,
           ylim=c(0, 8000), 
           ylab="Renda semanal (R$)", 
           xlab="Entregadores parceiros",
           col="lightgoldenrod")

# DeliveryPEA
# There is no data for PEA Delivery Workers in November
#svyboxplot(formula=C01012~A003, design=pea_delivery_nov, na.rm =T,
#           ylim=c(0, 8000), 
#           ylab="Renda semanal (R$)", 
#           xlab= "Entregadores PEA",
#           col="ligthyellow")

############################################################################
# GRAPH Income Sex (AVG)

# MAY
renda_sexo_drivermay <- svyby(formula= ~C01012, by = ~A003,
               design = drivers_may, FUN = svymean, na.rm=T)

renda_sexo_pea_drivermay <- svyby(formula= ~C01012, by = ~A003,
                              design = pea_drivers_may, FUN = svymean, na.rm=T)

renda_sexo_deliverymay <- svyby(formula= ~C01012, by = ~A003,
                                    design = delivery_may, FUN = svymean, na.rm=T)

renda_sexo_pea_deliverymay <- svyby(formula= ~C01012, by = ~A003,
                                design = pea_delivery_may, FUN = svymean, na.rm=T)

# Inflation correction -> Index -> IPCA IBGE << https://www3.bcb.gov.br/CALCIDADAO/publico/corrigirPorIndice.do?method=corrigirPorIndice >>

renda_sexo_driver_may <- renda_sexo_drivermay %>%
  mutate(renda = C01012*1,2313615) %>% 
  rename(sexo = "A003") %>% 
  select(sexo, renda)

renda_sexo_pea_driver_may <- renda_sexo_pea_drivermay %>%
  mutate(renda = C01012*1,2313615) %>% 
  rename(sexo = "A003") %>% 
  select(sexo, renda)

renda_sexo_delivery_may <- renda_sexo_deliverymay %>%  
  mutate(renda = C01012*1,2313615) %>% 
  rename(sexo = "A003") %>% 
  select(sexo, renda)

renda_sexo_pea_delivery_may <- renda_sexo_pea_deliverymay %>% 
  mutate(renda = C01012*1,2313615) %>% 
  rename(sexo = "A003") %>% 
  select(sexo, renda)


# NOV
renda_sexo_drivernov <- svyby(formula= ~C01012, by = ~A003,
                              design = drivers_nov, FUN = svymean, na.rm=T)

renda_sexo_pea_drivernov <- svyby(formula= ~C01012, by = ~A003,
                                   design = pea_drivers_nov, FUN = svymean, na.rm=T)

renda_sexo_deliverynov <- svyby(formula= ~C01012, by = ~A003,
                                design = delivery_nov, FUN = svymean, na.rm=T)

renda_sexo_pea_deliverynov <- svyby(formula= ~C01012, by = ~A003,
                                    design = pea_delivery_nov, FUN = svymean, na.rm=T)

# Inflation correction -> Index -> IPCA IBGE << https://www3.bcb.gov.br/CALCIDADAO/publico/corrigirPorIndice.do?method=corrigirPorIndice >>

renda_sexo_driver_nov <- renda_sexo_drivernov %>%
  mutate(renda = C01012*1,2313615) %>% 
  rename(sexo = "A003") %>% 
  select(sexo, renda)

renda_sexo_pea_driver_nov <- renda_sexo_pea_drivernov %>%
  mutate(renda = C01012*1,2313615) %>% 
  rename(sexo = "A003") %>% 
  select(sexo, renda)

renda_sexo_delivery_nov <- renda_sexo_deliverynov %>%  
  mutate(renda = C01012*1,2313615) %>% 
  rename(sexo = "A003") %>% 
  select(sexo, renda)

renda_sexo_pea_delivery_nov <- renda_sexo_pea_deliverynov %>% 
  mutate(renda = C01012*1,2313615) %>% 
  rename(sexo = "A003") %>% 
  select(sexo, renda)


###############################
# Graphs
# Colour dictionary
# Driver PLAT       Reds
# Driver PEA        Oranges
# Delivery PLAT     Greens
# Delivery PEA      Blues

# MAY
renda_sexo_driver_may %>% ggplot() +
  geom_col(aes(x= sexo, y= renda, fill=sexo)) +
  scale_y_continuous(limits = c(0, 6000), breaks = seq(0, 6000, by = 2000)) +
  theme_classic() +
  scale_fill_brewer(palette= "Reds") +
  guides(fill=guide_legend("Motoristas APP")) +
  labs(title = "", y = "Renda mensal (R$)", x = "") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="italic"),
        legend.title=element_text(size=14),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


renda_sexo_pea_driver_may %>% ggplot() +
  geom_col(aes(x= sexo, y= renda, fill=sexo)) +
  scale_y_continuous(limits = c(0, 6000), breaks = seq(0, 6000, by = 2000)) +
  theme_classic() +
  scale_fill_brewer(palette= "Oranges") +
  guides(fill=guide_legend("Motoristas PEA")) +
  labs(title = "", y = "Renda mensal (R$)", x = "") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="italic"),
        legend.title=element_text(size=14),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

renda_sexo_delivery_may %>% ggplot() +
  geom_col(aes(x= sexo, y= renda, fill=sexo)) +
  scale_y_continuous(limits = c(0, 6000), breaks = seq(0, 6000, by = 2000)) +
  theme_classic() +
  scale_fill_brewer(palette= "Greens") +
  guides(fill=guide_legend("Entregadores APP")) +
  labs(title = "", y = "Renda mensal (R$)", x = "") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="italic"),
        legend.title=element_text(size=14),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


renda_sexo_pea_delivery_may %>% ggplot() +
  geom_col(aes(x= sexo, y= renda, fill=sexo)) +
  scale_y_continuous(limits = c(0, 6000), breaks = seq(0, 6000, by = 2000)) +
  theme_classic() +
  scale_fill_brewer(palette= "Blues") +
  guides(fill=guide_legend("Entregadores PEA")) +
  labs(title = "", y = "Renda mensal (R$)", x = "") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="italic"),
        legend.title=element_text(size=14),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


# NOV
renda_sexo_driver_nov %>% ggplot() +
  geom_col(aes(x= sexo, y= renda, fill=sexo)) +
  scale_y_continuous(limits = c(0, 6000), breaks = seq(0, 6000, by = 2000)) +
  theme_classic() +
  scale_fill_brewer(palette= "Reds") +
  guides(fill=guide_legend("Motoristas APP")) +
  labs(title = "", y = "Renda mensal (R$)", x = "") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="italic"),
        legend.title=element_text(size=14),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


renda_sexo_pea_driver_nov %>% ggplot() +
  geom_col(aes(x= sexo, y= renda, fill=sexo)) +
  scale_y_continuous(limits = c(0, 6000), breaks = seq(0, 6000, by = 2000)) +
  theme_classic() +
  scale_fill_brewer(palette= "Oranges") +
  guides(fill=guide_legend("Motoristas PEA")) +
  labs(title = "", y = "Renda mensal (R$)", x = "") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="italic"),
        legend.title=element_text(size=14),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

renda_sexo_delivery_nov %>% ggplot() +
  geom_col(aes(x= sexo, y= renda, fill=sexo)) +
  scale_y_continuous(limits = c(0, 6000), breaks = seq(0, 6000, by = 2000)) +
  theme_classic() +
  scale_fill_brewer(palette= "Greens") +
  guides(fill=guide_legend("Entregadores APP")) +
  labs(title = "", y = "Renda mensal (R$)", x = "") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="italic"),
        legend.title=element_text(size=14),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


renda_sexo_pea_delivery_nov %>% ggplot() +
  geom_col(aes(x= sexo, y= renda, fill=sexo)) +
  scale_y_continuous(limits = c(0, 6000), breaks = seq(0, 6000, by = 2000)) +
  theme_classic() +
  scale_fill_brewer(palette= "Blues") +
  guides(fill=guide_legend("Entregadores PEA")) +
  labs(title = "", y = "Renda mensal (R$)", x = "") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="italic"),
        legend.title=element_text(size=14),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

############################################################################
# GRAPH: Income by state (UF)

# PLAT
avg_income_driver <- svyby(formula = ~C01012, by=~UF,
                           design = drivers_nov, FUN = svymean, na.rm = T)

avg_income_delivery <- svyby(formula = ~C01012, by=~UF,
                             design = delivery_nov, FUN = svymean, na.rm = T)

# PEA
avg_income_driver_pea <- svyby(formula = ~C01012, by=~UF,
                           design = pea_drivers_nov, FUN = svymean, na.rm = T)

avg_income_delivery_pea <- svyby(formula = ~C01012, by=~UF,
                           design = pea_delivery_nov, FUN = svymean, na.rm = T)

# Acronyms of the Brazilian states

estados <- c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN", "PB", 
             "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP", "PR", "SC", "RS", 
             "MS", "MT", "GO", "DF")

as.data.frame(estados)
print(estados)

# Then, bind UF's acronyms and correcting income by interest rate
# Inflation correction -> Index -> IPCA IBGE << https://www3.bcb.gov.br/CALCIDADAO/publico/corrigirPorIndice.do?method=corrigirPorIndice >>
avg_income_driver <- avg_income_driver %>% 
  mutate(renda = C01012*1,2313615) %>%
  bind_cols(estados) %>% 
  select(renda, UF)

avg_income_delivery <- avg_income_delivery %>% 
  mutate(renda = C01012*1,2313615) %>% # value correct by IPCA IBGE << https://www3.bcb.gov.br/CALCIDADAO/publico/corrigirPorIndice.do?method=corrigirPorIndice >>
  bind_cols(estados) %>% 
  select(renda, UF)

#######################
# AVG Estimation: INCOME 
avg_income_delivery_pea <- avg_income_delivery_pea %>% 
  mutate(renda = C01012*1,2313615) %>%
  bind_cols(estados) %>% 
  select(renda, UF)

avg_income_driver_pea <- avg_income_driver_pea %>% 
  mutate(renda = C01012*1,2313615) %>%
  bind_cols(estados) %>% 
  select(renda, UF)

################
# Graph DRIVER PLAT

avg_income_driver %>% ggplot() +
  geom_col(aes(x= UF, y= renda, fill=renda)) +
  scale_y_continuous(limits = c(0, 6000), breaks = seq(0, 6000, by = 2000)) +
  theme_classic() +
  labs(title = "", y = "Renda mensal (R$)", x = "UF", fill = "UF") +
  scale_fill_gradient2(low="light blue",
                       high = "dark blue") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.title=element_text(size=14),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


# Graph DELIVERY PLAT
avg_income_delivery %>% ggplot() +
  geom_col(aes(x= UF, y= renda, fill=renda)) +
  scale_y_continuous(limits = c(0, 6000), breaks = seq(0, 6000, by = 2000)) +
  theme_classic() +
  scale_fill_gradient2(low="white",
                       high = "red") +
  labs(title = "", y = "Renda mensal (R$)", x = "UF", fill = "UF") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.title=element_text(size=14),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

################################################################################
# GRAPH Labour journey
# Colour dictionary
# Driver PLAT       aquamarine3
# Driver PEA        honeydew
# Delivery PLAT     lightgoldenrod
# Delivery PEA      ligthyellow

# MAY
# Drivers
svyhist(formula=~as.numeric(C008), design=drivers_may, main = " ",
        xlab="Tempo disponível ao trabalho (h)", ylab="Densidade", 
        ylim=c(0.00, 0.12), 
        xlim=c(0, 100),
        col="aquamarine3")

svyhist(formula=~as.numeric(C008), design=pea_drivers_may, main = " ",
        xlab="Tempo disponível ao trabalho (h)", ylab="Densidade", 
        ylim=c(0.00, 0.12), 
        xlim=c(0, 100),
        col="honeydew")

# Delivery
svyhist(formula=~as.numeric(C008), design=delivery_may, main = " ",
        xlab="Tempo disponível ao trabalho (h)", ylab="Densidade", 
        ylim=c(0.00, 0.12), 
        xlim=c(0, 100),
        col="lightgoldenrod")

# There is no data for delivery May
# svyhist(formula=~as.numeric(C008), design=pea_delivery_may, main = " ",
#        xlab="Tempo disponível ao trabalho (h)", ylab="Densidade", 
#        ylim=c(0.00, 0.12),
#        xlim=c(0, 100),
#        col="ligthyellow")

# NOV
# Drivers
svyhist(formula=~as.numeric(C008), design=drivers_nov, main = " ",
        xlab="Tempo disponível ao trabalho (h)", ylab="Densidade", 
        ylim=c(0.00, 0.12), 
        xlim=c(0, 100),
        col="aquamarine3")

svyhist(formula=~as.numeric(C008), design=pea_drivers_nov, main = " ",
        xlab="Tempo disponível ao trabalho (h)", ylab="Densidade", 
        ylim=c(0.00, 0.12), 
        xlim=c(0, 100),
        col="honeydew")

# Delivery
svyhist(formula=~as.numeric(C008), design=delivery_nov, main = " ",
        xlab="Tempo disponível ao trabalho (h)", ylab="Densidade", 
        ylim=c(0.00, 0.12), 
        xlim=c(0, 100),
        col="lightgoldenrod")

# There is no data for delivery Nov
# svyhist(formula=~as.numeric(C008), design=pea_delivery_nov, main = " ",
#        xlab="Tempo disponível ao trabalho (h)", ylab="Densidade", 
#       ylim=c(0.00, 0.12), 
#        xlim=c(0, 100),
#       col="ligthyellow")


############################################################################
# << Emergencial Aid (Covid-19) -> Variable "D0051" >>
# << The graphs show whether workers received Emergency Assistance during the pandemic period >>
# Setting WD
if(file.exists("~/aux_nov")){
  cat("The folder already exists")
} else{
  dir.create("~/aux_nov")
}

setwd("~/aux_nov")

driver_aux_nov <- svytotal(x=~stat(D0051), design = drivers_nov, na.rm = T)
delivery_aux_nov <- svytotal(x=~stat(D0051), design = delivery_nov, na.rm = T)

write.table(stat(driver_aux_nov), file = "driver_aux_nov.cvs", sep = ";")
write.table(stat(delivery_aux_nov), file = "delivery_aux_nov.cvs", sep = ";")

driver_aux_nov <- read.csv("~/aux_nov/driver_aux_nov.cvs", header = T, sep = ";")
delivery_aux_nov <- read.csv("~/aux_nov/delivery_aux_nov.cvs", header = T, sep = ";")


driver_aux_nov <- tibble::rownames_to_column(driver_aux_nov, "aux") 
driver_aux_nov <- driver_aux_nov %>% 
  mutate(aux = replace(aux, aux == "stat(D0051)Sim", "Sim"),
         aux = replace(aux, aux == "stat(D0051)Não", "Não"))

# Delivery_nov

delivery_aux_nov <- tibble::rownames_to_column(delivery_aux_nov, "aux") 
delivery_aux_nov <- delivery_aux_nov %>% 
  mutate(aux = replace(aux, aux == "stat(D0051)Sim", "Sim"),
         aux = replace(aux, aux == "stat(D0051)Não", "Não")) %>% 
  filter(!is.na(total))

driver_aux_nov %>% ggplot + 
  geom_col(aes(x=aux, y=total, fill=aux)) +
  scale_y_continuous(limits = c(0, 140000), breaks = seq(0, 140000, by = 20000)) +
  theme_classic() +
  scale_fill_brewer(palette= "Reds") +
  guides(fill=guide_legend("Motoristas APP")) +
  labs(title = "", y = "Auxílio emergencial (n)", x = "") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="italic"),
        legend.title=element_text(size=14),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

delivery_aux_nov %>% ggplot + 
  geom_col(aes(x=aux, y=total, fill=aux)) +
  scale_y_continuous(limits = c(0, 140000), breaks = seq(0, 140000, by = 20000)) +
  theme_classic() +
  scale_fill_brewer(palette= "Greens") +
  guides(fill=guide_legend("Entregadores APP")) +
  labs(title = "", y = "Auxílio emergencial (n)", x = "") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="italic"),
        legend.title=element_text(size=14),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

###############################################################################
# << Delta Income x Emergency AID -> Variables: "D0053" | "C01012"

if(file.exists("~/aux_nov")){
  cat("The folder already exists")
} else{
  dir.create("~/aux_nov")
}

setwd("~/aux_nov")

aux_driver_nov <- svymean(x=~stat(D0053), design = drivers_nov, na.rm=T)
aux_delivery_nov <- svymean(x=~stat(D0053), design = delivery_nov, na.rm=T)

renda_driver_nov <- svymean(x=~stat(C01012), design = drivers_nov, na.rm=T)
renda_delivery_nov <- svymean(x=~stat(C01012), design = delivery_nov, na.rm=T)

write.table(aux_driver_nov, file = "aux_driver_nov.cvs", sep = ";")
write.table(aux_driver_nov, file = "aux_delivery_nov.cvs", sep = ";")

write.table(renda_driver_nov, file = "renda_driver_nov.cvs", sep = ";")
write.table(renda_driver_nov, file = "renda_delivery_nov.cvs", sep = ";")

aux_driver_nov <- read.csv("~/aux_nov/aux_driver_nov.cvs", header = T, sep = ";")
aux_delivery_nov <- read.csv("~/aux_nov/aux_delivery_nov.cvs", header = T, sep = ";")

renda_driver_nov <- read.csv("~/aux_nov/renda_driver_nov.cvs", header = T, sep = ";")
renda_delivery_nov <- read.csv("~/aux_nov/renda_delivery_nov.cvs", header = T, sep = ";")

renda_aux_driver <- aux_driver_nov %>% 
  bind_cols(renda_driver_nov) %>% 
  rename(aux ="mean...1",
         renda = mean...3) %>% 
  mutate(aux = aux**1,2313615) %>% 
  mutate(renda = renda**1,2313615)

renda_aux_delivery <- aux_delivery_nov %>% 
  bind_cols(renda_delivery_nov) %>% 
  rename(aux ="mean...1",
         renda = mean...3) %>% 
  mutate(aux = aux**1,2313615) %>% 
  mutate(renda = renda**1,2313615)

##################################################################################
# For report
avg_renda_texto_driver <- avg_income_driver %>% 
  arrange(renda)

avg_renda_texto_delivery <- avg_income_delivery %>% 
  arrange(renda)

avg_renda_texto_delivery_pea <- avg_income_delivery_pea %>% 
  arrange(renda)

avg_renda_texto_driver_pea <- avg_income_driver_pea %>% 
  arrange(renda)

# AVG income by state (UF)
mean(avg_income_delivery$renda)
mean(avg_income_driver$renda)
mean(avg_income_delivery_pea$renda)
mean(avg_income_driver_pea$renda)

print(avg_renda_texto_driver)
print(avg_renda_texto_delivery)
print(avg_renda_texto_delivery_pea)
print(avg_renda_texto_driver_pea)

# AVG income by worker category
print(mean(avg_income_delivery$renda))
print(mean(avg_income_driver$renda))
print(mean(avg_renda_texto_driver_pea$renda))
print(mean(avg_renda_texto_delivery_pea$renda))

# AVG labour journey by month
# MAY
# avg
avg_time_driver_may <- svymean(x=~C008, design = drivers_may, na.rm = T)
avg_time_delivery_may <- svymean(x=~C008, design = delivery_may, na.rm = T)
avg_time_pea_driver_may <- svymean(x=~C008, design = pea_drivers_may, na.rm = T)
avg_time_pea_delivery_may <- svymean(x=~C008, design = pea_delivery_may, na.rm = T)

# Median 
median_time_driver_may <- svyquantile(x=~C008, 
                                      design = drivers_may, 
                                      quantiles = 0.5,
                                      ci=T, na.rm=T)
median_time_delivery_may <- svyquantile(x=~C008, design = delivery_may, 
                                        quantiles = 0.5,
                                        ci=T, na.rm=T)
median_time_pea_driver_may <- svyquantile(x=~C008, 
                                          design = pea_drivers_may, 
                                          quantiles = 0.5,
                                          ci=T, na.rm=T)
median_time_pea_delivery_may <- svyquantile(x=~C008, 
                                            design = pea_delivery_may, 
                                            quantiles = 0.5,
                                            ci=T, na.rm=T)

# Prints AVG | Median labour journey by month
# MAY
print(avg_time_driver_may)
print(avg_time_delivery_may)
print(avg_time_pea_driver_may)
print(avg_time_pea_delivery_may)

print(median_time_driver_may)
print(median_time_delivery_may)
print(median_time_pea_driver_may)
print(median_time_pea_delivery_may)

# Prints AVG | Median labour journey by month
# NOV
avg_time_driver_nov <- svymean(x=~C008, design = drivers_nov, na.rm = T)
avg_time_delivery_nov <- svymean(x=~C008, design = delivery_nov, na.rm = T)
avg_time_pea_driver_nov <- svymean(x=~C008, design = pea_drivers_nov, na.rm = T)
avg_time_pea_delivery_nov <- svymean(x=~C008, design = pea_delivery_nov, na.rm = T)

median_time_driver_nov <- svyquantile(x=~C008, 
                                      design = drivers_nov, 
                                      quantiles = 0.5,
                                      ci=T, na.rm=T)
median_time_delivery_nov <- svyquantile(x=~C008, design = delivery_nov, 
                                        quantiles = 0.5,
                                        ci=T, na.rm=T)
median_time_pea_driver_nov <- svyquantile(x=~C008, 
                                          design = pea_drivers_nov, 
                                          quantiles = 0.5,
                                          ci=T, na.rm=T)
median_time_pea_delivery_nov <- svyquantile(x=~C008, 
                                            design = pea_delivery_nov, 
                                            quantiles = 0.5,
                                            ci=T, na.rm=T)

# Prints
print(avg_time_driver_nov)
print(avg_time_delivery_nov)
print(avg_time_pea_driver_nov)
print(avg_time_pea_delivery_nov)

print(median_time_driver_nov)
print(median_time_delivery_nov)
print(median_time_pea_driver_nov)
print(median_time_pea_delivery_nov)

######################## END ############################################