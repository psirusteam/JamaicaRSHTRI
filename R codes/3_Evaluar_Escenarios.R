############################################# 
### Item Analysis - ECLAC/STATIN          ###
###                                       ###
### Reproductive Health Survey 2021       ###                                       ###
### Creation of a welfare index           ###
###                                       ###
### Part three: assessment of escenarios  ###
###                                       ###
### ECLAC's Official Mission              ### 
### Author: Andrés Gutiérrez              ###
### Date: 2022                            ###
############################################# 

rm(list = ls())
options(psipen = -100, digits = 3)

# Leerr librerías ---------------------------------------------------------

library(WrightMap)
library(eRm)
library(ltm)
library(ggplot2)
library(foreign)
library(dplyr)
library(gridExtra)
library(CTT)
library(magrittr)
library(data.table)
library(survey)
library(FactoMineR)
library(factoextra)
library(homals)
library(corrplot)
library(srvyr)

# Leer base de datos. -----------------------------------------------------

data <- fread("Data/RHS2021_HH_WEALTH.dat") 


# Seleccionar columnas de interés  ----------------------------------------
escenario <- readRDS("Data/Escenario.rds") 

dat_1 <- data %>%
  dplyr::select(matches("hh_10")) 

# Renombrar columnas  -----------------------------------------------------

names(dat_1) <-
  c("[A] ELECTRIC STOVE",
    "[B] GAS STOVE",
    "[C] REFRIGERATOR",
    "[D] FREEZER (DEEP FREEZE)",
    "[E] MICROWAVE",
    "[F] RADIO",
    "[G] AIR CONDITIONER",
    "[H] ELECTRONIC GAMING EQUIPMENT",
    "[I] WASHING MACHINE",
    "[J] CLOTHES DRYER",
    "[K] ELECTRIC WATER HEATER",
    "[L] SOLAR WATER HEATER",
    "[M] COMPUTER (INCLUDING LAPTOP & TABLET)",
    "[N] TELEVISION",
    "[O] CABLE SERVICE",
    "[P] GENERATOR",
    "[Q] DISHWASHER",
    "[R] INTERNET WITHIN THE HOUSEHOLD",
    "[S] A WORKING MOTORCYCLE/MOTORBIKE",
    "[T] A WORKING MOTOR VEHICLE (CAR, VAN OR TR")



##########################################
## Diseño muestral

dat_2 <- bind_cols( dat_1 , escenario)
library(purrr)

resumen <-  map_df(names(escenario),function(by){
  map_df(names(dat_1),function(by2){
 a <- summary(lm(dat_2[[by2]] ~ dat_2[[by]]))$r.squared
    
      temp <- dat_2 %>% group_by_at(vars(by,by2)) %>% 
    tally() %>% 
      group_by_at(vars(by2)) %>% 
      mutate(prop = n/sum(n),
             Pregunta = by2,
             metodo = by,
             R2 = a) %>% ungroup()
  
  names(temp)[2] <- "Nivel_preg"  
  names(temp)[1] <- "Quantile"  
  temp
    })
})

resumen %>% distinct(Pregunta, metodo, R2) %>% 
  group_by( metodo ) %>% summarise(R2 = sum(R2))

map(names(dat_1),function(x){
  jpeg(filename = paste0("Output/",substr(x,1,20),".jpeg"),  
       width = 800, height = 600,  )
  boxplot(dat_2[[x]]~dat_2$score_acp, xlab = x,ylab = "")  
  dev.off()
})



  