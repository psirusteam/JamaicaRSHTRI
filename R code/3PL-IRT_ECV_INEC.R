############################################# 
### Análisis de ítems de la ECV 2014      ###
###                                       ###
### Creación de una escala de             ###
### equipamiento para la estratificación  ###
### del marco de muestreo                 ###
###                                       ###
### ECV - INEC                            ###
### ECLAC's Official Mission              ### 
### Author: Andrés Gutiérrez              ###
### Date: 2019                            ###
############################################# 

rm(list = ls())
options(psipen = -100, digits = 3)

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

data <- fread("Data/RHS2021_HH_WEALTH.dat") 


dat_1 <- data %>%
  dplyr::select(matches("hh_10"))
names(dat_1) <-
c(
"[A] ELECTRIC STOVE",
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
######################
### Teoría clásica ###
######################

names(dat_1)

#Reliability
cronbach.alpha(dat_1)  

# proporciones de bienestar
# proporciones de aciertos
prop <- colMeans(dat_1)
sort(prop, decreasing = T)
# varianza
var <- prop * (1 - prop)
sort(var, decreasing = T)

# proporciones estandarizadas
bienestar <- rowMeans(dat_1)
head(bienestar)
mean(bienestar)
sd(bienestar)

bienestar.est <- scale(bienestar)
hist(bienestar.est)

dificultad.est <- scale(prop)
hist(dificultad.est)

score <- 5 + 2 * bienestar.est
mean(score); sd(score)
min(score); max(score)

hist(score)
boxplot(score)

#######################
### Modelo de Rasch ###
#######################

res_rm_1 <- RM(dat_1)

pres <- person.parameter(res_rm_1)
theta.est <- pres$theta.table$`Person Parameter`
hist(theta.est)
plot(theta.est)
beta.est <- pres$betapar
hist(beta.est)
plot(beta.est)

bienestar.est <- scale(theta.est)
hist(bienestar.est)
dificultad.est <- scale(beta.est)
hist(dificultad.est)

score <- 5 + 2 * bienestar.est
hist(score)

for (i in 1:ncol(dat_1)) {
  plotICC(res_rm_1, item.subset = i, legend = TRUE)
  abline(h = 0.5, lty = 3, col = 2)
  abline(v = 0, lty = 2, col = 3)
}

plotjointICC(res_rm_1, cex = .4)
plotPImap(res_rm_1, cex.gen = .55, sorted = TRUE)

#######################
### 3PL de Birnbaum ###
#######################

#####################
# Análisis de ítems #
#####################

res_3pl_1 <- tpm(dat_1)
res_3pl_1
plot(res_3pl_1)

## Item Characteristic Curves
nrow(dat_1)
for (i in 1:ncol(dat_1)) {
  plot(res_3pl_1, items = i, legend = TRUE)
  abline(h = 0.5, lty = 3, col = 2)
  abline(v = 0, lty = 2, col = 3)
}

## Item Information Curves
plot(res_3pl_1, items = c(1:5), type = "IIC", legend = T)
plot(res_3pl_1, items = c(6:10), type = "IIC", legend = T)
plot(res_3pl_1, items = c(11:15), type = "IIC", legend = T)
plot(res_3pl_1, items = c(16:20), type = "IIC", legend = T)

## Test Information Function
plot(res_3pl_1, type = "IIC", items = 0, legend = T)
plot(res_3pl_1, type = "IIC", legend = T)

##########################
# Genración de la escala #
##########################

anaitem <- as.data.frame(coef(res_3pl_1))
rownames(anaitem)
anaitem[order(anaitem$Dscrmn),  ]
## Information at 3.5SD
anaitem$info <- NULL
anaitem$names <- NULL
for (i in 1:ncol(dat_1)) {
  anaitem$names[i] <- rownames(anaitem)[i] 
  anaitem$info[i] <- round(100 * unlist(information(res_3pl_1, c(-3.5,3.5), items = i))$PropRange)
}

arrange(anaitem, Dscrmn, info)

dat_1 %<>% dplyr::select(-c(6,4))

res_3pl_1 <- tpm(dat_1)
res_3pl_1
plot(res_3pl_1)

## Standardizing the scores and creating the index
pres <- factor.scores(res_3pl_1, dat_1)

theta.est <- pres$score.dat$z1
hist(theta.est)
plot(theta.est)

beta.est <- pres$coef[, 2]
hist(beta.est)
plot(beta.est)

c.est <- pres$coef[, 1]
hist(c.est)
plot(c.est)

a.est <- pres$coef[, 3]
hist(a.est)
plot(a.est)

wrightMap(theta.est, sort(beta.est), label.items.row = 3)

## Test Information Function
plot(res_3pl_1, type = "IIC", items = 0, legend = T)
plot(res_3pl_1, type = "IIC", legend = T)

## Análisis de ítems
anaitem <- as.data.frame(coef(res_3pl_1))
rownames(anaitem)
anaitem[order(anaitem$Dscrmn),  ]
## Information at 3.5SD
anaitem$info <- NULL
anaitem$names <- NULL
for (i in 1:ncol(dat_1)) {
  anaitem$names[i] <- rownames(anaitem)[i] 
  anaitem$info[i] <- round(100 * unlist(information(res_3pl_1, c(-3.5,3.5), items = i))$PropRange)
}

# Equating

bienestar.est <- scale(theta.est)
hist(bienestar.est)
dificultad.est <- scale(beta.est)
hist(dificultad.est)

score <- 5 + 2 * bienestar.est
mean(score)
sd(score)
hist(score, breaks = 10)
summary(score)

##########################################
## Diseño muestral
diseno <- dat_1 %>% 
  mutate(feh = data$hh_weight,
         score = score) %>% 
  svydesign(ids = ~1, weights = ~feh, data = .)

Quantile <- svyquantile(design = diseno,
            x = ~score, 
            quantiles =   seq(0, 1, by = 0.2))$score
#########################################

Quantil <- cut(score, breaks = Quantile[,1], 
    labels=paste0("Q",1:5), include.lowest=TRUE)
boxplot(score ~ Quantil)

# Cluster <- kmeans(score, 5)$cluster
# 
# boxplot(score ~ Cluster)
# 
# summary(Cluster)
# table(Cluster)

#######################
# Escogencia de ítems #
#######################

arrange(anaitem, (Dscrmn))
arrange(anaitem, (info))

anaitem1 <- anaitem %>%
  filter(info >= 95 & Dscrmn > 2) %>%
  arrange(Dffclt)

wrightMap(theta.est, sort(anaitem1$Dffclt), 
          label.items.row = 3)

anaitem2 <- anaitem %>%
  filter(info >= 95 | Dscrmn > 2) %>%
  arrange(Dffclt)

wrightMap(theta.est, sort(anaitem2$Dffclt), 
          label.items.row = 3)

