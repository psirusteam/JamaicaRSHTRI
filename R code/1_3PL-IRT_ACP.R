############################################# 
### Análisis de ítems de la ECV 2014      ###
###                                       ###
### Creación de una escala de             ###
### equipamiento para la estratificación  ###
### del marco de muestreo                 ###
###                                       ###
### ECLAC's Official Mission              ### 
### Author: Andrés Gutiérrez              ###
### Date: 2019                            ###
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

# Leer base de datos. -----------------------------------------------------

data <- fread("Data/RHS2021_HH_WEALTH.dat") 


# Seleccionar columnas de interés  ----------------------------------------

dat_1 <- data %>%
  dplyr::select(matches("hh_10"))

# Renombrar columnas  -----------------------------------------------------

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

###########################################
### Análisis de componentes principales ###
###########################################
M <- cor(dat_1)
dimnames(M)[[1]] <- substr(names(dat_1),1,3)
dimnames(M)[[2]] <- substr(names(dat_1),1,3)
corrplot(M, order = 'FPC', addCoef.col = 'black',
         tl.pos = 'd',
         cl.pos = 'n', col = COL2('PiYG'))

acp_1 <- PCA(dat_1, ncp = 2, graph = FALSE)

acp_1$var$contrib %>% data.frame() %>% arrange(desc( Dim.1))

fviz_pca_var(acp_1, col.var = "contrib",
             gradient.cols = c("white", "black"),
             ggtheme = theme_minimal())
fviz_pca_biplot(acp_1)

# seleccionando la primera componente.
dim1 <- as.numeric(scale(acp_1$ind$coord[,1]))
# escala de mu = 5 y sigmas = 2
score_acp1 <- 5 + 2*(dim1)
hist(score_acp1)
boxplot(score_acp1)

##################################################
### Análisis de componentes principales homals ###
##################################################
data_homal <- homals(dat_1, ndim = 2, level = "nominal")
acp_2 <- PCA(-data_homal$scoremat[,,1], ncp = 2, graph = FALSE)

acp_2$var$contrib %>% data.frame() %>% arrange(desc( Dim.1))

fviz_pca_var(acp_2, col.var = "contrib",
             gradient.cols = c("white", "black"),
             ggtheme = theme_minimal())
fviz_pca_biplot(acp_2)

# seleccionando la primera componente.
dim1 <- as.numeric(scale(acp_2$ind$coord[,1]))
# escala de mu = 5 y sigmas = 2
score_acp2 <- 5 + 2*(dim1)
hist(score_acp2)
boxplot(score_acp2)

######################
### Teoría clásica ###
######################

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

score_RM <- 5 + 2 * bienestar.est
hist(score_RM)

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
# Generación de la escala #
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

# Equating

bienestar.est <- scale(theta.est)
hist(bienestar.est)
dificultad.est <- scale(beta.est)
hist(dificultad.est)

score_TRI <- 5 + 2 * bienestar.est
mean(score_TRI)
sd(score_TRI)
hist(score_TRI, breaks = 10)
summary(score_TRI)

##########################################
## Diseño muestral
diseno <- dat_1 %>% 
  mutate(feh = data$hh_weight,
         score_acp = score_acp1,
         score_homals = score_acp2,
         score_RM = score_RM,
         score_TRI = score_TRI) %>% 
  svydesign(ids = ~1, weights = ~feh, data = .)

Quantile_acp1 <- svyquantile(
  design = diseno,
  x = ~ score_acp,
  quantiles =   seq(0, 1, by = 0.2)
)$score

Quantile_acp2 <- svyquantile(
  design = diseno,
  x = ~ score_homals,
  quantiles =   seq(0, 1, by = 0.2)
)$score


Quantile_RM <- svyquantile(
  design = diseno,
  x = ~ score_RM,
  quantiles =   seq(0, 1, by = 0.2)
)$score

Quantile_TRI <- svyquantile(
  design = diseno,
  x = ~ score_TRI,
  quantiles =   seq(0, 1, by = 0.2)
)$score
#########################################

Quantil_acp1 <- cut(score_acp1, breaks = Quantile_acp1[,1], 
    labels=paste0("Q",1:5), include.lowest=TRUE)

Quantil_acp2 <- cut(score_acp2, breaks = Quantile_acp2[,1], 
                   labels=paste0("Q",1:5), include.lowest=TRUE)

Quantil_RM <- cut(score_RM, breaks = Quantile_RM[,1], 
                   labels=paste0("Q",1:5), include.lowest=TRUE)

Quantil_TRI <- cut(score_TRI, breaks = Quantile_TRI[,1], 
                   labels=paste0("Q",1:5), include.lowest=TRUE)

score <- data.frame(metodo = gl(k = length(Quantil_acp1),n = 4,
                                labels =c("ACP","Homals","RM", "TRI")),
                    score = c(score_acp1,score_acp2,score_RM,score_TRI),
                    Quantile = c(Quantil_acp1,Quantil_acp2,Quantil_RM, Quantil_TRI)
                    )
ggplot(data = score, aes(x = Quantile, y = score,fill = metodo)) +
  geom_boxplot() + theme_minimal()

ggplot(data = score, aes(x = score,color = metodo)) +
  geom_density(size = 2, adjust = 1) + theme_minimal()

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
c(Quantil_acp1,Quantil_acp2,Quantil_RM, Quantil_TRI)

dat_1 %>% select(anaitem2$names) %>% 
  mutate(score_acp = Quantil_acp1,
         score_homals = Quantil_acp2,
         score_RM = Quantil_RM,
         score_TRI = Quantil_TRI) %>% 
  saveRDS(object = ., file = "Data/RHS2021_HH_recortada.rds")



