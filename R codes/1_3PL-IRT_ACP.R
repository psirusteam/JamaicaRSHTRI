#############################################
### Item Analysis - ECLAC/STATIN          ###
###                                       ###
### Reproductive Health Survey 2021       ###
### Creation of a welfare index           ###
###                                       ###
### Part one: every item matters          ###
###                                       ###
### ECLAC's Official Mission              ###
### Author: Andrés Gutiérrez              ###
### Date: 2022                            ###
#############################################

rm(list = ls())
options(psipen = -100, digits = 3)

# Reading libraries ---------------------------------------------------------

library(WrightMap)
library(eRm)
library(ltm)
library(CTT)

library(ggplot2)
library(foreign)
library(dplyr)
library(gridExtra)

library(magrittr)
library(data.table)
library(survey)

library(FactoMineR)
library(factoextra)
library(homals)
library(corrplot)

# Reading the database -----------------------------------------------------

data <- fread("Data/RHS2021_HH_WEALTH.dat")

# Selecting hh_10 questions  ----------------------------------------

dat_1 <- data %>%
  dplyr::select(matches("hh_10"))

# Renaming the columns to keep the labels  -----------------------------------------------------

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
    "[T] A WORKING MOTOR VEHICLE (CAR, VAN OR TR"
  )


# 1. PCA Principal Components Analysis -----------------------------


M <- cor(dat_1)
dimnames(M)[[1]] <- substr(names(dat_1), 1, 3)
dimnames(M)[[2]] <- substr(names(dat_1), 1, 3)
corrplot(M)

acp_1 <- PCA(dat_1, ncp = 2, graph = FALSE)

acp_1$var$contrib %>% data.frame() %>% arrange(desc(Dim.1))

fviz_pca_var(
  acp_1,
  col.var = "contrib",
  gradient.cols = c("white", "black"),
  ggtheme = theme_minimal()
)
fviz_pca_biplot(acp_1)

# Selecting the first component
dim1 <- as.numeric(scale(acp_1$ind$coord[, 1]))
# My preferred scale: mu = 5 sigma = 2
score_acp1 <- 5 + 2 * (dim1)
hist(score_acp1)
boxplot(score_acp1)

# 2. HOMALS Homogeinity analysis ----------------------------------------------


# Transforming discrete variables into continue variables


data_homal <- homals(dat_1, ndim = 2, level = "nominal")
acp_2 <- PCA(-data_homal$scoremat[, , 1], ncp = 2, graph = FALSE)

acp_2$var$contrib %>% data.frame() %>% arrange(desc(Dim.1))

fviz_pca_var(
  acp_2,
  col.var = "contrib",
  gradient.cols = c("white", "black"),
  ggtheme = theme_minimal()
)
fviz_pca_biplot(acp_2)

# First component selection
dim1 <- as.numeric(scale(acp_2$ind$coord[, 1]))
# My preferred scale: mu = 5 sigma = 2
score_homals <- 5 + 2 * (dim1)
hist(score_homals)
boxplot(score_homals)

# 3. Classic Item Response Theory ---------------------------------------------

#Reliability
cronbach.alpha(dat_1)

# welfare proportions
prop <- colMeans(dat_1)
sort(prop, decreasing = T)
# variance
var <- prop * (1 - prop)
sort(var, decreasing = T)

# standardized proportions
welfare <- rowMeans(dat_1)
mean(welfare)
sd(welfare)

welfare.est <- scale(welfare)
hist(welfare.est)

difficulty.est <- scale(prop)
hist(difficulty.est)

score_cirt <- 5 + 2 * welfare.est
mean(score_cirt)
sd(score_cirt)

hist(score_cirt)
boxplot(score_cirt)


# 4. Rasch Model ----------------------------------------------------------

res_rm_1 <- RM(dat_1)

pres <- person.parameter(res_rm_1)
theta.est <- pres$theta.table$`Person Parameter`
hist(theta.est)

beta.est <- pres$betapar
hist(beta.est)

welfare.est <- scale(theta.est)
hist(welfare.est)

score_RM <- 5 + 2 * welfare.est
hist(score_RM)

for (i in 1:ncol(dat_1)) {
  plotICC(res_rm_1, item.subset = i, legend = TRUE)
  abline(h = 0.5, lty = 3, col = 2)
  abline(v = 0, lty = 2, col = 3)
  print(i)
}

plotjointICC(res_rm_1, cex = .4)
plotPImap(res_rm_1, cex.gen = .55, sorted = TRUE)

# 5. Birnbaum's 3PL model------------------------------------------------------

res_3pl_1 <- tpm(dat_1)
res_3pl_1
plot(res_3pl_1)

## Item Characteristic Curves

for (i in 1:ncol(dat_1)) {
  plot(res_3pl_1, items = i, legend = TRUE)
  abline(h = 0.5, lty = 3, col = 2)
  abline(v = 0, lty = 2, col = 3)
  print(i)
}

## Item Information Curves
plot(res_3pl_1,
     items = c(1:5),
     type = "IIC",
     legend = T)

plot(res_3pl_1,
     items = c(6:10),
     type = "IIC",
     legend = T)

plot(res_3pl_1,
     items = c(11:15),
     type = "IIC",
     legend = T)

plot(res_3pl_1,
     items = c(16:20),
     type = "IIC",
     legend = T)

## Test Information Function
plot(res_3pl_1,
     type = "IIC",
     items = 0,
     legend = T)

plot(res_3pl_1, type = "IIC", legend = T)

# Generation of the scale

anaitem <- as.data.frame(coef(res_3pl_1))
rownames(anaitem)
anaitem[order(anaitem$Dscrmn), ]
## Information at 3.5SD
anaitem$info <- NULL
anaitem$names <- NULL
for (i in 1:ncol(dat_1)) {
  anaitem$names[i] <- rownames(anaitem)[i]
  anaitem$info[i] <-
    round(100 * unlist(information(res_3pl_1, c(-3.5, 3.5), items = i))$PropRange)
}

arrange(anaitem, Dscrmn, info)

# Standardizing the scores and creating the index
pres <- factor.scores(res_3pl_1, dat_1)

theta.est <- pres$score.dat$z1
hist(theta.est)

wrightMap(theta.est, sort(beta.est), label.items.row = 3)

# Equating

welfare.est <- scale(theta.est)
hist(welfare.est)
difficulty.est <- scale(beta.est)
hist(difficulty.est)

score_TRI <- 5 + 2 * welfare.est
mean(score_TRI)
sd(score_TRI)
hist(score_TRI, breaks = 20)
summary(score_TRI)

# Creation of welfare quantiles -------------------------------------------

weigthed_design <- dat_1 %>%
  mutate(
    feh = data$hh_weight,
    score_acp = score_acp1,
    score_homals = score_homals,
    score_RM = score_RM,
    score_TRI = score_TRI
  ) %>%
  svydesign(ids = ~ 1,
            weights = ~ feh,
            data = .)

Quantile_acp1 <- svyquantile(
  design = weigthed_design,
  x = ~ score_acp,
  quantiles =   seq(0, 1, by = 0.2)
)$score

Quantile_homals <- svyquantile(
  design = weigthed_design,
  x = ~ score_homals,
  quantiles =   seq(0, 1, by = 0.2)
)$score

Quantile_cirt <- svyquantile(
  design = weigthed_design,
  x = ~ score_cirt,
  quantiles =   seq(0, 1, by = 0.2)
)$score

Quantile_RM <- svyquantile(
  design = weigthed_design,
  x = ~ score_RM,
  quantiles =   seq(0, 1, by = 0.2)
)$score

Quantile_TRI <- svyquantile(
  design = weigthed_design,
  x = ~ score_TRI,
  quantiles =   seq(0, 1, by = 0.2)
)$score

Quantil_acp1 <- cut(
  score_acp1,
  breaks = Quantile_acp1[, 1],
  labels = paste0("Q", 1:5),
  include.lowest = TRUE
)

Quantil_homals <- cut(
  score_homals,
  breaks = Quantile_homals[, 1],
  labels = paste0("Q", 1:5),
  include.lowest = TRUE
)

Quantil_cirt <- cut(
  score_homals,
  breaks = Quantile_cirt[, 1],
  labels = paste0("Q", 1:5),
  include.lowest = TRUE
)

Quantil_RM <- cut(
  score_RM,
  breaks = Quantile_RM[, 1],
  labels = paste0("Q", 1:5),
  include.lowest = TRUE
)

Quantil_TRI <- cut(
  score_TRI,
  breaks = Quantile_TRI[, 1],
  labels = paste0("Q", 1:5),
  include.lowest = TRUE
)

score <- data.frame(
  method = gl(
    k = length(Quantil_acp1),
    n = 5,
    labels = c("ACP", "Homals", "CIRT", "RM", "TRI")
  ),
  score = c(score_acp1, score_homals, score_cirt, score_RM, score_TRI),
  Quantile = c(
    Quantil_acp1,
    Quantil_homals,
    Quantil_cirt,
    Quantil_RM,
    Quantil_TRI
  )
)

score <- score[complete.cases(score), ]

ggplot(data = score, aes(x = Quantile, y = score, fill = method)) +
  geom_boxplot() + theme_minimal()

ggplot(data = score, aes(x = score, color = method)) +
  geom_density() + theme_minimal()


# Final stage: Excluding items --------------------------------------------


arrange(anaitem, (Dscrmn))
arrange(anaitem, (info))

anaitem1 <- anaitem %>%
  filter(info >= 80 & Dscrmn > 1) %>%
  arrange(Dffclt)

wrightMap(theta.est, sort(anaitem1$Dffclt),
          label.items.row = 3)

anaitem2 <- anaitem %>%
  filter(info >= 80 | Dscrmn > 1) %>%
  arrange(Dffclt)
anaitem2

wrightMap(theta.est, sort(anaitem2$Dffclt),
          label.items.row = 3)

dat_1 %>% select(anaitem2$names) %>%
  mutate(
    score_acp = Quantil_acp1,
    score_homals = Quantil_homals,
    score_cirt = Quantil_cirt,
    score_RM = Quantil_RM,
    score_TRI = Quantil_TRI
  ) %>%
  saveRDS(object = ., file = "Data/RHS2021_HH_trimmed.rds")
