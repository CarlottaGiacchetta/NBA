options(scipen=999)

# LIBRERIE ----------------------------------------------------------------

library(plyr)
library(dplyr)
library(psych)
library(VIM)
library(funModeling)
library(factoextra)
library(factoextra)
library(GGally)
library(rgl)
library(factorMerger)
library(mctest)
library(gvlma)
library(MASS)
library(car)
library(lmtest)
library(sandwich)
library(mgcv)
library(ggeffects)
library(gratia)
library(stringr)
library(ppcor)
library(corrplot)
library(plyr)
library(ggplot2)
library(leaps)
library(robustbase)
library(forestmodel)


# IMPORTAZIONE DATI E PRIME ANALISI ---------------------------------------

load("players.Rdata")
str(players)
describe(players) # Ruoli doppi, meglio lasciare il primario
players[,c("FG.", "X3P.", "X2P.", "FT.", "eFG.")] = 100 * 
  players[,c("FG.", "X3P.", "X2P.", "FT.", "eFG.")]
table(players$Pos)

for (row in 1:nrow(players)) {
  pos = as.character(players[row, "Pos"])
  
  if(nchar(pos) > 2) {
    mat = matrix(unlist(str_split(pos, "-")),ncol=2,byrow=T)
    players[row, "Pos"] = mat[1,1]
    
  }
}

rm(mat, pos, row)

table(players$Tm)
table(players$Year)

players$Pos <- droplevels(players$Pos) 
table(players$Pos) #top!

summary(players$Sal)

par(mfrow=c(3,2))
plot(players$Pos, players$Sal)
plot(players$Age, players$Sal)
plot(players$G, players$Sal)
plot(players$PTS, players$Sal)
plot(players$eFG., players$Sal)
plot(players$eFG., players$MP)
par(mfrow=c(1,1))

# MISSING -----------------------------------------------------------------

players = players[is.na(players$Sal) == FALSE,]

missing = as.data.frame(sapply(players, function(x)(sum(is.na(x)))))
names(missing) = "MISSING"
missing$PERC = missing$MISSING / nrow(players)
missing

missing_del = missing[(missing$PERC >= 0.2),]
names(missing_del) = c("MISSING DELLE VARIABILI DA ELIMINARE", "PERC")
missing_del

missing_keep = missing[(missing$PERC < 0.2),]
names(missing_keep) = c("MISSING DELLE VARIABILI DA TENERE", "PERC")
missing_keep
rm(missing, missing_del, missing_keep)

# Zero variabili con percentuale di missing sopra il 20% osserviamo il grafico

missingness = aggr(players,col=c('navyblue','yellow'),numbers=TRUE,sortVars=TRUE,
                   labels=names(df),cex.axis=.7,gap=2)

missingness = aggr(players[, c("X3P.", "FT.", "X2P.", "FG.", "eFG.")],
                   col=c('navyblue','yellow'),numbers=TRUE,sortVars=TRUE,
                   labels=names(df),cex.axis=.7,gap=2)

# In realt? gli NA non rappresentano dei missing ma dei giocatori che non hanno mai
# realizzato un tiro e quindi possimamo sostituirli con 0

for (row in 1:nrow(players)) {
  
  for (col in 1:ncol(players)){
    
    val = players[row,col]
    
    if(is.na(val) == TRUE) {
     
      players[row,col] = 0 
    }
  }
  
}

# Ricontrolliamo i missing

missingness = aggr(players,col=c('navyblue','yellow'),numbers=TRUE,sortVars=TRUE,
                   labels=names(df),cex.axis=.7,gap=2) # top!

df1 = players
attach(df1)
df1$PER = (PTS-(FGA-FG)-(FTA-FT)+1.5*ORB+DRB+AST+STL+BLK-TOV-PF)*(GS/82)
summary(df1$PER)
df1$TRBW = ORB * 1.5 + DRB
summary(df1$TRBW)
df1$PAS = (AST + 1) / (TOV + 1)
summary(df1$PAS)
detach(df1)

par(mfrow=c(3,1))
plot(df1$PER, df1$Sal)
plot(df1$TRBW, df1$Sal)
plot(df1$PAS, df1$Sal)
par(mfrow=c(1,1))

df1[,c("ORB", "TRB", "DRB", "AST", "TOV")] = NULL

# OPTIMAL GROUPING --------------------------------------------------------

table(df1$Tm)
plot(df1$Tm, df1$Sal) # Sembrano esserci differenze nello stipendio in base alla squadra

reduce_levels <- mergeFactors(response = df1$Sal, 
                              factor = df1$Tm)
plot(reduce_levels , panel = "GIC",title = "", panelGrid = FALSE)
og = cutTree(reduce_levels)
df1$OG = og

table(df1$Tm)
table(df1$OG)
levels(df1$OG)

par(mfrow = c(1,2))
plot(df1$Tm, df1$Sal)
plot(df1$OG, df1$Sal)
par(mfrow = c(1,1))

# MODELLO INIZIALE --------------------------------------------------------

lm_p = lm(Sal ~ . - Player - Tm, data = players) # Scegliamo di utilizzare i gruppi ottimali
lm_d = lm(Sal ~ . - Player - Tm, data = df1)

summary(lm_p)
drop1(lm_p, test = "F")

summary(lm_d)
drop1(lm_d, test = "F")
forest_model(lm_d)

par(mfrow = c(2,2))
plot(lm_p)
par(mfrow = c(1,1))

par(mfrow = c(2,2))
plot(lm_d)
par(mfrow = c(1,1))

gvlma(lm_p) # not bad!
gvlma(lm_d)

plot(lm_p$fitted.values, players$Sal)
plot(lm_d$fitted.values, df1$Sal)

# TOL E VIF ---------------------------------------------------------------

df1_num = df1 %>% select_if(is.numeric)

mod = lm(Sal ~ ., df1_num)
imcdiag(mod) # certe variabili sono ottenute come funzione di altre, ovvia collin

mod_2 = lm(Sal ~ Age + G + GS + MP + FGA + FG. + X3PA + X3P. + X2PA + X2P. + eFG. +
             FTA + FT. + STL + BLK + PF + PTS + PER + TRBW + PAS, data = df1_num)
imcdiag(mod_2) # certe variabili sono ottenute come funzione di altre, ovvia collin

mod_3 = lm(Sal ~ Age + G + GS + MP + FG. + X3PA + X3P. + X2PA + X2P. + eFG. +
             FTA + FT. + STL + BLK + PF + PTS + PER + TRBW + PAS,
           data = df1_num)
imcdiag(mod_3)

mod_4 = lm(Sal ~ Age + G + GS + MP + FG. + X3P. + X2P. + FT. + 
              STL + BLK  + PF + PTS + TRBW + PAS + PER, data = df1_num)
imcdiag(mod_4)

mod_5 = lm(Sal ~ Age + G + GS + FG. + X3P. + X2P. + FT.  +
              STL + BLK + PF + PTS + PAS + PER +TRBW, data = df1_num)
imcdiag(mod_5)

mod_6 = lm(Sal ~ Age + G + GS + FG. + X3P. + X2P. + FT.  +
             STL + BLK + PF + PAS + PER + TRBW, data = df1_num)
imcdiag(mod_6)

df2 = df1

df2[,c("Tm", "MP", "FG", "FGA", "X3P", "X3PA", "X2P", "X2PA",
               "eFG.", "FT", "FTA", "PTS")] = NULL
df2_num = df2 %>% dplyr::select_if(is.numeric)

lm_2 = lm(Sal ~ ., data = df2_num)
par(mfrow=c(2,2))
plot(lm_2)
par(mfrow=c(1,1))
 
summary(mod)
summary(lm_2)
drop1(lm_2, test = "F")

# CHI QUADRO --------------------------------------------------------------

df2_cat = df2 %>% select_if(is.factor)

combos <- combn(ncol(df2_cat),2)
adply(combos, 2, function(x) {
  test <- chisq.test(df2_cat[, x[1]], df2_cat[, x[2]])
  tab  <- table(df2_cat[, x[1]], df2_cat[, x[2]])
  out <- data.frame("Row" = colnames(df2_cat)[x[1]]
                    , "Column" = colnames(df2_cat[x[2]])
                    , "Chi.Square" = round(test$statistic,3)
                    , "df"= test$parameter
                    , "p.value" = round(test$p.value, 3)
                    , "n" = sum(table(df2_cat[,x[1]], df2_cat[,x[2]]))
                    , "u1" =length(unique(df2_cat[,x[1]]))-1
                    , "u2" =length(unique(df2_cat[,x[2]]))-1
                    , "nMinu1u2" =sum(table(df2_cat[,x[1]], 
                                            df2_cat[,x[2]]))* min(length(unique(df2_cat[,x[1]]))-1 ,
                                                                  length(unique(df2_cat[,x[2]]))-1) 
                    , "Chi.Square norm"  =test$statistic/(sum(table(df2_cat[,x[1]],
                                                                    df2_cat[,x[2]]))* min(length(unique(df2_cat[,x[1]]))-1 ,
                                                                                          length(unique(df2_cat[,x[2]]))-1)) 
  )
  
  return(out)
  
}) 

# come ci aspettavamo nessun problema

# MODELLO POST COLLIN -----------------------------------------------------

lm_3 = lm(Sal ~ . -Player, data = df2)
drop1(lm_3, test = "F")
summary(lm_3)

plot(players$Year, players$Sal)
# Variabile anno sus, proviamo a toglierla

lm_4 = lm(Sal ~ . -Player - Year, data = df2)
drop1(lm_4, test = "F")
summary(lm_4)
# Per ora decidiamo di tenerla in quanto il modello non presenti evidenti miglioramenti senza
plot(lm_4$fitted.values, df2$Sal)

# BOX-COX -----------------------------------------------------------------

par(mfrow = c(2,2))
plot(lm_3)
par(mfrow = c(1,1))

boxcoxreg = boxcox(lm_3)
lambda = boxcoxreg$x[which.max(boxcoxreg$y)]
lambda

#proviamo con il logaritmo
df2_log = df2
df2_log$Sal = log(df2$Sal)
summary(df2_log$Sal)
summary(df2$Sal)

lm_log = lm(Sal ~ . -Player, data = df2_log)
summary(lm_3)
summary(lm_log)
drop1(lm_3, test = "F")
drop1(lm_log, test = "F")

par(mfrow = c(4,2))
plot(lm_log)
plot(lm_3)
par(mfrow = c(1,1))

par(mfrow = c(2,2))
plot(lm_log)
par(mfrow = c(1,1))

gvlma(lm_3)
gvlma(lm_log)
# teniamo log
plot(lm_log$fitted.values, log(df2$Sal))

# BINNING ---------------------------------------------------------------------

df3 = df2_log

for (row in 1:nrow(df3)) {
  age = df3[row, "Age"]
  if(age <= 22) {
    df3[row, "Age_cl"] = 1
  }
  if(age > 22 & age <= 25) {
    df3[row, "Age_cl"] = 2
  }
  if(age > 25 & age <= 29) {
    df3[row, "Age_cl"] = 3
  }
  if(age > 28 & age <= 42) {
    df3[row, "Age_cl"] = 4
  }
  
}

df3$Age_cl = as.factor(df3$Age_cl)

# Riproviamo il chi quadro

df3_cat = df3 %>% select_if(is.factor)

combos <- combn(ncol(df3_cat),2)
adply(combos, 2, function(x) {
  test <- chisq.test(df3_cat[, x[1]], df3_cat[, x[2]])
  tab  <- table(df3_cat[, x[1]], df3_cat[, x[2]])
  out <- data.frame("Row" = colnames(df3_cat)[x[1]]
                    , "Column" = colnames(df3_cat[x[2]])
                    , "Chi.Square" = round(test$statistic,3)
                    , "df"= test$parameter
                    , "p.value" = round(test$p.value, 3)
                    , "n" = sum(table(df3_cat[,x[1]], df3_cat[,x[2]]))
                    , "u1" =length(unique(df3_cat[,x[1]]))-1
                    , "u2" =length(unique(df3_cat[,x[2]]))-1
                    , "nMinu1u2" =sum(table(df3_cat[,x[1]], 
                                            df3_cat[,x[2]]))* min(length(unique(df3_cat[,x[1]]))-1 ,
                                                                  length(unique(df3_cat[,x[2]]))-1) 
                    , "Chi.Square norm"  =test$statistic/(sum(table(df3_cat[,x[1]],
                                                                    df3_cat[,x[2]]))* min(length(unique(df3_cat[,x[1]]))-1 ,
                                                                                          length(unique(df3_cat[,x[2]]))-1)) 
  )
  
  return(out)
  
}) 

par(mfrow = c(1,2))
plot(df3$Age_cl, df3$Sal)
plot(df3$Age, df3$Sal)
par(mfrow = c(1,1))

for (row in 1:nrow(df3)) {
  age_cl = df3[row, "Age_cl"]
  if(age_cl == 1 | age_cl == 2) {
    df3[row, "Age_cl_og"] = 1
  }
  if(age_cl == 3 | age_cl == 4) {
    df3[row, "Age_cl_og"] = 2
  }
}

df3$Age_cl_og = as.factor(df3$Age_cl_og)

par(mfrow = c(1,3))
plot(df3$Age, df3$Sal)
plot(df3$Age_cl, df3$Sal)
plot(df3$Age_cl_og, df3$Sal)
par(mfrow = c(1,1))

df3$Age = NULL
df3$Age_cl = NULL

# Proviamo il modello

lm_bin = lm_log = lm(Sal ~ . -Player, data = df3)

summary(lm_bin)
drop1(lm_bin, test = "F")

par(mfrow = c(4,2))
plot(lm_log)
plot(lm_bin)
par(mfrow = c(1,1))

forest_model(lm_bin)

# GAM ---------------------------------------------------------------------

df3$G = df3$G + 1
df3$GS = df3$GS + 1
df3$PER = df3$PER + 1
df3$STL = df3$STL+ 1 

crPlots(lm_bin)

gam_1 = gam(formula = Sal ~ s(G) + s(GS) + s(FG.) + s(X3P.) + s(X2P.) +
              s(FT.) + s(STL) + s(BLK) + s(PF) + s(PER) + s(TRBW) + s(PAS) +
              Pos + Year + OG + Age_cl_og, data = df3, method = "REML")
summary(gam_1)
plot(gam_1)

ggplot(data = df3, aes(x = G, y = Sal)) + geom_point(color = "gray") +
  geom_smooth(method = "loess", span = 0.2, col = "red") +
  geom_smooth(method = "loess", span = 0.5, col = "purple3") +
  theme_bw()
ggplot(data = df3, aes(x = log(G), y = Sal)) + geom_point(color = "gray") +
  geom_smooth(method = "loess", span = 0.2, col = "red") +
  geom_smooth(method = "loess", span = 0.5, col = "purple3") +
  theme_bw()

ggplot(data = df3, aes(x = GS, y = Sal)) + geom_point(color = "gray") +
  geom_smooth(method = "loess", span = 0.2, col = "red") +
  geom_smooth(method = "loess", span = 0.5, col = "purple3") +
  theme_bw()
ggplot(data = df3, aes(x = log(GS), y = Sal)) + geom_point(color = "gray") +
  geom_smooth(method = "loess", span = 0.2, col = "red") +
  geom_smooth(method = "loess", span = 0.5, col = "purple3") +
  theme_bw()

ggplot(data = df3, aes(x = FT., y = Sal)) + geom_point(color = "gray") +
  geom_smooth(method = "loess", span = 0.2, col = "red") +
  geom_smooth(method = "loess", span = 0.5, col = "purple3") +
  theme_bw()
ggplot(data = df3, aes(x = I((FT.)^2), y = Sal)) + geom_point(color = "gray") +
  geom_smooth(method = "loess", span = 0.2, col = "red") +
  geom_smooth(method = "loess", span = 0.5, col = "purple3") +
  theme_bw()

ggplot(data = df3, aes(x = PER, y = Sal)) + geom_point(color = "gray") +
  geom_smooth(method = "loess", span = 0.2, col = "red") +
  geom_smooth(method = "loess", span = 0.5, col = "purple3") +
  theme_bw()
ggplot(data = df3, aes(x = log(PER), y = Sal)) + geom_point(color = "gray") +
  geom_smooth(method = "loess", span = 0.2, col = "red") +
  geom_smooth(method = "loess", span = 0.5, col = "purple3") +
  theme_bw()

plot(df3$STL, df3$Sal)
ggplot(data = df3, aes(x = log(STL), y = Sal)) + geom_point(color = "gray") +
  geom_smooth(method = "loess", span = 0.2, col = "red") +
    geom_smooth(method = "loess", span = 0.5, col = "purple3") +
  theme_bw()

lm_gam = lm(formula = Sal ~ log(G) + log(GS) + FG. + X3P. + X2P. +
              I((FT.)^2) + FT. + log(STL) + BLK + PF + log(PER) + TRBW + PAS +
              Pos + Year + OG + Age_cl_og, data = df3)
summary(lm_gam)
forest_model(lm_gam)
drop1(lm_gam, test = "F")
par(mfrow=c(2,2))
plot(lm_gam)
par(mfrow=c(1,1))

plot(ggpredict(gam_1), facets = TRUE)
draw(gam_1)

anova(lm_bin, lm_gam)
gam.check(gam_1, k.rep = 1000) 
concurvity(gam_1)

# AIC E CP ----------------------------------------------------------------

step(lm_gam, direction = "both")

step(lm_gam, direction = "both", k = log(nrow(df3)))

#cp_sel = regsubsets(Sal ~ . - Player, data = df3)
#cp = summary(cp_sel)
#cp$which
#cp$cp
lm_prova = lm(formula = Sal ~ log(G) + log(GS) + X3P. + I((FT.)^2) + log(STL) + 
                log(PER) + TRBW + PAS + Pos + Year + OG + Age_cl_og, data = df3)
drop1(lm_prova, test = "F")

lm_AIC = lm(formula = Sal ~ log(G) + log(GS) +
              I(FT.^2) + FT.+ log(STL) + log(PER) +  
              TRBW + Year + Age_cl_og + OG, data = df3)
summary(lm_AIC)
drop1(lm_AIC, test = "F")

par(mfrow = c(2,2))
plot(lm_AIC)
par(mfrow = c(1,1))

# OUTLIERS ----------------------------------------------------------------

bptest(lm_AIC)

influencePlot(lm_AIC, main = "Influence Plot")

muvec = colMeans(df3[sapply(df3, is.numeric)])
muvec

df3[1827,-17]
df3[1847, -17]
df3[1979, -17]

cooksd = data.frame(cooks.distance(lm_AIC))

cutoff = 4 / ((nrow(df3) - length(lm_AIC$coefficients) - 2))
influential = df3[cooksd >= cutoff,]

filtered = df3[cooksd < cutoff, ]
lm_OUT = lm(formula = Sal ~ log(G) + log(GS) +
              I(FT.^2) + FT.+ log(STL) + log(PER) +  
              TRBW + Year + Age_cl_og + OG, data = filtered)
summary(lm_OUT)
drop1(lm_OUT, test = "F")

par(mfrow = c(2,2))
plot(lm_OUT)
par(mfrow = c(1,1))

# DFITTS ------------------------------------------------------------------

dffits = dffits(lm_AIC)

thresh3 = 2*sqrt(length(lm_AIC$coefficients)/length(dffits))
dffits[dffits > thresh3]

df3$dffits = dffits
clean = df3[df3$dffits <= thresh3, ]

lm_OUT_2 = lm(formula = Sal ~ log(G) +
                I(FT.^2) + FT.+ log(STL) + log(PER) +  
                TRBW + Year + Age_cl_og + OG, data = clean)

summary(lm_OUT_2)
drop1(lm_OUT_2, test = "F")

par(mfrow = c(2,2))
plot(lm_OUT_2)
par(mfrow = c(1,1))

# HETEROSKEDASTICITY -------------------------------------------------------

plot(lm_OUT$fitted.values, lm_OUT$residuals)

bptest(lm_OUT)
ncvTest(lm_OUT)

coeftest(lm_OUT, vcov = vcovHC(lm_OUT))

std_correct = lm_OUT %>% 
  vcovHC() %>% 
  diag() %>% 
  sqrt()
std_correct

# proviamo a togliere FT. e eseguire l'analisi

lm_OUT_FT = lm(formula = Sal ~ log(G) + log(GS) +
               log(STL) + log(PER) +  
              TRBW + Year + Age_cl_og + OG, data = filtered)

anova(lm_OUT, lm_OUT_FT)
bptest(lm_OUT_FT)

coeftest(lm_OUT_FT, vcov = vcovHC(lm_OUT_FT))

gvlma(lm_OUT_FT)

# proviamo a togliere year e eseguire l'analisi

lm_OUT_FT_YR = lm(formula = Sal ~ log(G) + log(GS) +
                  log(STL) + log(PER) +  
                 TRBW + Age_cl_og + OG, data = filtered)

anova(lm_OUT_FT_YR, lm_OUT_FT)
bptest(lm_OUT_FT_YR)

coeftest(lm_OUT_FT_YR, vcov = vcovHC(lm_OUT_FT_YR))

gvlma(lm_OUT_FT)

# BOOTSTRAP ---------------------------------------------------------------

lm_BOT=Boot(lm_OUT_FT, R=1999)
Confint(lm_BOT, level=c(.95), type="perc")
hist(lm_BOT, legend="separate")

# CONFRONTO MODELLO INIZIALE E FINALE -------------------------------------

par(mfrow = c(2,2))
plot(lm_d)
plot(lm_OUT_FT)
par(mfrow = c(1,1))

forest_model(lm_d)
forest_model(lm_OUT_FT_YR)

summary(lm_d)
summary(lm_OUT_FT)

gvlma(lm_d)
gvlma(lm_OUT_FT)

# ANALISI COEFFICIENTI -------------------------------------------------------------------

(exp(lm_OUT_FT$coefficients[7:14])-1)*100

# LOGIT -------------------------------------------------------------------

quantile(df2$Sal, probs = seq(0, 1, 0.1), na.rm = FALSE,
         names = TRUE, type = 7)

summary(df2$Sal)

df4 = df2

for (row in 1:nrow(df4)) {
  Sal = df4[row, "Sal"]
  if(Sal >= 30000000) {
    df4[row, "Sal"] = 1
  }
  if(Sal < 30000000) {
    df4[row, "Sal"] = 0
  }
}

for (row in 1:nrow(df4)) {
  age = df4[row, "Age"]
  if(age <= 25) {
    df4[row, "Age_cl"] = 1
  }
  if(age > 25) {
    df4[row, "Age_cl"] = 2
  }
}
df4$PER= df4$PER +1
df4$STL= df4$STL +1
df4$GS= df4$GS +1

summary(df4)
df4$Sal = as.factor(df4$Sal)
df4$Age_cl = as.factor(df4$Age_cl)
summary(df4$Age_cl)

lm_logit = glm(formula = Sal ~ log(G) + log(GS) +
                 log(STL) + log(PER) +  
                 TRBW + Year + Age_cl + OG, data = df4, family = "binomial")

summary(lm_logit)
drop1(lm_logit, test="LRT")


df4$predicted = predict(lm_logit, df4, type="response")
df4$predicted_y <- ifelse(df4$predicted > 0.5,1,0)

table(observed=df4$Sal, predicted=df4$predicted_y)
prob = table(observed=df4$Sal, predicted=df4$predicted_y)/nrow(df4)

accuracy = sum(diag(prob))
accuracy 

exp(lm_logit$coefficients)
