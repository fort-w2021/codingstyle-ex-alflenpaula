# ALTES SCRIPT:

lesen <- within(lesen, {
  WOL <- 5 - WieoftLesen
  WOTV <- 5 - WieoftFern
  WOM <- 5 - WieoftMusik
  WOW <- 5 - WieoftWalk
  WComp <- 5 - WieoftComp
  WOG <- 5 - WieoftGame
})


model1 <- lm(Fehlerzahl ~ sex + Lesezeitmin, data = lesen)
summary(model1)
#### (3) Bereinigung ###########################################################

### (a) Bereinigung der Variablen nach Geschlecht
# Abziehen des jeweiligen Gruppenmittelwertes, wobei die Regression mit binärer
# Variable Geschlecht der Mittwertschätzung entspricht

# Fehlerzber = Fehleranzahl nach Geschlecht bereinigt
model.b1 <- lm(Fehlerzahl ~ sex, data = lesen)
lesen$fehlerzahlber <- model.b1$res

#### entspricht abziehen des Mittelwertes
mean(lesen$fehlerzahlber[lesen$sex == 0])

# Unterschied betrachten
par(mfrow = c(1, 2))
boxplot(Fehlerzahl ~ sex, varwidth = TRUE, main = "unbereinigt", data = lesen)
boxplot(fehlerzahlber ~ sex, varwidth = TRUE, main = "bereinigt", data = lesen)

# Lesezeitminber = Lesezeitmin nach Geschlecht bereinigt
model.b2 <- lm(Lesezeitmin ~ sex, data = lesen)
lesen$Lesezeitminber <- model.b2$res

# Unterschied betrachten
par(mfrow = c(1, 2))

summary(model.b2)

### (b) Bereinigtes Modell
model.ber <- lm(fehlerzahlber ~ Lesezeitminber - 1, data = lesen)
summary(model.ber)

### coeffizient gleich !
summary(model1)

library(car)
#### Added variable plot oder partial leverage plot
avPlots(model1, ~Lesezeitmin)
with(lesen, plot(Lesezeitminber, fehlerzahlber))
abline(model.ber)
model1 <- # Modell 1: großes Modell
  modelf <- lm(Fehlerzahl ~ sex + Jahrgang + Lesezeitmin + WOL + WOG +
    WOTV, data = lesen)

library(car)
library(effects)
par(mfrow = c(1, 1))
plot(allEffects(modelf), ylim = c(10, 20))
avPlots(model1, ~Lesezeitmin)
summary(modelf)
