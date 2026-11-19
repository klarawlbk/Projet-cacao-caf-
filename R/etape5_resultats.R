# =============================================================================
#  PROJET D'ÉCONOMÉTRIE – « Du cacao au café »
#  ÉTAPE 5 – Résultats : modèle final, cointégration, long terme, court terme
#            (ECM), diagnostics et stabilité  (parties 5 et 6 du rapport)
#
#  Modèle retenu (étape 3) : larabica = f(lcacao, lsucre, lbrent)
#  Prérequis : base_cacao_cafe_2000_2026.csv dans le dossier de travail
# =============================================================================

# ---- 0. Packages ------------------------------------------------------------
# install.packages(c("ARDL", "performance", "lmtest", "car", "strucchange"))
library(ARDL)
library(performance)
library(lmtest)
library(car)
library(strucchange)   # CUSUM

# ---- 1. Données -------------------------------------------------------------
data <- read.csv("base_cacao_cafe_2000_2026.csv")
data$date <- as.Date(data$date)
data_ts <- ts(data[, -1], start = c(2000, 1), frequency = 12)

# >>> Choix des contrôles : retirer "lbrent" ici pour le modèle sans Brent <<<
formule <- larabica ~ lcacao + lsucre + lbrent

# ---- 2. Sélection du modèle (rappel étape 3) --------------------------------
model1 <- auto_ardl(formule, data = data_ts, max_order = 4, selection = "AIC")
model1$top_orders           # classement des spécifications par AIC
model_final <- model1$best_model
model_final$order           # ARDL(2,1,1,0)
summary(model_final)

# ---- 3. Test de cointégration : Bounds test de Pesaran et al. (2001) -------
# H0 : pas de relation de long terme.
# Case 3 : constante non restreinte, pas de tendance (les prix n'ont pas de
# tendance déterministe propre, cf. étape 2).
# Règle : F > borne I(1) -> cointégration ; F < borne I(0) -> pas de
# cointégration ; entre les deux -> zone d'indécision.
bf5  <- bounds_f_test(model_final, case = 3, alpha = 0.05)
bf1  <- bounds_f_test(model_final, case = 3, alpha = 0.01)
bf10 <- bounds_f_test(model_final, case = 3, alpha = 0.10)
bt5  <- bounds_t_test(model_final, case = 3, alpha = 0.05)
bf5      # F = 5,88 ; p = 0,006

tab_bounds <- data.frame(
  Test      = c("F (Wald)", "F (Wald)", "F (Wald)", "t"),
  Seuil     = c("10 %", "5 %", "1 %", "5 %"),
  Statistique = round(c(bf10$tab$statistic, bf5$tab$statistic,
                        bf1$tab$statistic, bt5$tab$statistic), 3),
  Borne_I0  = round(c(bf10$tab[, 2], bf5$tab[, 2], bf1$tab[, 2], bt5$tab[, 2]), 3),
  Borne_I1  = round(c(bf10$tab[, 3], bf5$tab[, 3], bf1$tab[, 3], bt5$tab[, 3]), 3),
  p_value   = round(c(bf5$p.value, bf5$p.value, bf5$p.value, bt5$p.value), 4))
print(tab_bounds)

# ---- 4. Relation de long terme ----------------------------------------------
# Coefficients de long terme = élasticités (variables en log)
lt <- multipliers(model_final, type = "lr")
print(lt)
# Lecture : +10 % durable du cacao -> +8,7 % du café à long terme

# ---- 5. Court terme : modèle à correction d'erreur (ECM) --------------------
ecm <- recm(model_final, case = 3)
summary(ecm)
ect <- coef(summary(ecm))["ect", ]
vitesse   <- ect["Estimate"]
demi_vie  <- log(0.5) / log(1 + vitesse)       # en mois
cat("Vitesse d'ajustement :", round(vitesse, 4),
    "| demi-vie :", round(demi_vie, 1), "mois\n")
# Conditions : coefficient ect négatif, compris entre -1 et 0, significatif

# ---- 6. Diagnostics (fonctions du cours + tests classiques) -----------------
check_normality(model_final)           # H0 : résidus normaux
check_autocorrelation(model_final)     # H0 : pas d'autocorrélation (DW)
check_heteroscedasticity(model_final)  # H0 : homoscédasticité (Breusch-Pagan)

jb  <- tseries::jarque.bera.test(residuals(model_final))
sw  <- shapiro.test(residuals(model_final))
bg1 <- bgtest(model_final, order = 1)
bg12 <- bgtest(model_final, order = 12)
bp  <- bptest(model_final)
reset <- resettest(model_final, power = 2:3, type = "fitted")  # forme fonctionnelle
arch <- function(e, lags = 12) {                               # test ARCH (Engle)
  e2 <- e^2; n <- length(e2)
  X <- embed(e2, lags + 1)
  r2 <- summary(lm(X[, 1] ~ X[, -1]))$r.squared
  stat <- (n - lags) * r2
  c(stat = stat, p = 1 - pchisq(stat, lags))
}
arch12 <- arch(residuals(model_final), 12)

# VIF sur la relation de long terme (régression statique en niveaux)
vif_lt <- vif(lm(formule, data = data))

tab_diag <- data.frame(
  Test = c("Normalité – Shapiro-Wilk", "Normalité – Jarque-Bera",
           "Autocorrélation – Breusch-Godfrey (1)",
           "Autocorrélation – Breusch-Godfrey (12)",
           "Hétéroscédasticité – Breusch-Pagan",
           "Effets ARCH (12)", "Forme fonctionnelle – RESET"),
  H0 = c("résidus normaux", "résidus normaux", "pas d'autocorrélation",
         "pas d'autocorrélation", "homoscédasticité", "pas d'effet ARCH",
         "spécification correcte"),
  Statistique = round(c(sw$statistic, jb$statistic, bg1$statistic,
                        bg12$statistic, bp$statistic, arch12["stat"],
                        reset$statistic), 3),
  p_value = round(c(sw$p.value, jb$p.value, bg1$p.value, bg12$p.value,
                    bp$p.value, arch12["p"], reset$p.value), 3))
tab_diag$Conclusion <- ifelse(tab_diag$p_value > 0.05, "H0 non rejetée (OK)",
                              "H0 rejetée")
print(tab_diag)
print(round(vif_lt, 2))   # tous < 10

# ---- 7. Stabilité : CUSUM et CUSUMSQ (Brown, Durbin et Evans, 1975) ---------
# On ré-estime le modèle ARDL avec lm() sur sa matrice de régresseurs pour
# pouvoir calculer les résidus récursifs.
y <- model_final$model[, 1]
X <- model.matrix(model_final)
dates_mod <- as.numeric(time(residuals(model_final)))

cusum <- efp(y ~ X - 1, type = "Rec-CUSUM")
sctest(cusum)          # H0 : stabilité des coefficients

# CUSUMSQ : somme cumulée des carrés des résidus récursifs
w  <- recresid(y ~ X - 1)
k  <- ncol(X); n <- length(y)
s  <- cumsum(w^2) / sum(w^2)
t_ <- seq_along(w)
esp <- t_ / length(w)                         # valeur attendue sous H0
m_  <- (n - k) / 2 - 1
c0  <- 1.358 / sqrt(m_) - 0.6675 / m_         # approx. Edgerton & Wells (1994), 5 %
cusumsq_sort <- any(abs(s - esp) > c0)
cat("CUSUMSQ hors des bandes à 5 % :", cusumsq_sort, "\n")

png("figure7_cusum_cusumsq.png", width = 2400, height = 1000, res = 220)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(cusum, main = "CUSUM (bandes à 5 %)", xlab = "", ylab = "", alpha = 0.05)
dates_w <- tail(dates_mod, length(w))
plot(dates_w, s, type = "l", lwd = 1.8, col = "#1f4e79", ylim = c(-0.2, 1.2),
     main = "CUSUM des carrés (bandes à 5 %)", xlab = "", ylab = "")
lines(dates_w, esp + c0, lty = 2, col = "red")
lines(dates_w, esp - c0, lty = 2, col = "red")
lines(dates_w, esp, col = "grey60")
dev.off()

# ---- 8. Graphique : prix observé vs équilibre de long terme -----------------
# Équilibre : larabica* = θ0 + θ1·lcacao + θ2·lsucre + θ3·lbrent
th <- setNames(lt$Estimate, lt$Term)
data$eq_lt <- th["(Intercept)"] + th["lcacao"] * data$lcacao +
  th["lsucre"] * data$lsucre +
  (if ("lbrent" %in% names(th)) th["lbrent"] * data$lbrent else 0)
data$ecart <- data$larabica - data$eq_lt      # déséquilibre (terme d'erreur)

png("figure8_equilibre_long_terme.png", width = 2400, height = 1500, res = 220)
par(mfrow = c(2, 1), mar = c(2.5, 4.5, 2.5, 1))
plot(data$date, data$larabica, type = "l", lwd = 1.8, col = "#1f4e79",
     ylim = range(c(data$larabica, data$eq_lt)), xlab = "",
     ylab = "log prix Arabica",
     main = "Prix du café Arabica observé et équilibre de long terme")
lines(data$date, data$eq_lt, lwd = 1.8, col = "#7b3f00", lty = 2)
legend("topleft", bty = "n", lwd = 1.8, lty = c(1, 2),
       col = c("#1f4e79", "#7b3f00"),
       legend = c("Observé", "Équilibre de long terme (cacao, sucre, Brent)"))
grid()
plot(data$date, data$ecart, type = "h", col = ifelse(data$ecart > 0, "#1f4e79", "#c0504d"),
     xlab = "", ylab = "Écart (log)",
     main = "Écart à l'équilibre (terme de correction d'erreur)")
abline(h = 0); grid()
dev.off()

# ---- 9. Graphique : ajusté vs observé et résidus ----------------------------
png("figure9_residus.png", width = 2400, height = 1000, res = 220)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(dates_mod, residuals(model_final), type = "l", col = "#1f4e79",
     main = "Résidus du modèle ARDL", xlab = "", ylab = "")
abline(h = 0, col = "grey40"); grid()
hist(residuals(model_final), breaks = 30, freq = FALSE, col = "#c9d9ea",
     border = "white", main = "Distribution des résidus", xlab = "")
curve(dnorm(x, mean(residuals(model_final)), sd(residuals(model_final))),
      add = TRUE, lwd = 2, col = "#7b3f00")
dev.off()

# ---- 10. Tableaux pour le rapport -------------------------------------------
cf <- coef(summary(model_final))
tab_ardl <- data.frame(Variable = rownames(cf), round(cf, 4), row.names = NULL)
tab_lt   <- data.frame(Variable = lt$Term, round(lt[, -1], 4))
tab_ecm  <- data.frame(Variable = rownames(coef(summary(ecm))),
                       round(coef(summary(ecm)), 4), row.names = NULL)

write.csv(tab_ardl,   "tableau10_ardl_coefficients.csv", row.names = FALSE)
write.csv(tab_bounds, "tableau11_bounds_test.csv",       row.names = FALSE)
write.csv(tab_lt,     "tableau12_long_terme.csv",        row.names = FALSE)
write.csv(tab_ecm,    "tableau13_ecm_court_terme.csv",   row.names = FALSE)
write.csv(tab_diag,   "tableau14_diagnostics.csv",       row.names = FALSE)
