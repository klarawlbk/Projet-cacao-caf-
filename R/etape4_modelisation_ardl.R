# =============================================================================
#  PROJET D'ÉCONOMÉTRIE – « Du cacao au café »
#  ÉTAPE 4 – Modélisation ARDL et Bounds test (partie 5 du rapport)
#
#   4.1  Spécification du modèle
#   4.2  Choix des variables de contrôle (analyse combinatoire)
#   4.3  Sélection des retards : AIC vs BIC
#   4.4  Robustesse au nombre maximal de retards
#   4.5  Estimation du modèle retenu
#   4.6  Bounds test de Pesaran, Shin et Smith (2001)
#
#  Prérequis : base_cacao_cafe_2000_2026.csv dans le dossier de travail
# =============================================================================

# ---- 0. Packages ------------------------------------------------------------
# install.packages(c("ARDL", "performance", "lmtest", "car"))
library(ARDL)
library(performance)
library(lmtest)
library(car)

# ---- 1. Données -------------------------------------------------------------
data <- read.csv("base_cacao_cafe_2000_2026.csv")
data$date <- as.Date(data$date)
data_ts <- ts(data[, -1], start = c(2000, 1), frequency = 12)

# =============================================================================
# 4.1 SPÉCIFICATION
# =============================================================================
# Variable dépendante : larabica (log du prix du café Arabica)
# Variable d'intérêt  : lcacao   (log du prix du cacao)
# Contrôles candidats : lsucre, lbrent, luree
#
# Modèle ARDL(p, q1, q2, q3) :
#   larabica_t = c + Σ(i=1..p) φi larabica_{t-i}
#                  + Σ(j=0..q1) β1j lcacao_{t-j}
#                  + Σ(j=0..q2) β2j lsucre_{t-j}
#                  + Σ(j=0..q3) β3j lbrent_{t-j} + ε_t
#
# Forme à correction d'erreur non contrainte (UECM), utilisée par le
# Bounds test :
#   Δlarabica_t = c + π0 larabica_{t-1} + π1 lcacao_{t-1} + π2 lsucre_{t-1}
#                 + π3 lbrent_{t-1} + Σ (termes en différences) + ε_t
#   H0 (pas de cointégration) : π0 = π1 = π2 = π3 = 0

# =============================================================================
# 4.2 CHOIX DES CONTRÔLES : ANALYSE COMBINATOIRE
# =============================================================================
# Le cacao est toujours présent ; on teste les 2^3 = 8 combinaisons de
# contrôles. Pour chaque combinaison, les retards sont choisis par AIC.

evaluer <- function(xs, sel = "AIC", max_order = 4) {
  f <- as.formula(paste("larabica ~", paste(xs, collapse = " + ")))
  m <- auto_ardl(f, data = data_ts, max_order = max_order,
                 selection = sel)$best_model
  bf <- bounds_f_test(m, case = 3)
  lt <- multipliers(m); lt <- lt[lt$Term == "lcacao", ]
  data.frame(
    Regresseurs = paste(xs, collapse = " + "),
    Critere   = sel,
    Ordre     = paste0("ARDL(", paste(m$order, collapse = ","), ")"),
    AIC       = round(AIC(m), 2),
    BIC       = round(BIC(m), 2),
    R2_aj     = round(summary(m)$adj.r.squared, 4),
    F_bounds  = round(bf$statistic, 2),
    p_F       = round(bf$p.value, 4),
    LT_cacao  = round(lt$Estimate, 3),
    p_normal  = round(as.numeric(check_normality(m)), 3),
    p_BG12    = round(bgtest(m, order = 12)$p.value, 3),
    p_hetero  = round(as.numeric(check_heteroscedasticity(m)), 3),
    VIF_max   = if (length(xs) > 1) round(max(vif(lm(f, data = data))), 2) else NA
  )
}

controles <- c("lsucre", "lbrent", "luree")
combinaisons <- c(list(character(0)),
                  unlist(lapply(1:3, function(k) combn(controles, k, simplify = FALSE)),
                         recursive = FALSE))

tab_comb <- do.call(rbind, lapply(combinaisons, function(cb) evaluer(c("lcacao", cb))))
tab_comb <- tab_comb[order(tab_comb$AIC), ]          # classement par AIC
tab_comb$Tous_tests_OK <- with(tab_comb,
  p_normal > 0.05 & p_BG12 > 0.05 & p_hetero > 0.05 & (is.na(VIF_max) | VIF_max < 10))
print(tab_comb, row.names = FALSE)

# Lecture :
#  - Les 8 modèles sont cointégrés (p_F < 0,05) et l'élasticité au cacao
#    reste comprise entre 0,71 et 1,16 : le résultat ne dépend pas des contrôles.
#  - Seuls les 4 modèles contenant le SUCRE passent tous les diagnostics
#    (sans sucre, la normalité est rejetée) : le sucre est indispensable.
#  - Entre ces 4 modèles, les écarts d'AIC sont < 3 points (négligeables).
#  - Choix retenu : cacao + sucre + Brent
#      * sucre : intrant du chocolat et produit tropical concurrent ;
#      * Brent : coûts d'énergie, de transport et d'engrais, choc commun ;
#      * urée exclue : corrélée au Brent (0,85) et rupture de série en 2022
#        (changement de marché de référence).

# =============================================================================
# 4.3 SÉLECTION DES RETARDS : AIC vs BIC
# =============================================================================
formule <- larabica ~ lcacao + lsucre + lbrent

sel_AIC <- auto_ardl(formule, data = data_ts, max_order = 4, selection = "AIC")
sel_BIC <- auto_ardl(formule, data = data_ts, max_order = 4, selection = "BIC")
head(sel_AIC$top_orders, 10)   # 10 meilleures combinaisons de retards (AIC)
head(sel_BIC$top_orders, 10)   # 10 meilleures combinaisons de retards (BIC)

tab_aic_bic <- rbind(evaluer(c("lcacao", "lsucre", "lbrent"), "AIC"),
                     evaluer(c("lcacao", "lsucre", "lbrent"), "BIC"))
print(tab_aic_bic, row.names = FALSE)
# Lecture :
#  - AIC -> ARDL(2,1,1,0) ; BIC -> ARDL(1,0,1,0), plus parcimonieux.
#  - Le modèle BIC laisse de l'autocorrélation dans les résidus
#    (Breusch-Godfrey p < 0,01) et rejette la normalité : ses retards sont
#    insuffisants. Or l'absence d'autocorrélation est une condition de
#    validité du Bounds test (Pesaran et al., 2001).
#  - On retient donc le modèle sélectionné par l'AIC.

# =============================================================================
# 4.4 ROBUSTESSE AU NOMBRE MAXIMAL DE RETARDS
# =============================================================================
tab_maxlag <- do.call(rbind, lapply(c(3, 4, 6, 8, 12), function(mo) {
  r <- evaluer(c("lcacao", "lsucre", "lbrent"), "AIC", max_order = mo)
  cbind(Retards_max = mo, r[, c("Ordre", "AIC", "F_bounds", "p_F", "LT_cacao")])
}))
print(tab_maxlag, row.names = FALSE)
# -> le même ARDL(2,1,1,0) est retenu quel que soit le maximum (3 à 12 mois)

# =============================================================================
# 4.5 ESTIMATION DU MODÈLE RETENU
# =============================================================================
model_final <- sel_AIC$best_model
summary(model_final)
# Un bon modèle a une constante (cours) : elle est présente, même si elle
# n'est pas significative.

# Forme UECM (celle sur laquelle porte le Bounds test)
uecm_final <- uecm(model_final)
summary(uecm_final)

# =============================================================================
# 4.6 BOUNDS TEST (Pesaran, Shin et Smith, 2001)
# =============================================================================
# Choix du « case » :
#  - case 3 = constante non restreinte, pas de tendance. Justification :
#    un prix relatif de matières premières n'a pas de raison économique de
#    suivre une tendance déterministe ; on laisse la constante libre, ce qui
#    est le cas standard des applications sur prix (Pesaran et al., 2001).
#  - case 2 (constante restreinte, comme en cours) estimé en complément.
# Règle de décision :
#   F > borne I(1) -> cointégration
#   F < borne I(0) -> pas de cointégration
#   entre les deux -> zone d'indécision

bounds_f_test(model_final, case = 3)   # sortie détaillée
bounds_t_test(model_final, case = 3)

tab_bounds <- do.call(rbind, lapply(list(c(3, 0.10), c(3, 0.05), c(3, 0.01),
                                         c(2, 0.05)), function(cs) {
  b <- bounds_f_test(model_final, case = cs[1], alpha = cs[2])
  data.frame(Case = cs[1], Seuil = paste0(100 * cs[2], " %"),
             F = round(b$tab$statistic, 3),
             Borne_I0 = round(b$tab[, 2], 3), Borne_I1 = round(b$tab[, 3], 3),
             p_value = round(b$p.value, 4),
             Decision = ifelse(b$tab$statistic > b$tab[, 3], "Cointégration",
                        ifelse(b$tab$statistic < b$tab[, 2], "Pas de cointégration",
                               "Indécision")))
}))
bt <- bounds_t_test(model_final, case = 3, alpha = 0.05)
tab_bounds <- rbind(tab_bounds, data.frame(
  Case = 3, Seuil = "5 % (test t)", F = round(bt$tab$statistic, 3),
  Borne_I0 = round(bt$tab[, 2], 3), Borne_I1 = round(bt$tab[, 3], 3),
  p_value = round(bt$p.value, 4),
  Decision = ifelse(bt$tab$statistic < bt$tab[, 3], "Cointégration", "Non")))
names(tab_bounds)[3] <- "Statistique"
print(tab_bounds, row.names = FALSE)
# -> F = 5,88 > borne I(1) même au seuil de 1 % (5,55) : cointégration.
#    Le test t confirme (-3,99 < -3,77).

# Rappel : dans le sens inverse (cacao = Y), le Bounds test ne rejette
# jamais H0 (p > 0,25 ; voir etape3_exploration_sens.R, tableau 7).

# ---- Export -----------------------------------------------------------------
write.csv(tab_comb,    "tableau_4a_combinaisons_controles.csv", row.names = FALSE)
write.csv(tab_aic_bic, "tableau_4b_aic_vs_bic.csv",             row.names = FALSE)
write.csv(tab_maxlag,  "tableau_4c_retards_max.csv",            row.names = FALSE)
write.csv(tab_bounds,  "tableau_4d_bounds_test.csv",            row.names = FALSE)
cf <- coef(summary(model_final))
write.csv(data.frame(Variable = rownames(cf), round(cf, 4)),
          "tableau_4e_estimation_ardl.csv", row.names = FALSE)

# Étape suivante (étape 5) : coefficients de long terme, ECM, diagnostics,
# stabilité CUSUM/CUSUMSQ -> etape5_resultats.R
