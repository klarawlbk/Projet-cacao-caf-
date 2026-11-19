# =============================================================================
#  PROJET D'ÉCONOMÉTRIE – « Du cacao au café »
#  ÉTAPE 3 (exploration) – Analyse combinatoire et choix du sens du modèle
#
#  Ce script reproduit tous les résultats annoncés :
#   A. Cacao = Y      -> 16 modèles (8 combinaisons de contrôles × AIC/BIC)
#   B. Cacao = Y      -> robustesse : Robusta, sous-échantillon, dummies 2024
#   C. Café  = Y      -> 32 modèles (Arabica et Robusta × 8 combinaisons × AIC/BIC)
#   D. Sous-périodes  -> les deux sens comparés
#   E. Détail du modèle recommandé avec les fonctions du cours
#
#  Prérequis : base_cacao_cafe_2000_2026.csv (étape 1) dans le dossier de travail
# =============================================================================

# ---- 0. Packages ------------------------------------------------------------
# install.packages(c("ARDL", "performance", "lmtest", "car", "tseries"))
library(ARDL)
library(performance)   # check_normality, check_autocorrelation, ...
library(lmtest)        # bgtest (Breusch-Godfrey)
library(car)           # vif

# ---- 1. Données -------------------------------------------------------------
data <- read.csv("base_cacao_cafe_2000_2026.csv")
data$date <- as.Date(data$date)

# Transforme un sous-ensemble de la base en série temporelle mensuelle
# (nécessaire pour que les retards et les dates soient corrects dans ARDL)
en_ts <- function(df) {
  debut <- c(as.integer(format(df$date[1], "%Y")),
             as.integer(format(df$date[1], "%m")))
  ts(df[, setdiff(names(df), "date")], start = debut, frequency = 12)
}
data_ts <- en_ts(data)

# ---- 2. Fonction qui estime UN modèle et résume tous les tests --------------
# y    : variable dépendante (texte), ex. "lcacao"
# xs   : variables explicatives (vecteur), la 1re est la variable d'intérêt
# sel  : critère de sélection des retards ("AIC" ou "BIC")
# debut, fin : période d'estimation
resume_modele <- function(y, xs, sel = "AIC",
                          debut = "2000-01-01", fin = "2026-09-01") {
  df <- data[data$date >= as.Date(debut) & data$date <= as.Date(fin), ]
  formule <- as.formula(paste(y, "~", paste(xs, collapse = " + ")))

  # Sélection automatique des retards (max 4 mois) – comme en cours
  m <- auto_ardl(formule, data = en_ts(df), max_order = 4,
                 selection = sel)$best_model

  # Bounds test de Pesaran (case 3 = constante non restreinte, sans tendance)
  bf <- bounds_f_test(m, case = 3)
  bt <- bounds_t_test(m, case = 3)

  # Coefficient de long terme de la variable d'intérêt (élasticité)
  lr <- multipliers(m)
  lr <- lr[lr$Term == xs[1], ]

  # VIF calculé sur la relation de long terme (régression en niveaux) :
  # dans l'ARDL lui-même, les retards d'une même variable sont
  # mécaniquement colinéaires, le VIF n'y a donc pas de sens.
  vif_max <- if (length(xs) > 1) max(vif(lm(formule, data = df))) else NA

  data.frame(
    Y         = y,
    X         = paste(xs, collapse = " + "),
    Periode   = paste(substr(debut, 1, 7), "à", substr(fin, 1, 7)),
    Critere   = sel,
    Ordre     = paste(m$order, collapse = ","),
    AIC       = round(AIC(m), 1),
    BIC       = round(BIC(m), 1),
    F_bounds  = round(bf$statistic, 2),
    p_F       = round(bf$p.value, 3),
    p_t       = round(bt$p.value, 3),
    LT_coef   = round(lr$Estimate, 3),
    LT_p      = round(lr[[5]], 3),
    p_normal  = round(as.numeric(check_normality(m)), 3),
    p_hetero  = round(as.numeric(check_heteroscedasticity(m)), 3),
    p_autoc   = round(as.numeric(check_autocorrelation(m)), 3),
    p_BG12    = round(bgtest(m, order = 12)$p.value, 3),
    VIF_max   = round(vif_max, 2)
  )
}

# ---- 3. Toutes les combinaisons de contrôles (analyse combinatoire) --------
controles <- c("lsucre", "lbrent", "luree")
# 2^3 = 8 combinaisons : aucune, 1 contrôle (3), 2 contrôles (3), les 3 (1)
combinaisons <- c(list(character(0)),
                  unlist(lapply(1:3, function(k) combn(controles, k, simplify = FALSE)),
                         recursive = FALSE))
length(combinaisons)   # 8

# ---- A. Sens initial : CACAO = Y, ARABICA = X (16 modèles) ------------------
res_A <- do.call(rbind, lapply(combinaisons, function(cb)
  rbind(resume_modele("lcacao", c("larabica", cb), "AIC"),
        resume_modele("lcacao", c("larabica", cb), "BIC"))))
print(res_A[, c("X", "Critere", "Ordre", "F_bounds", "p_F", "LT_coef", "LT_p", "p_normal")],
      row.names = FALSE)
# -> p_F entre 0,25 et 0,80 : AUCUNE cointégration ; normalité rejetée partout

# ---- B. Cacao = Y : vérifications complémentaires ---------------------------
res_B <- rbind(
  resume_modele("lcacao", "lrobusta"),                          # Robusta
  resume_modele("lcacao", c("lrobusta", "lsucre", "lbrent")),
  resume_modele("lcacao", c("larabica", "lsucre", "lbrent"),    # sans 2024-26
                fin = "2023-12-01"))
print(res_B[, c("X", "Periode", "F_bounds", "p_F", "p_normal")], row.names = FALSE)

# Avec des dummies impulsionnelles sur les mois de crise (|résidu| > 3 é.-t.)
# Syntaxe ARDL : les variables après « | » sont fixes (sans retards)
crise <- c("2001-03", "2002-11", "2024-02", "2024-03", "2024-04",
           "2024-05", "2024-12", "2026-02", "2026-05")
data_d <- data
for (mois in crise) data_d[[paste0("d", sub("-", "", mois))]] <-
  as.numeric(format(data_d$date, "%Y-%m") == mois)
f_d <- as.formula(paste("lcacao ~ larabica + lsucre + lbrent |",
                        paste0("d", sub("-", "", crise), collapse = " + ")))
m_d <- auto_ardl(f_d, data = en_ts(data_d), max_order = 4, selection = "AIC")$best_model
bounds_f_test(m_d, case = 3)$p.value    # ≈ 0,39 -> toujours pas de cointégration

# ---- C. Sens inverse : CAFÉ = Y, CACAO = X (32 modèles) ---------------------
res_C <- do.call(rbind, lapply(c("larabica", "lrobusta"), function(y)
  do.call(rbind, lapply(combinaisons, function(cb)
    rbind(resume_modele(y, c("lcacao", cb), "AIC"),
          resume_modele(y, c("lcacao", cb), "BIC"))))))
print(res_C, row.names = FALSE)
# -> cointégration dans les 32 modèles (p_F ≤ 0,055), élasticité 0,71 à 1,23

# ---- D. Sous-périodes : les deux sens comparés ------------------------------
periodes <- list(c("2000-01-01", "2013-12-01"), c("2000-01-01", "2019-12-01"),
                 c("2000-01-01", "2023-12-01"), c("2010-01-01", "2026-09-01"))
res_D <- do.call(rbind, lapply(periodes, function(p)
  rbind(resume_modele("lcacao",   "larabica", debut = p[1], fin = p[2]),
        resume_modele("larabica", "lcacao",   debut = p[1], fin = p[2]))))
print(res_D[, c("Y", "X", "Periode", "F_bounds", "p_F", "LT_coef", "LT_p")],
      row.names = FALSE)
# -> cacao = Y : jamais cointégré ; café = Y : toujours cointégré

# ---- E. Le modèle recommandé, détaillé comme en cours -----------------------
model1 <- auto_ardl(larabica ~ lcacao + lsucre + lbrent, data = data_ts,
                    max_order = 4, selection = "AIC")
model1$top_orders            # les modèles les plus performants
model_final <- model1$best_model
summary(model_final)         # ARDL(2,1,1,0)

check_normality(model_final)          # p = 0,096 -> vert
check_autocorrelation(model_final)    # vert
check_heteroscedasticity(model_final) # vert
bgtest(model_final, order = 12)       # Breusch-Godfrey 12 retards

bounds_f_test(model_final, case = 3)  # F = 5,88 ; p = 0,006 -> cointégration
multipliers(model_final)              # élasticité de long terme au cacao = 0,87
summary(recm(model_final, case = 3))  # ECM : terme de correction ≈ -0,06

# ---- Export -----------------------------------------------------------------
write.csv(res_A, "tableau7_cacaoY_combinaisons.csv", row.names = FALSE)
write.csv(res_C, "tableau8_cafeY_combinaisons.csv",  row.names = FALSE)
write.csv(res_D, "tableau9_sous_periodes.csv",       row.names = FALSE)
