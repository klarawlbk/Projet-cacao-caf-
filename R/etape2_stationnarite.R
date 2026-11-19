# =============================================================================
#  PROJET D'ÉCONOMÉTRIE – « Du cacao au café »
#  ÉTAPE 2 – Analyse préliminaire : stationnarité et ordres d'intégration
#            (partie 4 du rapport)
#
#  Prérequis : avoir lancé etape1_donnees.R (objet `data`), ou relire la base
#  exportée base_cacao_cafe_2000_2026.csv.
# =============================================================================

# ---- 0. Packages ------------------------------------------------------------
# install.packages(c("tseries", "urca"))   # une seule fois
library(tseries)   # adf.test, kpss.test, pp.test  (fonctions du cours)
library(urca)      # ur.df, ur.kpss, ur.za        (versions détaillées)

# ---- 1. Données -------------------------------------------------------------
if (!exists("data") || !is.data.frame(data)) {   # `data` existe aussi en base R
  data <- read.csv("base_cacao_cafe_2000_2026.csv")
  data$date <- as.Date(data$date)
}
vars <- c(lcacao   = "log Cacao",
          larabica = "log Café Arabica",
          lrobusta = "log Café Robusta",
          lsucre   = "log Sucre",
          lbrent   = "log Brent",
          luree    = "log Urée")

# ---- 2. Observation visuelle et ACF (cours, section 3.2) -------------------
# Une ACF qui décroît lentement est un signe de non-stationnarité.
png("figure4_acf_niveaux.png", width = 2400, height = 1600, res = 220)
par(mfrow = c(3, 2), mar = c(4, 4, 3, 1))
for (v in names(vars)) acf(data[[v]], lag.max = 36, main = paste("ACF –", vars[v]))
dev.off()

png("figure5_series_differenciees.png", width = 2400, height = 1600, res = 220)
par(mfrow = c(3, 2), mar = c(3, 4, 2.5, 1))
for (v in names(vars)) {
  plot(data$date[-1], diff(data[[v]]), type = "l", col = "#1f4e79",
       main = paste("Δ", vars[v], "(variation mensuelle)"), xlab = "", ylab = "")
  abline(h = 0, col = "grey50"); grid()
}
dev.off()

png("figure6_acf_differences.png", width = 2400, height = 1600, res = 220)
par(mfrow = c(3, 2), mar = c(4, 4, 3, 1))
for (v in names(vars)) acf(diff(data[[v]]), lag.max = 36,
                           main = paste("ACF – Δ", vars[v]))
dev.off()

# ---- 3. Tests du cours (tseries) -------------------------------------------
# ADF   : H0 = racine unitaire (non stationnaire). p < 0,05 -> stationnaire.
# KPSS  : H0 = stationnaire.                       p < 0,05 -> non stationnaire.
# PP    : comme l'ADF, robuste à l'hétéroscédasticité (complément).
# En niveau : les séries ont une tendance -> ADF avec tendance, KPSS "Trend".
# En différence : pas de tendance -> KPSS "Level".
# NB : tseries borne les p-values KPSS/PP à [0,01 ; 0,10] (valeurs tabulées).

tests_cours <- function(x) {
  dx <- diff(x)
  c(ADF_niv  = adf.test(x)$p.value,
    KPSS_niv = suppressWarnings(kpss.test(x, null = "Trend")$p.value),
    PP_niv   = suppressWarnings(pp.test(x)$p.value),
    ADF_dif  = suppressWarnings(adf.test(dx)$p.value),
    KPSS_dif = suppressWarnings(kpss.test(dx, null = "Level")$p.value),
    PP_dif   = suppressWarnings(pp.test(dx)$p.value))
}
tab_cours <- t(sapply(data[names(vars)], tests_cours))
print(round(tab_cours, 3))

# ---- 4. Tests détaillés (urca) : statistiques et valeurs critiques ---------
# ADF : nombre de retards choisi par AIC (max 12 en mensuel).
#   Niveau     : type = "trend" (constante + tendance)
#   Différence : type = "drift" (constante seule)
# KPSS : lags = "long" (fenêtre de Newey-West plus large, recommandé en mensuel)
#   Niveau : type = "tau" (tendance) | Différence : type = "mu" (constante)

res <- data.frame()
for (v in names(vars)) {
  x  <- data[[v]]; dx <- diff(x)
  a0 <- ur.df(x,  type = "trend", lags = 12, selectlags = "AIC")
  a1 <- ur.df(dx, type = "drift", lags = 12, selectlags = "AIC")
  k0 <- ur.kpss(x,  type = "tau", lags = "long")
  k1 <- ur.kpss(dx, type = "mu",  lags = "long")
  res <- rbind(res, data.frame(
    Serie          = vars[v],
    ADF_niv_stat   = a0@teststat[1], ADF_niv_vc5  = a0@cval[1, "5pct"],
    KPSS_niv_stat  = k0@teststat,    KPSS_niv_vc5 = k0@cval[1, "5pct"],
    ADF_dif_stat   = a1@teststat[1], ADF_dif_vc5  = a1@cval[1, "5pct"],
    KPSS_dif_stat  = k1@teststat,    KPSS_dif_vc5 = k1@cval[1, "5pct"]))
}
# Règles de décision :
#   ADF  : stat < valeur critique  -> on rejette H0 -> stationnaire
#   KPSS : stat > valeur critique  -> on rejette H0 -> non stationnaire
res$Niveau_stationnaire <- with(res, ifelse(ADF_niv_stat < ADF_niv_vc5, "oui (ADF)", "non (ADF)"))
res$Niveau_KPSS         <- with(res, ifelse(KPSS_niv_stat > KPSS_niv_vc5, "non (KPSS)", "oui (KPSS)"))
res$Diff_stationnaire   <- with(res, ifelse(ADF_dif_stat < ADF_dif_vc5, "oui (ADF)", "non (ADF)"))
res$Diff_KPSS           <- with(res, ifelse(KPSS_dif_stat > KPSS_dif_vc5, "non (KPSS)", "oui (KPSS)"))
rownames(res) <- NULL
print(res, digits = 3)

# ---- 5. Vérification qu'aucune série n'est I(2) ----------------------------
# Si la différence première est stationnaire, la série n'est pas I(2).
# Contre-vérification : ADF sur la différence seconde (doit rejeter H0 très
# nettement, sinon la série serait I(3) – cas absurde pour un prix).
for (v in names(vars)) {
  p2 <- suppressWarnings(adf.test(diff(data[[v]], differences = 2))$p.value)
  cat(sprintf("%-18s ADF sur Δ² : p = %.3f\n", vars[v], p2))
}

# ---- 6. Robustesse : rupture structurelle (Zivot-Andrews) -------------------
# Le pic du cacao en 2024-2025 peut biaiser l'ADF vers la non-stationnarité
# (Perron, 1989). Zivot-Andrews teste H0 = racine unitaire contre
# H1 = stationnaire avec UNE rupture endogène (date choisie par le test).
za_tab <- data.frame()
for (v in names(vars)) {
  z <- ur.za(data[[v]], model = "both", lag = 4)
  za_tab <- rbind(za_tab, data.frame(
    Serie = vars[v], ZA_stat = z@teststat, vc5 = z@cval[2],
    Date_rupture = format(data$date[z@bpoint], "%Y-%m"),
    Rejet_H0 = z@teststat < z@cval[2]))
}
print(za_tab, digits = 3)

# ---- 7. Export --------------------------------------------------------------
write.csv(round(tab_cours, 3), "tableau4_pvalues_tests_cours.csv")
num <- sapply(res, is.numeric); res_exp <- res; res_exp[num] <- round(res[num], 3)
write.csv(res_exp, "tableau5_adf_kpss_urca.csv", row.names = FALSE)
write.csv(za_tab, "tableau6_zivot_andrews.csv", row.names = FALSE)

# Conclusion attendue -> voir le commentaire dans le rapport (partie 4)
