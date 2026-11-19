# =============================================================================
#  PROJET D'ÉCONOMÉTRIE – « Du cacao au café »
#  ÉTAPE 6 – Robustesse et sens de la causalité (partie 6 du rapport)
#
#   6.1  Tableau de robustesse : le modèle principal face à 8 variantes
#        (variété de café, contrôles, période, crise 2024-2025)
#   6.2  Spécification inverse : le cacao expliqué par le café
#   6.3  Causalité de Granger (procédure de Toda-Yamamoto)
#   6.4  Exogénéité faible : qui s'ajuste à qui ?
#
#  Prérequis : base_cacao_cafe_2000_2026.csv dans le dossier de travail
# =============================================================================

# ---- 0. Packages ------------------------------------------------------------
# install.packages(c("ARDL", "performance", "lmtest", "car", "vars"))
library(ARDL)
library(performance)
library(lmtest)
library(car)      # linearHypothesis (test de Wald)
library(vars)     # VARselect (choix du nombre de retards du VAR)

# ---- 1. Données -------------------------------------------------------------
data <- read.csv("base_cacao_cafe_2000_2026.csv")
data$date <- as.Date(data$date)

# Dummy de crise du cacao (flambée de 2024-2025), utilisée en 6.1
data$crise <- as.numeric(data$date >= as.Date("2024-01-01") &
                         data$date <= as.Date("2025-12-01"))

en_ts <- function(df) {
  ts(df[, setdiff(names(df), "date")],
     start = c(as.integer(format(df$date[1], "%Y")),
               as.integer(format(df$date[1], "%m"))), frequency = 12)
}

# ---- 2. Fonction : estime une variante et résume tous les résultats ---------
# y : variable dépendante ; xs : régresseurs (le 1er est la variable
# d'intérêt) ; fixes : régresseurs sans retard (dummies) ; période.
variante <- function(nom, y, xs, fixes = NULL,
                     debut = "2000-01-01", fin = "2026-09-01") {
  df <- data[data$date >= as.Date(debut) & data$date <= as.Date(fin), ]
  f  <- paste(y, "~", paste(xs, collapse = " + "))
  if (!is.null(fixes)) f <- paste(f, "|", paste(fixes, collapse = " + "))
  m  <- auto_ardl(as.formula(f), data = en_ts(df), max_order = 4,
                  selection = "AIC")$best_model
  bf <- bounds_f_test(m, case = 3)
  lt <- multipliers(m); lt <- lt[lt$Term == xs[1], ]
  ec <- coef(summary(recm(m, case = 3)))["ect", ]
  data.frame(
    Variante   = nom,
    Periode    = paste(substr(debut, 1, 7), "-", substr(fin, 1, 7)),
    N          = nobs(m),
    Ordre      = paste0("(", paste(m$order, collapse = ","), ")"),
    F_bounds   = round(bf$statistic, 2),
    p_F        = round(bf$p.value, 4),
    Elast_LT   = round(lt$Estimate, 3),
    p_LT       = round(lt[[5]], 4),
    ECT        = round(ec["Estimate"], 3),
    p_ECT      = round(ec["Pr(>|t|)"], 4),
    Demi_vie   = round(log(0.5) / log(1 + ec["Estimate"]), 1),
    p_normal   = round(as.numeric(check_normality(m)), 3),
    p_BG12     = round(bgtest(m, order = 12)$p.value, 3),
    p_hetero   = round(as.numeric(check_heteroscedasticity(m)), 3),
    row.names  = NULL)
}

# =============================================================================
# 6.1 TABLEAU DE ROBUSTESSE (café = Y, cacao = X)
# =============================================================================
ctrl <- c("lsucre", "lbrent")
tab_rob <- rbind(
  # Référence
  variante("Modèle principal (Arabica)",     "larabica", c("lcacao", ctrl)),
  # (a) Autre variété de café
  variante("Robusta au lieu d'Arabica",      "lrobusta", c("lcacao", ctrl)),
  # (b) Autres contrôles
  variante("Sans Brent",                     "larabica", c("lcacao", "lsucre")),
  variante("Avec urée",                      "larabica", c("lcacao", ctrl, "luree")),
  variante("Cacao seul (bivarié)",           "larabica", "lcacao"),
  # (c) Crise du cacao 2024-2025
  variante("Dummy crise 2024-2025",          "larabica", c("lcacao", ctrl), fixes = "crise"),
  variante("Avant la crise",                 "larabica", c("lcacao", ctrl), fin = "2023-12-01"),
  # (d) Autres sous-périodes
  variante("Avant le Covid",                 "larabica", c("lcacao", ctrl), fin = "2019-12-01"),
  variante("Depuis 2010",                    "larabica", c("lcacao", ctrl), debut = "2010-01-01")
)
print(tab_rob, row.names = FALSE)

# Lecture :
#  - Cointégration dans les 9 variantes (p_F < 0,05)
#  - Élasticité au cacao entre 0,69 et 1,16, toujours significative
#  - ECT négatif et significatif partout (demi-vie de 8 à 17 mois)
#  - Diagnostics verts sauf : bivarié (normalité 0,019) et « depuis 2010 »
#    (autocorrélation BG12 = 0,011) -> variantes secondaires, à signaler

# =============================================================================
# 6.2 SPÉCIFICATION INVERSE : CACAO = Y
# =============================================================================
tab_inv <- rbind(
  variante("Inverse : cacao ~ Arabica + contrôles", "lcacao", c("larabica", ctrl)),
  variante("Inverse : cacao ~ Robusta + contrôles", "lcacao", c("lrobusta", ctrl)),
  variante("Inverse : cacao ~ Arabica (bivarié)",   "lcacao", "larabica"),
  variante("Inverse : avant la crise",              "lcacao", c("larabica", ctrl),
           fin = "2023-12-01"))
# Sans cointégration, les colonnes ECT / demi-vie n'ont pas d'interprétation
# (la p-value de l'ECT ne suit pas une loi de Student sous H0 : c'est
# justement pour cela qu'on utilise les bornes de Pesaran). On ne garde que
# les colonnes pertinentes.
tab_inv <- tab_inv[, c("Variante", "Periode", "N", "Ordre", "F_bounds", "p_F",
                       "Elast_LT", "p_LT", "p_normal", "p_BG12", "p_hetero")]
print(tab_inv, row.names = FALSE)
# -> Jamais de cointégration (p_F > 0,35) : le cacao ne s'ajuste pas au café.
#    En outre, normalité et homoscédasticité sont rejetées.

# =============================================================================
# 6.3 CAUSALITÉ DE GRANGER : PROCÉDURE DE TODA-YAMAMOTO (1995)
# =============================================================================
# Pourquoi Toda-Yamamoto ? Le test de Granger classique suppose des séries
# stationnaires. Nos séries sont I(1). Toda-Yamamoto permet de tester en
# niveaux : on estime un VAR avec p + dmax retards (dmax = 1, ordre
# d'intégration maximal) mais on ne teste que les p premiers retards.
#
# H0 : « X ne cause pas Y au sens de Granger »
#      (les p premiers retards de X sont tous nuls dans l'équation de Y)

# (a) Nombre de retards p du VAR en niveaux (critère AIC, max 12 mois)
var_data <- data[, c("larabica", "lcacao", "lsucre", "lbrent")]
sel <- VARselect(var_data, lag.max = 12, type = "const")
sel$selection
p    <- sel$selection["AIC(n)"]
dmax <- 1                      # toutes les séries sont I(1) (étape 2)
cat("Retards du VAR : p =", p, "| retards estimés : p + dmax =", p + dmax, "\n")

# (b) Fonction de test
toda_yamamoto <- function(y, x, p, dmax, autres = c("lsucre", "lbrent")) {
  vars_eq <- c(y, x, autres)
  L <- p + dmax
  # Construction des retards 1..L de chaque variable
  lags <- do.call(cbind, lapply(vars_eq, function(v)
    sapply(1:L, function(l) dplyr::lag(data[[v]], l))))
  colnames(lags) <- unlist(lapply(vars_eq, function(v) paste0(v, "_L", 1:L)))
  df <- data.frame(y = data[[y]], lags)[-(1:L), ]
  reg <- lm(y ~ ., data = df)
  # On teste uniquement les p premiers retards de x (pas le retard dmax)
  h0 <- paste0(x, "_L", 1:p, " = 0")
  w  <- linearHypothesis(reg, h0, test = "Chisq")
  data.frame(H0 = paste(x, "ne cause pas", y),
             Retards_testes = p,
             Chi2 = round(w$Chisq[2], 2),
             p_value = round(w$`Pr(>Chisq)`[2], 4),
             Decision = ifelse(w$`Pr(>Chisq)`[2] < 0.05,
                               "Rejet : causalité", "Pas de causalité"))
}

tab_granger <- rbind(
  toda_yamamoto("larabica", "lcacao",   p, dmax),
  toda_yamamoto("lcacao",   "larabica", p, dmax),
  toda_yamamoto("lrobusta", "lcacao",   p, dmax),
  toda_yamamoto("lcacao",   "lrobusta", p, dmax))
print(tab_granger, row.names = FALSE)

# (c) Sensibilité au nombre de retards testés (p = 1 à 6)
tab_granger_p <- do.call(rbind, lapply(1:6, function(pp) data.frame(
  p = pp,
  p_cacao_vers_cafe = toda_yamamoto("larabica", "lcacao",   pp, dmax)$p_value,
  p_cafe_vers_cacao = toda_yamamoto("lcacao",   "larabica", pp, dmax)$p_value)))
print(tab_granger_p, row.names = FALSE)
# Lecture : aucune causalité de Granger « par les retards », dans aucun sens,
# quel que soit p. Ce test ne capte que la capacité des prix PASSÉS du cacao
# à prévoir le café. Or la transmission passe (i) par l'effet du MÊME mois
# (d(lcacao) = 0,155 dans l'ECM), exclu par construction du test de Granger,
# et (ii) par l'ajustement lent vers l'équilibre (6 %/mois), trop diffus pour
# être détecté sur quelques retards. La causalité de long terme se teste
# donc par le terme de correction d'erreur (section 6.4).

# =============================================================================
# 6.4 EXOGÉNÉITÉ FAIBLE : QUI S'AJUSTE À L'ÉQUILIBRE ?
# =============================================================================
# On calcule l'écart à l'équilibre de long terme (estimé à l'étape 4) et on
# regarde s'il aide à prévoir la variation de chaque prix le mois suivant.
#  - Dans l'équation du café : coefficient négatif et significatif attendu
#    (le café corrige l'écart).
#  - Dans l'équation du cacao : coefficient non significatif attendu
#    (le cacao ignore l'écart = variable « faiblement exogène »).
model_final <- auto_ardl(larabica ~ lcacao + lsucre + lbrent, data = en_ts(data),
                         max_order = 4, selection = "AIC")$best_model
lt <- multipliers(model_final)
th <- setNames(lt$Estimate, lt$Term)
data$ecart <- data$larabica - (th["(Intercept)"] + th["lcacao"] * data$lcacao +
                               th["lsucre"] * data$lsucre + th["lbrent"] * data$lbrent)

d  <- function(x) c(NA, diff(x))
l1 <- function(x) c(NA, head(x, -1))
df_ex <- data.frame(
  d_cafe  = d(data$larabica), d_cacao = d(data$lcacao),
  ecart_1 = l1(data$ecart),
  d_cafe_1 = l1(d(data$larabica)), d_cacao_1 = l1(d(data$lcacao)),
  d_sucre_1 = l1(d(data$lsucre)), d_brent_1 = l1(d(data$lbrent)))
df_ex <- na.omit(df_ex)

eq_cafe  <- lm(d_cafe  ~ ecart_1 + d_cafe_1 + d_cacao_1 + d_sucre_1 + d_brent_1, data = df_ex)
eq_cacao <- lm(d_cacao ~ ecart_1 + d_cafe_1 + d_cacao_1 + d_sucre_1 + d_brent_1, data = df_ex)

tab_exo <- rbind(
  data.frame(Equation = "Δ café (Arabica)", t(round(coef(summary(eq_cafe))["ecart_1", ], 4))),
  data.frame(Equation = "Δ cacao",          t(round(coef(summary(eq_cacao))["ecart_1", ], 4))))
names(tab_exo) <- c("Equation", "Coef_ecart", "Ecart_type", "t", "p_value")
print(tab_exo, row.names = FALSE)
# -> Café : -0,054 (p < 0,001) : il corrige l'écart à l'équilibre.
#    Cacao : +0,009 (p = 0,58) : il l'ignore -> faiblement exogène.
#    Causalité de LONG TERME du cacao vers le café, pas l'inverse.

# ---- Export -----------------------------------------------------------------
write.csv(tab_rob,     "tableau15_robustesse.csv",           row.names = FALSE)
write.csv(tab_inv,     "tableau16_specification_inverse.csv", row.names = FALSE)
write.csv(tab_granger, "tableau17_granger_toda_yamamoto.csv", row.names = FALSE)
write.csv(tab_granger_p, "tableau17b_granger_sensibilite.csv", row.names = FALSE)
write.csv(tab_exo,     "tableau18_exogeneite_faible.csv",     row.names = FALSE)
