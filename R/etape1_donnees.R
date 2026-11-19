# =============================================================================
#  PROJET D'ÉCONOMÉTRIE – Master 1
#  « Du cacao au café : existe-t-il une relation de long terme entre les prix
#    mondiaux de ces deux matières premières ? »
#
#  ÉTAPE 1 – Construction de la base de données (partie 3 du rapport)
#
#  Source unique : Banque mondiale, Commodity Price Data (« Pink Sheet »),
#  fichier CMO-Historical-Data-Monthly.xlsx, feuille « Monthly Prices »,
#  édition du 2 octobre 2026.
#  https://www.worldbank.org/en/research/commodity-markets
# =============================================================================

# ---- 0. Packages ------------------------------------------------------------
# install.packages(c("readxl", "tseries"))   # à lancer une seule fois
library(readxl)
library(tseries)     # jarque.bera.test (normalité des séries)

# ---- 1. Import --------------------------------------------------------------
# >>> Adapter le chemin à votre ordinateur <<<
chemin <- "C:/Users/willi/Downloads/CMO-Historical-Data-Monthly.xlsx"
if (!file.exists(chemin)) chemin <- "CMO-Historical-Data-Monthly.xlsx"

# Les 4 premières lignes du fichier sont des titres ; la ligne 5 contient les
# noms des séries, la ligne 6 les unités, les données commencent ligne 7.
brut <- read_excel(chemin, sheet = "Monthly Prices", skip = 4)
View(brut)

unites <- unlist(brut[1, ])          # on garde les unités pour vérification
brut   <- brut[-1, ]                 # on retire la ligne des unités
names(brut)[1] <- "periode"

# ---- 2. Sélection des variables retenues -----------------------------------
series <- c(cacao   = "Cocoa",
            arabica = "Coffee, Arabica",
            robusta = "Coffee, Robusta",
            sucre   = "Sugar, world",
            brent   = "Crude oil, Brent",
            uree    = "Urea")

# Vérification des unités telles qu'écrites dans le fichier
print(unites[series])
#   Cocoa, Arabica, Robusta, Sugar : $/kg   |  Brent : $/bbl  |  Urea : $/mt

data <- data.frame(periode = brut$periode)
for (v in names(series)) {
  # les valeurs indisponibles sont codées "…" ou ".." -> deviennent NA
  data[[v]] <- suppressWarnings(as.numeric(brut[[series[v]]]))
}

# ---- 3. Dates ---------------------------------------------------------------
# "2000M01" -> date du premier jour du mois
data$date <- as.Date(paste0(substr(data$periode, 1, 4), "-",
                            substr(data$periode, 6, 7), "-01"))

# ---- 4. Période d'étude : janvier 2000 – septembre 2026 ---------------------
debut <- as.Date("2000-01-01")
fin   <- as.Date("2026-09-01")
data  <- data[data$date >= debut & data$date <= fin, ]
data  <- data[, c("date", names(series))]
rownames(data) <- NULL

# ---- 5. Contrôles de qualité ------------------------------------------------
cat("Nombre d'observations :", nrow(data), "\n")                 # 321
cat("Première date :", format(min(data$date)),
    "- Dernière date :", format(max(data$date)), "\n")
cat("Valeurs manquantes par série :\n"); print(colSums(is.na(data)))
# Continuité mensuelle : aucun mois ne doit manquer
mois_attendus <- seq(debut, fin, by = "month")
cat("Mois manquants :", length(setdiff(mois_attendus, data$date)), "\n")
cat("Prix négatifs ou nuls :", sum(data[, -1] <= 0, na.rm = TRUE), "\n")

# ---- 6. Transformation logarithmique ---------------------------------------
# Justification : (i) stabiliser la variance (forte hausse des niveaux et de la
# volatilité en fin de période), (ii) lire les coefficients comme des
# élasticités dans le modèle ARDL.
for (v in names(series)) data[[paste0("l", v)]] <- log(data[[v]])
# lcacao, larabica, lrobusta, lsucre, lbrent, luree

# Objet série temporelle (utile pour les tests de l'étape 2)
data_ts <- ts(data[, -1], start = c(2000, 1), frequency = 12)

# ---- 7. Statistiques descriptives (Tableau 2 du rapport) -------------------
asymetrie <- function(x) mean((x - mean(x))^3) / sd(x)^3
kurtosis  <- function(x) mean((x - mean(x))^4) / sd(x)^4

stats_desc <- function(df) {
  t(sapply(df, function(x) c(
    N          = length(x),
    Moyenne    = mean(x),
    Ecart_type = sd(x),
    Min        = min(x),
    Mediane    = median(x),
    Max        = max(x),
    CV_pct     = 100 * sd(x) / mean(x),
    Asymetrie  = asymetrie(x),
    Kurtosis   = kurtosis(x),
    JB_pvalue  = jarque.bera.test(x)$p.value)))
}
tab_niveaux <- stats_desc(data[, names(series)])
tab_logs    <- stats_desc(data[, paste0("l", names(series))])
tab_logs    <- tab_logs[, colnames(tab_logs) != "CV_pct"]  # CV sans sens en log
print(round(tab_niveaux, 3))
print(round(tab_logs, 3))

# Matrice de corrélation (en log) – premier aperçu des co-mouvements
cor_logs <- cor(data[, paste0("l", names(series))])
print(round(cor_logs, 2))
# Corrélation des variations mensuelles (log-différences)
cor_dlogs <- cor(apply(data[, paste0("l", names(series))], 2, diff))
print(round(cor_dlogs, 2))

# ---- 8. Graphiques ----------------------------------------------------------
# Figure 1 : les six prix en niveau
png("figure1_prix_niveaux.png", width = 2400, height = 1600, res = 220)
par(mfrow = c(3, 2), mar = c(3, 4, 2.5, 1))
titres <- c(cacao = "Cacao ($/kg)", arabica = "Café Arabica ($/kg)",
            robusta = "Café Robusta ($/kg)", sucre = "Sucre, monde ($/kg)",
            brent = "Pétrole Brent ($/baril)", uree = "Urée ($/tonne)")
for (v in names(series)) {
  plot(data$date, data[[v]], type = "l", lwd = 1.3, col = "#1f4e79",
       main = titres[v], xlab = "", ylab = "")
  grid()
}
dev.off()

# Figure 2 : cacao vs Arabica et Robusta, en log, base 100 = janvier 2000
base100 <- function(x) 100 * x / x[1]
png("figure2_cacao_cafe_base100.png", width = 2400, height = 1300, res = 220)
par(mar = c(3, 4.5, 2.5, 1))
plot(data$date, base100(data$cacao), type = "l", lwd = 2, col = "#7b3f00",
     log = "y", ylim = c(40, 1500), xlab = "",
     ylab = "Indice (janv. 2000 = 100, échelle log)",
     main = "Prix mondiaux du cacao et du café, 2000–2026")
lines(data$date, base100(data$arabica), lwd = 2, col = "#1f4e79")
lines(data$date, base100(data$robusta), lwd = 1.5, col = "#6fa8dc", lty = 2)
abline(v = as.Date(c("2024-01-01", "2025-06-01")), lty = 3, col = "grey40")
legend("topleft", bty = "n",
       legend = c("Cacao", "Café Arabica", "Café Robusta"),
       col = c("#7b3f00", "#1f4e79", "#6fa8dc"), lwd = c(2, 2, 1.5),
       lty = c(1, 1, 2))
grid()
dev.off()

# Figure 3 : séries en log (celles qui entreront dans le modèle)
png("figure3_series_log.png", width = 2400, height = 1600, res = 220)
par(mfrow = c(3, 2), mar = c(3, 4, 2.5, 1))
for (v in names(series)) {
  plot(data$date, data[[paste0("l", v)]], type = "l", lwd = 1.3,
       col = "#1f4e79", main = paste0("log ", titres[v]), xlab = "", ylab = "")
  grid()
}
dev.off()

# ---- 9. Export de la base propre -------------------------------------------
write.csv(data, "base_cacao_cafe_2000_2026.csv", row.names = FALSE)
write.csv(round(tab_niveaux, 3), "tableau2_stats_niveaux.csv")
write.csv(round(tab_logs, 3),    "tableau2b_stats_logs.csv")
write.csv(round(cor_logs, 2),    "tableau3_correlations_logs.csv")

# Fin de l'étape 1 -> l'objet `data` (niveaux + logs) est prêt pour
# l'étape 2 : ADF / KPSS sur lcacao, larabica, lrobusta, lsucre, lbrent, luree.
