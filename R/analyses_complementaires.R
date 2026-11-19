# =============================================================================
#  Du cacao au café – Analyses complémentaires
#  1. Estimation récursive : l'ARDL(2,1,1,0) réestimé sur un échantillon qui
#     s'allonge d'un mois à chaque fois (2000-2009, 2000-2010, ..., 2000-2026),
#     comparé à des fenêtres glissantes de 10 ans
#  2. Prévision hors échantillon : estimation 2000-2023, prévision 2024-2026,
#     comparée à une marche aléatoire et à un modèle sans cacao
#  À lancer depuis le dossier R/ (lit base_cacao_cafe_2000_2026.csv)
# =============================================================================
library(ARDL); library(jsonlite)
d <- read.csv("base_cacao_cafe_2000_2026.csv"); d$date <- as.Date(d$date)
vars <- c("larabica", "lcacao", "lsucre", "lbrent")
ordre <- c(2, 1, 1, 0)                         # ordre du modèle final

# ---- 1. Élasticité de long terme : estimation récursive et glissante --------
L <- 120
fins <- seq(L, nrow(d))
estime <- function(debut, t) {
  sub <- ts(d[debut:t, vars], frequency = 12)
  m <- ardl(larabica ~ lcacao + lsucre + lbrent, data = sub, order = ordre)
  mu <- multipliers(m)
  ect <- sum(coef(m)[c("L(larabica, 1)", "L(larabica, 2)")]) - 1
  data.frame(fin = format(d$date[t]), elast = mu$Estimate[2], se = mu$`Std. Error`[2],
             p_bornes = unname(bounds_f_test(m, case = 3)$p.value), ect = unname(ect))
}
recur <- do.call(rbind, lapply(fins, function(t) estime(1, t)))        # 1er mois fixe
glis  <- do.call(rbind, lapply(fins, function(t) estime(t - L + 1, t))) # 10 ans glissants
print(recur[seq(1, nrow(recur), 24), ], digits = 3)
cat("Récursif : élasticité de", round(min(recur$elast), 2), "à", round(max(recur$elast), 2),
    "; part cointégrée à 5 % :", round(mean(recur$p_bornes < 0.05), 2), "\n")
cat("Récursif depuis 2015 : élasticité de", round(min(recur$elast[recur$fin >= "2015-01-01"]), 2), "à",
    round(max(recur$elast[recur$fin >= "2015-01-01"]), 2), "\n")
cat("Glissant 10 ans : part cointégrée à 5 % :", round(mean(glis$p_bornes < 0.05), 2), "\n")

# ---- 2. Prévision hors échantillon -------------------------------------------
# Estimation sur 2000-01 à 2023-12, puis prévision dynamique de 2024-01 à
# 2026-09 : seuls les prix du cacao, du sucre et du Brent observés sont
# utilisés ; le prix du café est prévu de proche en proche, sans jamais voir
# sa vraie valeur (prévision conditionnelle aux explicatives).
fin_est <- which(d$date == as.Date("2023-12-01"))
m_est <- ardl(larabica ~ lcacao + lsucre + lbrent,
              data = ts(d[1:fin_est, vars], frequency = 12), order = ordre)
cf <- coef(m_est); print(round(cf, 4))
y <- d$larabica; yhat <- y
for (t in (fin_est + 1):nrow(d)) {
  yhat[t] <- cf["(Intercept)"] + cf["L(larabica, 1)"] * yhat[t - 1] +
    cf["L(larabica, 2)"] * yhat[t - 2] + cf["lcacao"] * d$lcacao[t] +
    cf["L(lcacao, 1)"] * d$lcacao[t - 1] + cf["lsucre"] * d$lsucre[t] +
    cf["L(lsucre, 1)"] * d$lsucre[t - 1] + cf["lbrent"] * d$lbrent[t]
}
h <- (fin_est + 1):nrow(d)
naif <- rep(y[fin_est], length(h))            # marche aléatoire : prix figé
# Modèle de comparaison sans le cacao (sucre et Brent seulement, retards par AIC)
m_sc <- auto_ardl(larabica ~ lsucre + lbrent, data = ts(d[1:fin_est, vars], frequency = 12),
                  max_order = 4, selection = "AIC")$best_model
sc <- coef(m_sc); ysc <- y
for (t in h) {
  ysc[t] <- sc["(Intercept)"] + sum(sapply(grep("^L\\(larabica", names(sc), value = TRUE),
              function(n) sc[n] * ysc[t - as.integer(sub(".*, (\\d+)\\)", "\\1", n))])) +
    sum(sapply(setdiff(names(sc), c("(Intercept)", grep("larabica", names(sc), value = TRUE))), function(n) {
      v <- if (grepl("lsucre", n)) "lsucre" else "lbrent"
      k <- if (grepl("^L\\(", n)) as.integer(sub(".*, (\\d+)\\)", "\\1", n)) else 0
      sc[n] * d[[v]][t - k] }))
}
cat("Modèle sans cacao :", paste(m_sc$order, collapse = ","), "\n")
rmse <- function(e) sqrt(mean(e^2))
res_prev <- c(rmse_ardl = rmse(y[h] - yhat[h]), rmse_naif = rmse(y[h] - naif),
              rmse_sans_cacao = rmse(y[h] - ysc[h]),
              mae_ardl = mean(abs(y[h] - yhat[h])), mae_naif = mean(abs(y[h] - naif)),
              mae_sans_cacao = mean(abs(y[h] - ysc[h])))
print(round(res_prev, 4))
cat("Gain de précision (RMSE) :", round(100 * (1 - res_prev[1] / res_prev[2]), 1), "%\n")
cat("Gain par rapport au modèle sans cacao :", round(100 * (1 - res_prev[1] / res_prev[3]), 1), "%\n")
print(data.frame(date = d$date[h], reel = round(exp(y[h]), 2), prevu = round(exp(yhat[h]), 2),
                 sans_cacao = round(exp(ysc[h]), 2))[seq(1, length(h), 3), ])

write_json(list(
  recursive = recur, glissante = glis,
  prevision = list(dates = format(d$date[(fin_est - 47):nrow(d)]),
                   reel = round(exp(y[(fin_est - 47):nrow(d)]), 3),
                   prevu = c(rep(NA, 48), round(exp(yhat[h]), 3)),
                   naif = c(rep(NA, 48), round(exp(naif), 3)),
                   sans_cacao = c(rep(NA, 48), round(exp(ysc[h]), 3)),
                   debut = format(d$date[fin_est + 1]),
                   rmse = as.list(round(res_prev, 4)))),
  "../assets/data/complements.json", digits = 6, auto_unbox = TRUE, dataframe = "columns", na = "null")

# ---- 3. Figures pour le mémoire (annexe E) ----------------------------------
dr <- as.Date(recur$fin)
png("../figures/figE2_recursive.png", width = 2400, height = 950, res = 220)
par(mar = c(2.4, 4.2, 0.8, 0.8))
plot(dr, recur$elast, type = "n", ylim = c(-0.2, 2.2), las = 1, xlab = "",
     ylab = "Élasticité de long terme")
polygon(c(dr, rev(dr)), c(recur$elast - 1.96 * recur$se, rev(recur$elast + 1.96 * recur$se)),
        col = adjustcolor("#1f4e79", 0.15), border = NA)
abline(h = c(0, 0.869), lty = c(1, 2), col = c("grey60", "#7b3f00"))
lines(dr, recur$elast, lwd = 2, col = "#1f4e79")
points(dr[recur$p_bornes < 0.05], recur$elast[recur$p_bornes < 0.05], pch = 16, cex = 0.45, col = "#2f7d4a")
legend("topright", bty = "n", cex = 0.85, lwd = c(2, 8, 1, NA), pch = c(NA, NA, NA, 16),
       col = c("#1f4e79", adjustcolor("#1f4e79", 0.15), "#7b3f00", "#2f7d4a"), lty = c(1, 1, 2, NA),
       legend = c("Élasticité (échantillon débutant en 2000)", "Intervalle à 95 %",
                  "Modèle final (0,87)", "Cointégration à 5 %"))
dev.off()

pr <- (fin_est - 47):nrow(d); dp <- d$date[pr]
png("../figures/figE3_prevision.png", width = 2400, height = 950, res = 220)
par(mar = c(2.4, 4.2, 0.8, 0.8))
plot(dp, exp(y[pr]), type = "n", ylim = c(1.5, 10), las = 1, xlab = "", ylab = "Café Arabica ($/kg)")
rect(d$date[fin_est + 1], 0, max(dp) + 20, 11, col = "grey93", border = NA)
lines(dp, exp(y[pr]), lwd = 2.2, col = "black")
lines(d$date[h], exp(yhat[h]), lwd = 2.2, col = "#1f4e79", lty = 2)
lines(d$date[h], exp(ysc[h]), lwd = 1.8, col = "#c0504d", lty = 3)
lines(d$date[h], exp(naif), lwd = 1.4, col = "grey50", lty = 4)
legend("topleft", bty = "n", cex = 0.85, lwd = c(2.2, 2.2, 1.8, 1.4), lty = c(1, 2, 3, 4),
       col = c("black", "#1f4e79", "#c0504d", "grey50"),
       legend = c("Prix observé", "Prévision ARDL (estimé sur 2000-2023)",
                  "Prévision sans le cacao", "Marche aléatoire (prix de déc. 2023)"))
text(d$date[fin_est + 2], 9.6, "Période de prévision", adj = 0, cex = 0.8, col = "grey40")
dev.off()
