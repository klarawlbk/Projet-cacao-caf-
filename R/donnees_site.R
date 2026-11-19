# =============================================================================
#  Données du site de présentation (écrit ../assets/data/modele.json)
#  Séries, équilibre de long terme, multiplicateurs dynamiques, test des bornes
#  À lancer depuis le dossier R/ (lit base_cacao_cafe_2000_2026.csv)
# =============================================================================
library(ARDL); library(jsonlite)
d  <- read.csv("base_cacao_cafe_2000_2026.csv")
ts_ <- ts(d[, -1], start = c(2000, 1), frequency = 12)
m  <- auto_ardl(larabica ~ lcacao + lsucre + lbrent, data = ts_,
                max_order = 4, selection = "AIC")$best_model
cf <- coef(m); lt <- multipliers(m)
th <- setNames(lt$Estimate, lt$Term)

# Équilibre de long terme et écart (en log)
eq  <- th["(Intercept)"] + th["lcacao"] * d$lcacao + th["lsucre"] * d$lsucre +
       th["lbrent"] * d$lbrent
gap <- d$larabica - eq

# Multiplicateurs dynamiques cumulés d'un choc permanent sur le log du cacao
a1 <- cf["L(larabica, 1)"]; a2 <- cf["L(larabica, 2)"]
b0 <- cf["lcacao"];        b1 <- cf["L(lcacao, 1)"]
mult <- numeric(61)
for (h in 0:60) {
  m1 <- if (h >= 1) mult[h] else 0; m2 <- if (h >= 2) mult[h - 1] else 0
  mult[h + 1] <- a1 * m1 + a2 * m2 + b0 + (if (h >= 1) b1 else 0)
}

bornes <- do.call(rbind, lapply(c(0.10, 0.05, 0.01), function(a) {
  b <- bounds_f_test(m, case = 3, alpha = a)
  data.frame(alpha = a, I0 = unname(b$parameters[1]), I1 = unname(b$parameters[2]))
}))

write_json(list(
  dates = d$date,
  prix  = d[, c("cacao", "arabica", "robusta", "sucre", "brent", "uree")],
  equilibre = round(unname(eq), 4), observe = round(d$larabica, 4),
  ecart = round(unname(gap), 4),
  multiplicateurs = round(unname(mult), 5),
  long_terme = round(th, 4),
  ect = -(1 - a1 - a2),
  F = unname(bounds_f_test(m, case = 3)$statistic),
  p_F = bounds_f_test(m, case = 3)$p.value,
  bornes = bornes), "../assets/data/modele.json", digits = 6, auto_unbox = TRUE, dataframe = "columns")
