# =============================================================================
#  Du cacao au café : existe-t-il une relation de long terme entre les prix
#  mondiaux de ces deux matières premières ?          — script complet (R)
#  Données : Banque mondiale, Pink Sheet, CMO-Historical-Data-Monthly.xlsx
# =============================================================================
# install.packages(c("readxl","DBI","RPostgres","tseries","urca","ARDL",
#                    "performance","lmtest","car","strucchange","vars"))
library(readxl); library(DBI); library(RPostgres); library(tseries); library(urca)
library(ARDL); library(performance); library(lmtest); library(car)
library(strucchange); library(vars)

# ---- 1. Import et préparation (partie 3) -----------------------------------
brut <- read_excel("CMO-Historical-Data-Monthly.xlsx", sheet = "Monthly Prices", skip = 4)
brut <- brut[-1, ]                                  # ligne des unités
series <- c(cacao = "Cocoa", arabica = "Coffee, Arabica", robusta = "Coffee, Robusta",
            sucre = "Sugar, world", brent = "Crude oil, Brent", uree = "Urea")
data <- data.frame(date = as.Date(paste0(substr(brut[[1]], 1, 4), "-",
                                         substr(brut[[1]], 6, 7), "-01")))
for (v in names(series)) data[[v]] <- suppressWarnings(as.numeric(brut[[series[v]]]))
data <- data[data$date >= as.Date("2000-01-01") & data$date <= as.Date("2026-09-01"), ]
stopifnot(nrow(data) == 321, !anyNA(data))
for (v in names(series)) data[[paste0("l", v)]] <- log(data[[v]])
data_ts <- ts(data[, -1], start = c(2000, 1), frequency = 12)

# ---- 2. Base de données SQL PostgreSQL (annexe A) ---------------------------
# Export du fichier pour la base, puis dans le terminal :
#   createdb cacao_cafe
#   psql -d cacao_cafe -f base_cacao_cafe_postgresql.sql
write.csv(transform(data[, c("date", names(series))], date = format(date)),
          "donnees_sql.csv", row.names = FALSE)
con <- tryCatch(dbConnect(Postgres(), dbname = "cacao_cafe", host = "localhost",
                          user = "postgres", password = "postgres"),
                error = function(e) NULL)
if (!is.null(con)) {                                # lecture de la base SQL dans R
  prix_sql <- dbGetQuery(con, "SELECT * FROM prix ORDER BY date")
  stopifnot(nrow(prix_sql) == 321,
            isTRUE(all.equal(as.numeric(prix_sql$arabica), data$arabica)))
  dbDisconnect(con)
}

# ---- 3. Stationnarité (partie 4) --------------------------------------------
lv <- c("larabica", "lcacao", "lsucre", "lbrent", "lrobusta", "luree")
tab_stat <- t(sapply(lv, function(v) { x <- data[[v]]; dx <- diff(x)
  c(ADF_niv  = ur.df(x,  type = "trend", lags = 12, selectlags = "AIC")@teststat[1],
    KPSS_niv = ur.kpss(x, type = "tau", lags = "long")@teststat,
    ADF_dif  = ur.df(dx, type = "drift", lags = 12, selectlags = "AIC")@teststat[1],
    KPSS_dif = ur.kpss(dx, type = "mu", lags = "long")@teststat,
    ZA       = ur.za(x, model = "both", lag = 4)@teststat) }))
print(round(tab_stat, 3))   # VC 5 % : -3,42 | 0,146 | -2,87 | 0,463 | -5,08

# ---- 4. Choix du sens : test des bornes dans les deux sens (partie 4) -------
sens <- function(f) { m <- auto_ardl(f, data = data_ts, max_order = 4, selection = "AIC")$best_model
  bounds_f_test(m, case = 3)$p.value }
c(cacao_explique = sens(lcacao ~ larabica + lsucre + lbrent),     # 0,59
  cafe_explique  = sens(larabica ~ lcacao + lsucre + lbrent))     # 0,006

# ---- 5. Modélisation ARDL (partie 5) ----------------------------------------
# Analyse combinatoire des contrôles (2^3 = 8 modèles)
ctrl <- c("lsucre", "lbrent", "luree")
combis <- c(list(character(0)), unlist(lapply(1:3, function(k) combn(ctrl, k, simplify = FALSE)),
                                       recursive = FALSE))
tab_comb <- do.call(rbind, lapply(combis, function(cb) {
  f <- as.formula(paste("larabica ~", paste(c("lcacao", cb), collapse = " + ")))
  m <- auto_ardl(f, data = data_ts, max_order = 4, selection = "AIC")$best_model
  data.frame(controles = paste(cb, collapse = "+"), ordre = paste(m$order, collapse = ","),
             AIC = AIC(m), p_bornes = bounds_f_test(m, case = 3)$p.value,
             elast = multipliers(m)$Estimate[2], p_norm = as.numeric(check_normality(m))) }))
print(tab_comb[order(tab_comb$AIC), ], digits = 3)

# Sélection des retards : AIC contre BIC
sel_aic <- auto_ardl(larabica ~ lcacao + lsucre + lbrent, data = data_ts, max_order = 4, selection = "AIC")
sel_bic <- auto_ardl(larabica ~ lcacao + lsucre + lbrent, data = data_ts, max_order = 4, selection = "BIC")
sel_aic$best_model$order; sel_bic$best_model$order        # (2,1,1,0) et (1,0,1,0)
bgtest(sel_bic$best_model, order = 12)                    # p = 0,006 : BIC écarté

model_final <- sel_aic$best_model
summary(model_final)
bounds_f_test(model_final, case = 3, alpha = 0.01)        # F = 5,88 > I(1) = 5,55
bounds_f_test(model_final, case = 2, alpha = 0.05)        # contrôle, cas 2
bounds_t_test(model_final, case = 3, alpha = 0.05)

# ---- 6. Résultats (partie 6) ------------------------------------------------
multipliers(model_final)                                  # élasticité au cacao : 0,869
ecm <- recm(model_final, case = 3); summary(ecm)          # ECT : -0,060
log(0.5) / log(1 + coef(ecm)["ect"])                      # demi-vie : 11,3 mois

check_normality(model_final); check_autocorrelation(model_final)
check_heteroscedasticity(model_final); vif(lm(larabica ~ lcacao + lsucre + lbrent, data = data))
shapiro.test(residuals(model_final)); jarque.bera.test(residuals(model_final))
bgtest(model_final, order = 1); bgtest(model_final, order = 12); bptest(model_final)
resettest(model_final, power = 2:3, type = "fitted")
e2 <- residuals(model_final)^2; X <- embed(e2, 13)        # test ARCH(12) d'Engle
1 - pchisq((length(e2) - 12) * summary(lm(X[, 1] ~ X[, -1]))$r.squared, 12)

y <- model_final$model[, 1]; Xm <- model.matrix(model_final)
cusum <- efp(y ~ Xm - 1, type = "Rec-CUSUM"); sctest(cusum); plot(cusum)
w <- recresid(y ~ Xm - 1); s <- cumsum(w^2) / sum(w^2)   # CUSUM des carrés
m_ <- (length(y) - ncol(Xm)) / 2 - 1; c0 <- 1.358 / sqrt(m_) - 0.6675 / m_
any(abs(s - seq_along(w) / length(w)) > c0)               # FALSE : stable

# Causalité de Granger, procédure de Toda-Yamamoto (p = 2, dmax = 1)
p <- VARselect(data[, c("larabica", "lcacao", "lsucre", "lbrent")], lag.max = 12)$selection["AIC(n)"]
ty <- function(y, x, p, dmax = 1) {
  v <- c(y, x, "lsucre", "lbrent"); L <- p + dmax
  lags <- do.call(cbind, lapply(v, function(z) sapply(1:L, function(l) c(rep(NA, l), head(data[[z]], -l)))))
  colnames(lags) <- unlist(lapply(v, function(z) paste0(z, "_L", 1:L)))
  df <- data.frame(y = data[[y]], lags)[-(1:L), ]
  reg <- lm(y ~ ., data = df)
  linearHypothesis(reg, paste0(x, "_L", 1:p, " = 0"), test = "Chisq") }   # retard dmax non testé
ty("larabica", "lcacao", p); ty("lcacao", "larabica", p)  # p = 0,27 et 0,30

# Exogénéité faible : qui corrige l'écart à l'équilibre ?
th <- multipliers(model_final)$Estimate
ecart <- data$larabica - (th[1] + th[2] * data$lcacao + th[3] * data$lsucre + th[4] * data$lbrent)
d <- function(z) c(NA, diff(z)); l1 <- function(z) c(NA, head(z, -1))
summary(lm(d(data$larabica) ~ l1(ecart) + l1(d(data$larabica)) + l1(d(data$lcacao)) +
             l1(d(data$lsucre)) + l1(d(data$lbrent))))   # -0,054 ; p < 0,001
summary(lm(d(data$lcacao) ~ l1(ecart) + l1(d(data$larabica)) + l1(d(data$lcacao)) +
             l1(d(data$lsucre)) + l1(d(data$lbrent))))   # 0,009 ; p = 0,58
