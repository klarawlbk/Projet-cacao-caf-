# Figures du rapport (format compact) – à lancer après etape1 (base CSV)
suppressMessages({library(ARDL); library(strucchange)})
data <- read.csv("base_cacao_cafe_2000_2026.csv"); data$date <- as.Date(data$date)
data_ts <- ts(data[, -1], start = c(2000, 1), frequency = 12)

# Figure 1 : cacao et cafés, base 100
b100 <- function(x) 100 * x / x[1]
ans <- as.Date(paste0(seq(2000, 2025, 5), "-01-01"))
png("fig1_cacao_cafe.png", width = 2400, height = 900, res = 220)
par(mar = c(2.2, 4.2, 0.6, 0.8))
plot(data$date, b100(data$cacao), type = "l", lwd = 2, col = "#7b3f00", log = "y",
     ylim = c(40, 1300), xlab = "", ylab = "Base 100 = janv. 2000 (log)", las = 1, xaxt = "n")
axis.Date(1, at = ans, format = "%Y")
lines(data$date, b100(data$arabica), lwd = 2, col = "#1f4e79")
lines(data$date, b100(data$robusta), lwd = 1.4, col = "#6fa8dc", lty = 2)
legend("topleft", bty = "n", legend = c("Cacao", "Café Arabica", "Café Robusta"),
       col = c("#7b3f00", "#1f4e79", "#6fa8dc"), lwd = c(2, 2, 1.4), lty = c(1, 1, 2), cex = 0.9)
abline(v = ans, h = c(50, 100, 200, 500, 1000), col = "lightgray", lty = 3); dev.off()

# Modèle final
m <- auto_ardl(larabica ~ lcacao + lsucre + lbrent, data = data_ts, max_order = 4,
               selection = "AIC")$best_model

# Figure 2 : CUSUM et CUSUMSQ
y <- m$model[, 1]; X <- model.matrix(m)
cus <- efp(y ~ X - 1, type = "Rec-CUSUM")
w <- recresid(y ~ X - 1); s <- cumsum(w^2) / sum(w^2); esp <- seq_along(w) / length(w)
m_ <- (length(y) - ncol(X)) / 2 - 1; c0 <- 1.358 / sqrt(m_) - 0.6675 / m_
tt <- tail(as.numeric(time(residuals(m))), length(w))
png("fig2_cusum.png", width = 2400, height = 820, res = 220)
par(mfrow = c(1, 2), mar = c(2.2, 3, 1.8, 0.8))
plot(cus, main = "CUSUM", xlab = "", ylab = "", alpha = 0.05, cex.main = 1)
plot(tt, s, type = "l", lwd = 1.8, col = "#1f4e79", ylim = c(-0.15, 1.15),
     main = "CUSUM des carrés", xlab = "", ylab = "", cex.main = 1, las = 1)
lines(tt, esp + c0, lty = 2, col = "red"); lines(tt, esp - c0, lty = 2, col = "red")
lines(tt, esp, col = "grey60"); dev.off()

# Figure 3 : équilibre de long terme
th <- setNames(multipliers(m)$Estimate, multipliers(m)$Term)
eq <- th["(Intercept)"] + th["lcacao"] * data$lcacao + th["lsucre"] * data$lsucre +
  th["lbrent"] * data$lbrent
ec <- data$larabica - eq
png("fig3_equilibre.png", width = 2400, height = 1100, res = 220)
layout(matrix(1:2), heights = c(1.5, 1))
par(mar = c(0.6, 4.2, 0.6, 0.8))
plot(data$date, data$larabica, type = "l", lwd = 1.8, col = "#1f4e79", xaxt = "n",
     ylim = range(c(data$larabica, eq)), xlab = "", ylab = "log prix Arabica", las = 1)
lines(data$date, eq, lwd = 1.8, col = "#7b3f00", lty = 2)
legend("topleft", bty = "n", lwd = 1.8, lty = c(1, 2), col = c("#1f4e79", "#7b3f00"), cex = 0.9,
       legend = c("Prix observé", "Équilibre de long terme (cacao, sucre, Brent)"))
abline(v = ans, col = "lightgray", lty = 3)
par(mar = c(2.2, 4.2, 0.4, 0.8))
plot(data$date, ec, type = "h", col = ifelse(ec > 0, "#1f4e79", "#c0504d"), xlab = "",
     ylab = "Écart", las = 1, xaxt = "n")
axis.Date(1, at = ans, format = "%Y")
abline(h = 0); abline(v = ans, col = "lightgray", lty = 3); dev.off()
