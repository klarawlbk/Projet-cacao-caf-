# =============================================================================
#  Du cacao au café – Travailler sous R directement sur la base PostgreSQL
#  Les données ne sont PAS lues dans le fichier Excel : elles viennent de la
#  base cacao_cafe créée dans le terminal (scripts du dossier SQL/).
# =============================================================================
# ---- 0. Packages : installés automatiquement s'ils manquent, puis chargés ---
paquets <- c("DBI", "RPostgres", "tseries", "ARDL")
a_installer <- paquets[!paquets %in% rownames(installed.packages())]
if (length(a_installer) > 0) install.packages(a_installer)
library(DBI); library(RPostgres); library(tseries); library(ARDL)

# ---- 1. Connexion à la base PostgreSQL --------------------------------------
con <- dbConnect(Postgres(),
                 dbname   = "cacao_cafe",          # base créée dans le terminal
                 host     = "localhost",
                 port     = 5432,
                 user     = "postgres",
                 password = "postgres")
con                                         # informations sur la connexion

# ---- 2. Explorer la base depuis R --------------------------------------------
dbListTables(con)                           # tables présentes : prix, variables
dbListFields(con, "prix")                   # colonnes de la table prix
dbReadTable(con, "variables")               # description des séries

# Requêtes SQL envoyées depuis R : le calcul est fait par PostgreSQL
dbGetQuery(con, "SELECT COUNT(*) AS nb_mois, MIN(date) AS debut, MAX(date) AS fin
                 FROM prix")
dbGetQuery(con, "SELECT EXTRACT(YEAR FROM date) AS annee,
                        ROUND(AVG(cacao), 2)   AS cacao_moyen,
                        ROUND(AVG(arabica), 2) AS arabica_moyen
                 FROM prix
                 WHERE date >= '2020-01-01'
                 GROUP BY annee ORDER BY annee")

# ---- 3. Importer la table prix dans R ---------------------------------------
prix <- dbGetQuery(con, "SELECT * FROM prix ORDER BY date")
str(prix)                                   # 321 obs. de 7 variables
head(prix)
stopifnot(nrow(prix) == 321, !anyNA(prix))

# ---- 4. Statistiques descriptives en niveau (tableau B1 du mémoire) ---------
asymetrie <- function(x) mean((x - mean(x))^3) / sd(x)^3
kurtosis  <- function(x) mean((x - mean(x))^4) / sd(x)^4
series <- c("cacao", "arabica", "robusta", "sucre", "brent", "uree")

stats_niveau <- t(sapply(prix[, series], function(x) c(
  Moyenne    = mean(x),
  Ecart_type = sd(x),
  Mediane    = median(x),
  Min        = min(x),
  Max        = max(x),
  Asymetrie  = asymetrie(x),
  Kurtosis   = kurtosis(x),
  p_JB       = jarque.bera.test(x)$p.value)))
print(round(stats_niveau, 3))
# Les prix en niveau sont très asymétriques (surtout le cacao : 2,47) et
# non normaux (p Jarque-Bera < 0,05) : d'où le passage en logarithme.

# ---- 5. Le modèle ARDL estimé sur les données de la base --------------------
for (v in series) prix[[paste0("l", v)]] <- log(prix[[v]])
data_ts <- ts(prix[, paste0("l", series)], start = c(2000, 1), frequency = 12)

modele <- auto_ardl(larabica ~ lcacao + lsucre + lbrent, data = data_ts,
                    max_order = 4, selection = "AIC")$best_model
modele$order                                # (2,1,1,0)
bounds_f_test(modele, case = 3)             # F = 5,88 : cointégration
multipliers(modele)                         # élasticité au cacao : 0,869
# Mêmes résultats qu'avec le fichier Excel : la base SQL est fiable.

# ---- 6. Renvoyer les résultats dans la base PostgreSQL ----------------------
tab <- data.frame(serie = rownames(stats_niveau), round(stats_niveau, 3),
                  row.names = NULL)
names(tab) <- tolower(names(tab))           # noms de colonnes en minuscules pour SQL
dbWriteTable(con, "stats_niveau", tab, overwrite = TRUE)

lt <- multipliers(modele)
dbWriteTable(con, "resultats_long_terme",
             data.frame(variable = lt$Term, elasticite = round(lt$Estimate, 3),
                        p_value = round(lt$`Pr(>|t|)`, 4)),
             overwrite = TRUE)

dbListTables(con)                           # prix, variables + 2 nouvelles tables
dbGetQuery(con, "SELECT serie, moyenne, ecart_type, min, max
                 FROM stats_niveau ORDER BY moyenne DESC")

dbDisconnect(con)
