# Du cacao au café : existe-t-il une relation de long terme entre les prix mondiaux de ces deux matières premières ?

Projet d'économétrie – Master 1 (2026)  
Klara Willebrouck, William Ngoie et Chryst-Emmanuel Igondjo Nguema

**Site interactif du projet : https://klarawlbk.github.io/Projet-cacao-caf-/**

Le site présente la démarche et les résultats, et permet de :
- explorer les six séries de prix (zoom sur une période, base 100 ou prix) ;
- suivre la corrélation glissante cacao-café selon l'horizon choisi ;
- visualiser le test des bornes selon le seuil et le sens du modèle ;
- comparer le prix du café à son équilibre de long terme, mois par mois ;
- simuler un choc sur le prix du cacao et voir la réponse du café (multiplicateurs dynamiques du modèle) ;
- interroger la base de données en SQL directement dans le navigateur ;
- lire tous les scripts R et SQL.

## Résultat principal

Les prix du cacao et du café Arabica sont cointégrés, mais dans un seul sens : le prix du café s'ajuste vers un équilibre de long terme fixé par le cacao (avec le sucre et le Brent en contrôles).

- ARDL(2,1,1,0), test des bornes de Pesaran, Shin et Smith (2001) : F = 5,88 (p = 0,006)
- Élasticité de long terme du café au cacao : 0,87
- Terme de correction d'erreur : −0,060, soit une demi-vie d'environ 11 mois
- Diagnostics validés, relation stable (CUSUM), résultat confirmé par 8 variantes

## Contenu du dépôt

```
index.html                  site de présentation (GitHub Pages)
assets/                     style, scripts et données du site
  data/modele.json          séries, équilibre, multiplicateurs (créé par R/donnees_site.R)
  data/resultats.json       tableaux de résultats (stationnarité, robustesse, diagnostics)
  vendor/                   bibliothèque locale sql.js
R/
  projet_cacao_cafe.R       script complet et reproductible (annexe F du mémoire)
  travail_R_base_SQL.R      R connecté à PostgreSQL : requêtes, stats en niveau, ARDL, écriture dans la base
  etape1_donnees.R … etape6_robustesse.R   les six étapes de l'analyse
  donnees_site.R            calcule les données du site à partir du modèle
  figures_rapport.R         figures du mémoire
  CMO-Historical-Data-Monthly.xlsx, base_cacao_cafe_2000_2026.csv   données
SQL/
  base_cacao_cafe_postgresql.sql   création de la table prix, import, vérifications
  analyses_sql_postgresql.sql      jointure, vues, fenêtrage, CORR, REGR
  donnees_sql.csv                  données importées dans PostgreSQL
figures/                    figures du mémoire et captures (terminal Linux, RStudio)
```

## Reproduire l'analyse

**Base de données** (terminal Linux, dossier `SQL/`) :

```bash
createdb cacao_cafe
psql -d cacao_cafe -f base_cacao_cafe_postgresql.sql
psql -P pager=off -d cacao_cafe -f analyses_sql_postgresql.sql
```

**Analyse sous R** (RStudio, dossier `R/` comme répertoire de travail) :

```r
install.packages(c("readxl", "DBI", "RPostgres", "tseries", "urca", "ARDL",
                   "performance", "lmtest", "car", "strucchange", "vars", "jsonlite"))
source("travail_R_base_SQL.R")   # R branché sur la base PostgreSQL
source("projet_cacao_cafe.R")    # toute l'analyse
```

## Données

Banque mondiale, *Commodity Markets – Pink Sheet*, prix mensuels de janvier 2000 à septembre 2026 : cacao, café Arabica, café Robusta, sucre, pétrole Brent, urée.  
https://www.worldbank.org/en/research/commodity-markets

## Bibliothèques utilisées par le site

sql.js (SQLite compilé en WebAssembly, licence MIT), copiée dans `assets/vendor/`. La coloration du code est intégrée au site.
