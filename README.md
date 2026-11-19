# Du cacao au café : existe-t-il une relation de long terme entre les prix mondiaux de ces deux matières premières ?

Projet d'économétrie – Master 1  
William Ngoiem et Klara Willebrouck

Ce dépôt contient l'ensemble du code utilisé dans notre mémoire : l'analyse économétrique sous **R** et la base de données sous **PostgreSQL**, créée et interrogée dans un terminal Linux (Ubuntu sous WSL).

## Résultat principal

Les prix du cacao et du café Arabica sont cointégrés, mais dans un seul sens : le prix du café s'ajuste vers un équilibre de long terme fixé par le cacao (avec le sucre et le Brent en contrôles).

- Modèle ARDL(2,1,1,0), test des bornes de Pesaran, Shin et Smith (2001) : F = 5,88 (p = 0,006)
- Élasticité de long terme du café au cacao : 0,87
- Terme de correction d'erreur : −0,060, soit une demi-vie d'environ 11 mois
- Tous les diagnostics sont validés (normalité, autocorrélation, hétéroscédasticité, RESET, CUSUM)

## Contenu

```
R/
  projet_cacao_cafe.R          script complet et reproductible (celui de l'annexe F)
  etape1_donnees.R             import du Pink Sheet, préparation, statistiques descriptives
  etape2_stationnarite.R       tests ADF, KPSS, PP, Zivot-Andrews
  etape3_exploration_sens.R    test des bornes dans les deux sens (choix de la variable expliquée)
  etape4_modelisation_ardl.R   analyse combinatoire des contrôles, auto_ardl, AIC/BIC, test des bornes
  etape5_resultats.R           long terme, ECM, diagnostics (check_*), CUSUM
  etape6_robustesse.R          variantes de robustesse, Toda-Yamamoto, exogénéité faible
  figures_rapport.R            figures du mémoire
  CMO-Historical-Data-Monthly.xlsx   données brutes (Banque mondiale, Pink Sheet)
  base_cacao_cafe_2000_2026.csv      base préparée (créée par etape1)
SQL/
  base_cacao_cafe_postgresql.sql     création de la table, import du CSV, requêtes de vérification
  analyses_sql_postgresql.sql        analyses complémentaires (jointure, vues, fenêtrage, CORR, REGR)
  donnees_sql.csv                    données importées dans PostgreSQL (exportées par R)
figures/                             figures du mémoire et captures du terminal
```

## Reproduire l'analyse sous R

1. Ouvrir RStudio et choisir le dossier `R/` comme répertoire de travail (*Session > Set Working Directory > To Source File Location*).
2. Installer les packages une seule fois :
   ```r
   install.packages(c("readxl", "DBI", "RPostgreSQL", "tseries", "urca", "ARDL",
                      "performance", "lmtest", "car", "strucchange", "vars"))
   ```
3. Lancer `projet_cacao_cafe.R` (tout en un) ou les scripts `etape1` à `etape6` dans l'ordre.

La connexion à PostgreSQL dans la partie 2 du script est facultative : si la base n'existe pas sur l'ordinateur, le script continue normalement.

## Reproduire la base SQL (PostgreSQL)

Dans un terminal Linux, depuis le dossier `SQL/` :

```bash
createdb cacao_cafe
psql -d cacao_cafe -f base_cacao_cafe_postgresql.sql
psql -P pager=off -d cacao_cafe -f analyses_sql_postgresql.sql
```

Le premier script crée la table `prix` (clé primaire sur la date, contraintes `CHECK` de prix positifs) et importe les 321 mois. Le second ajoute une table `variables`, deux vues et calcule des statistiques directement en SQL ; la corrélation obtenue (0,79 en niveau, 0,22 en variation) est identique à celle calculée sous R.

## Données

Banque mondiale, *Commodity Markets – Pink Sheet*, prix mensuels de janvier 2000 à septembre 2026 : cacao, café Arabica, café Robusta, sucre, pétrole Brent, urée.  
https://www.worldbank.org/en/research/commodity-markets
