-- =====================================================================
--  Base de données SQL « Du cacao au café » – PostgreSQL, dans le terminal
--  1) Créer la base (une seule fois) :
--       createdb cacao_cafe
--  2) Exécuter ce script :
--       psql -d cacao_cafe -f base_cacao_cafe_postgresql.sql
--  (le fichier donnees_sql.csv, exporté par R, doit être dans le même dossier)
-- =====================================================================

-- 1. Créer la table : une ligne par mois, une colonne par prix (comme sous Excel)
DROP TABLE IF EXISTS prix;
CREATE TABLE prix (
  date     DATE    PRIMARY KEY,                     -- mois, unique
  cacao    NUMERIC NOT NULL CHECK (cacao   > 0),    -- $/kg
  arabica  NUMERIC NOT NULL CHECK (arabica > 0),    -- $/kg
  robusta  NUMERIC NOT NULL CHECK (robusta > 0),    -- $/kg
  sucre    NUMERIC NOT NULL CHECK (sucre   > 0),    -- $/kg
  brent    NUMERIC NOT NULL CHECK (brent   > 0),    -- $/baril
  uree     NUMERIC NOT NULL CHECK (uree    > 0)     -- $/tonne
);

-- 2. Importer le fichier CSV
\copy prix FROM 'donnees_sql.csv' WITH (FORMAT csv, HEADER true)

-- 3. Requêtes de vérification et d'exploration
-- (a) Combien de mois, de quand à quand ?
SELECT COUNT(*) AS nb_mois, MIN(date) AS debut, MAX(date) AS fin FROM prix;

-- (b) Y a-t-il des valeurs manquantes ?
SELECT COUNT(*) AS lignes_incompletes FROM prix
WHERE cacao IS NULL OR arabica IS NULL OR robusta IS NULL
   OR sucre IS NULL OR brent IS NULL OR uree IS NULL;

-- (c) Prix moyens par année
SELECT EXTRACT(YEAR FROM date) AS annee,
       ROUND(AVG(cacao), 2)   AS cacao_moyen,
       ROUND(AVG(arabica), 2) AS arabica_moyen
FROM prix
GROUP BY annee
ORDER BY annee;

-- (d) Les cinq mois où le cacao a été le plus cher
SELECT date, cacao, arabica FROM prix ORDER BY cacao DESC LIMIT 5;
