-- =====================================================================
--  Analyses complémentaires en SQL – « Du cacao au café » (PostgreSQL)
--  À lancer APRÈS base_cacao_cafe_postgresql.sql :
--       psql -d cacao_cafe -f analyses_sql_postgresql.sql
-- =====================================================================

-- ---------------------------------------------------------------------
-- 1. Une deuxième table et une jointure : la description des variables
-- ---------------------------------------------------------------------
DROP VIEW  IF EXISTS v_variations;
DROP VIEW  IF EXISTS v_prix_long;
DROP TABLE IF EXISTS variables;

CREATE TABLE variables (
  code     TEXT PRIMARY KEY,
  libelle  TEXT NOT NULL,      -- nom de la série dans le Pink Sheet
  unite    TEXT NOT NULL,
  role     TEXT NOT NULL       -- rôle dans le modèle ARDL
);
INSERT INTO variables VALUES
  ('arabica', 'Coffee, Arabica',  '$/kg',    '1. Expliquée'),
  ('cacao',   'Cocoa',            '$/kg',    '2. Explicative principale'),
  ('sucre',   'Sugar, world',     '$/kg',    '3. Contrôle'),
  ('brent',   'Crude oil, Brent', '$/baril', '3. Contrôle'),
  ('robusta', 'Coffee, Robusta',  '$/kg',    '4. Robustesse'),
  ('uree',    'Urea',             '$/tonne', '5. Sensibilité');

-- Vue « longue » : une ligne par mois et par variable (dépivotage)
CREATE VIEW v_prix_long AS
SELECT p.date, v.code, v.valeur
FROM prix p
CROSS JOIN LATERAL (VALUES ('cacao', p.cacao), ('arabica', p.arabica),
                           ('robusta', p.robusta), ('sucre', p.sucre),
                           ('brent', p.brent), ('uree', p.uree)) AS v(code, valeur);

-- (e) Statistiques descriptives par variable, avec jointure sur la description
SELECT va.role, va.libelle, va.unite,
       COUNT(*)                        AS n,
       ROUND(AVG(l.valeur), 2)         AS moyenne,
       ROUND(MIN(l.valeur), 2)         AS minimum,
       ROUND(MAX(l.valeur), 2)         AS maximum,
       ROUND(100 * STDDEV_SAMP(l.valeur) / AVG(l.valeur), 1) AS cv_pct
FROM v_prix_long l
JOIN variables va ON va.code = l.code
GROUP BY va.role, va.libelle, va.unite
ORDER BY va.role;

-- ---------------------------------------------------------------------
-- 2. Fonctions de fenêtrage : variations mensuelles et moyenne mobile
-- ---------------------------------------------------------------------
-- Vue des variations mensuelles en logarithme (≈ variation en %)
CREATE VIEW v_variations AS
SELECT date,
       LN(cacao)   - LN(LAG(cacao)   OVER (ORDER BY date)) AS d_cacao,
       LN(arabica) - LN(LAG(arabica) OVER (ORDER BY date)) AS d_arabica
FROM prix;

-- (f) La crise du cacao vue en SQL : moyenne mobile sur 12 mois et
--     variation sur un an, un mois par trimestre de 2023 à 2026
WITH indicateurs AS (
  SELECT date, cacao,
         AVG(cacao) OVER (ORDER BY date ROWS BETWEEN 11 PRECEDING AND CURRENT ROW)
                                                         AS moyenne_mobile_12m,
         cacao / LAG(cacao, 12) OVER (ORDER BY date) - 1 AS variation_1an
  FROM prix
)
SELECT date, cacao,
       ROUND(moyenne_mobile_12m, 2)    AS moyenne_mobile_12m,
       ROUND(100 * variation_1an, 0)   AS variation_1an_pct
FROM indicateurs
WHERE date >= '2023-01-01' AND EXTRACT(MONTH FROM date) IN (1, 4, 7, 10)
ORDER BY date;

-- ---------------------------------------------------------------------
-- 3. Statistiques directement en SQL
-- ---------------------------------------------------------------------
-- (g) Corrélations cacao-café : en niveau (log) et en variation mensuelle
--     -> à comparer avec R (0,79 et 0,22)
SELECT ROUND(CORR(LN(cacao), LN(arabica))::numeric, 2) AS corr_niveaux,
       (SELECT ROUND(CORR(d_cacao, d_arabica)::numeric, 2) FROM v_variations)
                                                        AS corr_variations
FROM prix;

-- (h) Volatilité mensuelle et corrélation par période (CASE + GROUP BY)
SELECT CASE WHEN date < '2010-01-01' THEN '1. 2000-2009'
            WHEN date < '2024-01-01' THEN '2. 2010-2023'
            ELSE '3. 2024-2026 (crise)' END             AS periode,
       COUNT(*)                                         AS mois,
       ROUND(100 * STDDEV_SAMP(d_cacao)::numeric, 1)    AS volatilite_cacao_pct,
       ROUND(100 * STDDEV_SAMP(d_arabica)::numeric, 1)  AS volatilite_arabica_pct,
       ROUND(CORR(d_cacao, d_arabica)::numeric, 2)      AS corr_variations
FROM v_variations
WHERE d_cacao IS NOT NULL
GROUP BY periode
ORDER BY periode;

-- (i) Régression « naïve » en niveau : log café = a + b × log cacao
--     -> pente et R² élevés, mais régression potentiellement fallacieuse
--        entre séries I(1) (Granger et Newbold, 1974) : d'où l'ARDL
SELECT ROUND(REGR_SLOPE(LN(arabica), LN(cacao))::numeric, 3)     AS pente,
       ROUND(REGR_INTERCEPT(LN(arabica), LN(cacao))::numeric, 3) AS constante,
       ROUND(REGR_R2(LN(arabica), LN(cacao))::numeric, 3)        AS r2
FROM prix;
