# Projet Marketing — Régression linéaire multiple en R

Prédire les **ventes** d'un produit à partir des dépenses publicitaires
(`youtube`, `facebook`, `newspaper`) — dataset `marketing` du package
**datarium**.

## Fichiers

- [projet_marketing.R](projet_marketing.R) — script R complet, exécutable de bout en bout.
- [rapport_marketing.Rmd](rapport_marketing.Rmd) — rapport R Markdown (à knit en HTML/PDF).

## Prérequis

```r
install.packages(c("tidyverse", "datarium", "lmtest", "car", "GGally"))
# Pour le rapport :
install.packages(c("rmarkdown", "knitr"))
```

## Exécution

- Script :
  ```r
  source("projet_marketing.R")
  ```
- Rapport :
  ```r
  rmarkdown::render("rapport_marketing.Rmd")
  ```

## Plan suivi

1. Importation et exploration (`summary`, corrélations, `ggpairs`)
2. Régression multiple `lm(sales ~ youtube + facebook + newspaper)`
3. Test de significativité globale (F-test)
4. Analyse des coefficients (t-tests, IC, modèle réduit)
5. Vérification des hypothèses MCO
   (normalité Shapiro, homoscédasticité Breusch-Pagan, autocorrélation
   Durbin-Watson, multicolinéarité VIF)
6. Évaluation : R², R² ajusté, AIC, BIC, ANOVA
7. Prédiction et interprétation business
