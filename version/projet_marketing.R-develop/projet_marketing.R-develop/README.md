# Projet Marketing — Analyse multivariée en R

Ce projet a pour objectif de **segmenter des clients** à partir de leurs
caractéristiques démographiques et de leurs habitudes d’achat, en appliquant
des méthodes d’**analyse multivariée**.

## Dataset utilisé

- **Nom** : Customer Personality Analysis
- **Source** : Kaggle
- **Nombre d’observations** : 2240
- **Nombre de variables** : 29
- **Thématique** : marketing / comportement client

## Objectifs du projet

- Comprendre la structure d’un jeu de données multidimensionnel
- Réduire la dimensionnalité par **Analyse en Composantes Principales (ACP)**
- Identifier des segments de clients par **classification K-means**
- Interpréter les profils obtenus et proposer des recommandations business

## Méthodologie suivie

1. Compréhension du problème et présentation des données
2. Nettoyage et préparation des données
3. Sélection des variables quantitatives pertinentes
4. Analyse en composantes principales (**ACP**)
5. Classification non supervisée par **K-means**
6. Analyse combinée ACP + segmentation
7. Interprétation statistique et recommandations marketing

## Fichiers du dépôt

- `projet_marketing.R` : script R principal
- `rapport_marketing.Rmd` : rapport R Markdown
- `marketing_campaign.csv` : dataset utilisé
- `.gitignore` : fichiers à ignorer dans Git

## Packages utilisés

```r
install.packages(c(
  "tidyverse", "FactoMineR", "factoextra", "cluster",
  "corrplot", "readr", "gridExtra", "rmarkdown", "knitr"
))
```

## Exécution

Exécuter le script principal :

```r
source("projet_marketing.R")
```

Générer le rapport :

```r
rmarkdown::render("rapport_marketing.Rmd")
```

## Résultats principaux

- Réduction de dimension par ACP
- Identification de **4 segments clients**
- Visualisation des profils dans l’espace factoriel
- Recommandations pour le ciblage commercial

## Cadre pédagogique

Ce projet a été réalisé dans le cadre du module **Méthodes statistiques et étude de données (Analyse multivariée)** du parcours **Ingénieur Data Science & IA**.
