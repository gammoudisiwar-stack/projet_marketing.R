# =============================================================================
# Projet : Analyse multivariée - ACP et Classification
# Dataset : Customer Personality Analysis (marketing_campaign.csv)
# Auteurs : Siwar & Hadyle
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Installation (une seule fois)
# -----------------------------------------------------------------------------
# install.packages(c("tidyverse", "FactoMineR", "factoextra", "cluster",
#                    "corrplot", "readr", "gridExtra"))

# -----------------------------------------------------------------------------
# 1. Chargement des packages et données
# -----------------------------------------------------------------------------
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(cluster)
library(corrplot)
library(readr)
library(gridExtra)

# Charger le fichier de données
# Le fichier marketing_campaign.csv doit être présent dans le répertoire du projet
df <- read_delim("marketing_campaign.csv", delim = "\t")

cat("\n==========================================")
cat("\nPROJET ANALYSE MULTIVARIEE - MARKETING")
cat("\n==========================================\n")

cat("\nDimensions initiales :", dim(df)[1], "x", dim(df)[2], "\n")
cat("\nNoms des colonnes :\n")
print(names(df))

cat("\nValeurs manquantes par variable (avant nettoyage) :\n")
print(colSums(is.na(df)))

# -----------------------------------------------------------------------------
# 2. Nettoyage et préparation
# -----------------------------------------------------------------------------
cat("\n--- NETTOYAGE DES DONNEES ---\n")

annee_courante <- as.integer(format(Sys.Date(), "%Y"))

# Suppression des lignes avec revenu manquant + création de variables dérivées
# puis filtrage des observations incohérentes
df_clean <- df %>%
  filter(!is.na(Income)) %>%
  mutate(
    Age = annee_courante - Year_Birth,
    Enfants = Kidhome + Teenhome,
    Depenses_total = MntWines + MntFruits + MntMeatProducts +
      MntFishProducts + MntSweetProducts + MntGoldProds,
    Achats_total = NumWebPurchases + NumCatalogPurchases + NumStorePurchases,
    Accepte_campagnes = AcceptedCmp1 + AcceptedCmp2 + AcceptedCmp3 +
      AcceptedCmp4 + AcceptedCmp5
  ) %>%
  filter(Age >= 18, Age <= 100, Income > 0)

cat("\nObservations après nettoyage :", nrow(df_clean), "\n")
cat("Valeurs manquantes après nettoyage :\n")
print(colSums(is.na(df_clean)))

cat("\nRésumé de Income après nettoyage :\n")
print(summary(df_clean$Income))

cat("\nRésumé de Age après nettoyage :\n")
print(summary(df_clean$Age))

# Contrôle visuel simple des valeurs extrêmes
par(mfrow = c(1, 2))
boxplot(df_clean$Income, main = "Boxplot du revenu", col = "lightblue")
boxplot(df_clean$Age, main = "Boxplot de l'âge", col = "lightgreen")
par(mfrow = c(1, 1))

# -----------------------------------------------------------------------------
# 3. Sélection des variables pour l'ACP
# -----------------------------------------------------------------------------
cat("\n--- SELECTION DES VARIABLES POUR L'ACP ---\n")

vars_acp <- df_clean %>%
  select(
    Income, Recency, MntWines, MntFruits, MntMeatProducts,
    MntFishProducts, MntSweetProducts, MntGoldProds,
    NumDealsPurchases, NumWebPurchases, NumCatalogPurchases,
    NumStorePurchases, NumWebVisitsMonth, Age
  )

cat("\nVariables retenues pour l'ACP :", ncol(vars_acp), "\n")
print(names(vars_acp))

cat("\nValeurs manquantes dans les variables ACP :", sum(is.na(vars_acp)), "\n")
vars_acp <- na.omit(vars_acp)

# -----------------------------------------------------------------------------
# 4. Analyse en Composantes Principales (ACP)
# -----------------------------------------------------------------------------
cat("\n--- ANALYSE EN COMPOSANTES PRINCIPALES ---\n")
cat("Les variables sont standardisées automatiquement avec scale.unit = TRUE.\n")

res.pca <- PCA(vars_acp, scale.unit = TRUE, graph = FALSE)

eigen_values <- get_eigenvalue(res.pca)
cat("\nVariance expliquée par les 5 premiers axes :\n")
print(round(eigen_values[1:5, ], 3))

# Graphique d'éboulis
fviz_eig(res.pca, addlabels = TRUE,
         main = "Variance expliquée par les axes factoriels")

# Cercle des corrélations
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("blue", "yellow", "red"),
             repel = TRUE,
             title = "Cercle des corrélations")

# Visualisation des individus
fviz_pca_ind(res.pca,
             alpha.ind = 0.4,
             title = "Projection des individus sur le plan factoriel")

# -----------------------------------------------------------------------------
# 5. Classification (K-means)
# -----------------------------------------------------------------------------
cat("\n--- CLASSIFICATION K-MEANS ---\n")

coords_pca <- res.pca$ind$coord[, 1:3]

# Détermination visuelle du nombre de clusters
p1 <- fviz_nbclust(coords_pca, kmeans, method = "wss") +
  ggtitle("Méthode Elbow")

p2 <- fviz_nbclust(coords_pca, kmeans, method = "silhouette") +
  ggtitle("Méthode Silhouette")

grid.arrange(p1, p2, ncol = 2)

cat("\nChoix retenu : k = 4, d'après les méthodes Elbow et Silhouette.\n")

set.seed(123)
kmeans_result <- kmeans(coords_pca, centers = 4, nstart = 25)

cat("\nTaille des clusters :\n")
print(table(kmeans_result$cluster))

fviz_cluster(kmeans_result, data = coords_pca,
             ellipse.type = "convex",
             palette = c("#2E9FDF", "#E7B800", "#FC4E07", "#00AFBB"),
             main = "Segmentation clients (K-means)")

# -----------------------------------------------------------------------------
# 6. Profil des clusters
# -----------------------------------------------------------------------------
cat("\n--- PROFIL DES CLUSTERS ---\n")

df_clean$Cluster <- kmeans_result$cluster

profil <- df_clean %>%
  group_by(Cluster) %>%
  summarise(
    Revenu = mean(Income),
    Age = mean(Age),
    Depenses = mean(Depenses_total),
    Achats_web = mean(NumWebPurchases),
    Visites_web = mean(NumWebVisitsMonth),
    Effectif = n(),
    .groups = "drop"
  )

cat("\nProfil moyen par cluster :\n")
print(round(profil, 0))

profil_detail <- df_clean %>%
  group_by(Cluster) %>%
  summarise(
    MntWines = mean(MntWines),
    MntMeatProducts = mean(MntMeatProducts),
    Achats_magasin = mean(NumStorePurchases),
    Promotions = mean(NumDealsPurchases),
    .groups = "drop"
  )

cat("\nDétail des habitudes d'achat par cluster :\n")
print(round(profil_detail, 0))

# -----------------------------------------------------------------------------
# 7. Analyse combinée ACP + classification
# -----------------------------------------------------------------------------
cat("\n--- ANALYSE COMBINEE ACP + CLASSIFICATION ---\n")

fviz_pca_ind(res.pca,
             habillage = as.factor(kmeans_result$cluster),
             palette = c("#2E9FDF", "#E7B800", "#FC4E07", "#00AFBB"),
             addEllipses = TRUE,
             ellipse.level = 0.95,
             title = "Segments clients dans l'espace factoriel")

# -----------------------------------------------------------------------------
# 8. Conclusion
# -----------------------------------------------------------------------------
cat("\n==========================================")
cat("\nCONCLUSIONS")
cat("\n==========================================\n")
cat("- L'ACP permet de résumer l'information contenue dans 14 variables quantitatives.\n")
cat("- La classification K-means identifie 4 segments de clients distincts.\n")
cat("- Les profils obtenus peuvent être mobilisés pour cibler les campagnes marketing.\n")
cat("- Les résultats doivent être interprétés avec prudence en l'absence de dimension temporelle.\n")
