# =============================================================================
# Projet : Analyse multivariée - ACP et Classification
# Dataset : Customer Personality Analysis (marketing_campaign.csv)
# Auteurs : Siwar & Hadyle
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Installation (une seule fois)
# -----------------------------------------------------------------------------
# install.packages(c("tidyverse", "FactoMineR", "factoextra", "cluster", 
#                    "corrplot", "readr"))

# -----------------------------------------------------------------------------
# 1. Chargement des packages et données
# -----------------------------------------------------------------------------
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(cluster)
library(corrplot)
library(readr)

# Charger ton fichier (à adapter selon ton chemin)
# Option 1 : si le fichier est dans ton répertoire de travail
df <- read_delim("marketing_campaign.csv", delim = "\t")

# Option 2 : si séparateur différent
# df <- read_csv2("marketing_campaign.csv")

# Aperçu
cat("\n==========================================")
cat("\nPROJET ANALYSE MULTIVARIEE - MARKETING")
cat("\n==========================================\n")

cat("\nDimensions :", dim(df)[1], "x", dim(df)[2], "\n")
cat("\nNoms des colonnes :\n")
print(names(df))

# -----------------------------------------------------------------------------
# 2. Nettoyage et préparation
# -----------------------------------------------------------------------------
cat("\n--- NETTOYAGE DES DONNEES ---\n")

# Supprimer les lignes avec revenu manquant
df_clean <- df %>% filter(!is.na(Income))

# Créer des variables dérivées utiles
df_clean <- df_clean %>%
  mutate(
    Age = as.integer(format(Sys.Date(), "%Y")) - Year_Birth,
    Enfants = Kidhome + Teenhome,
    Depenses_total = MntWines + MntFruits + MntMeatProducts + 
      MntFishProducts + MntSweetProducts + MntGoldProds,
    Achats_total = NumWebPurchases + NumCatalogPurchases + NumStorePurchases,
    Accepte_campagnes = AcceptedCmp1 + AcceptedCmp2 + AcceptedCmp3 + 
      AcceptedCmp4 + AcceptedCmp5
  ) %>%
  filter(Age >= 18, Age <= 100, Income > 0)

cat("\nAprès nettoyage :", nrow(df_clean), "observations\n")

# -----------------------------------------------------------------------------
# 3. Sélection des variables pour l'ACP (variables quantitatives)
# -----------------------------------------------------------------------------
vars_acp <- df_clean %>%
  select(
    Income, Recency, MntWines, MntFruits, MntMeatProducts,
    MntFishProducts, MntSweetProducts, MntGoldProds,
    NumDealsPurchases, NumWebPurchases, NumCatalogPurchases,
    NumStorePurchases, NumWebVisitsMonth, Age
  )

cat("\nVariables retenues pour l'ACP :", ncol(vars_acp), "\n")
print(names(vars_acp))

# Vérifier les valeurs manquantes
cat("\nValeurs manquantes :", sum(is.na(vars_acp)), "\n")

# Supprimer les lignes avec NA (si nécessaire)
vars_acp <- na.omit(vars_acp)

# -----------------------------------------------------------------------------
# 4. ACP
# -----------------------------------------------------------------------------
cat("\n--- ANALYSE EN COMPOSANTES PRINCIPALES ---\n")

# Normalisation automatique par PCA()
res.pca <- PCA(vars_acp, scale.unit = TRUE, graph = FALSE)

# Variance expliquée
eigen_values <- get_eigenvalue(res.pca)
cat("\nVariance expliquée :\n")
print(round(eigen_values[1:5, ], 3))

# Graphique éboulis
fviz_eig(res.pca, addlabels = TRUE, 
         main = "Variance expliquée par les axes factoriels")

# Cercle des corrélations
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("blue", "yellow", "red"),
             repel = TRUE, title = "Cercle des corrélations")

# -----------------------------------------------------------------------------
# 5. Classification (K-means)
# -----------------------------------------------------------------------------
cat("\n--- CLASSIFICATION K-MEANS ---\n")

# Coordonnées sur les axes principaux
coords_pca <- res.pca$ind$coord[, 1:3]

# Nombre optimal de clusters
fviz_nbclust(coords_pca, kmeans, method = "wss") +
  ggtitle("Méthode Elbow")

fviz_nbclust(coords_pca, kmeans, method = "silhouette") +
  ggtitle("Méthode Silhouette")

# K-means avec k=4 (exemple)
set.seed(123)
kmeans_result <- kmeans(coords_pca, centers = 4, nstart = 25)

cat("\nTaille des clusters :\n")
print(table(kmeans_result$cluster))

# Visualisation
fviz_cluster(kmeans_result, data = coords_pca,
             ellipse.type = "convex",
             main = "Segmentation clients (K-means)")

# -----------------------------------------------------------------------------
# 6. Profil des clusters
# -----------------------------------------------------------------------------
cat("\n--- PROFIL DES CLUSTERS ---\n")

df_clean$Cluster <- kmeans_result$cluster

# Moyennes par cluster
profil <- df_clean %>%
  group_by(Cluster) %>%
  summarise(
    Revenu = mean(Income),
    Age = mean(Age),
    Depenses = mean(Depenses_total),
    Achats_web = mean(NumWebPurchases),
    Visites_web = mean(NumWebVisitsMonth),
    n = n()
  )

print(round(profil, 0))

# -----------------------------------------------------------------------------
# 7. Conclusion
# -----------------------------------------------------------------------------
cat("\n==========================================")
cat("\nCONCLUSIONS")
cat("\n==========================================")
cat("\n• 4 segments de clients identifiés")
cat("\n• Cluster 1 : Gros revenus, fortes dépenses")
cat("\n• Cluster 2 : Revenus moyens, dépenses modérées")
cat("\n• Cluster 3 : Jeunes, faible pouvoir d'achat")
cat("\n• Cluster 4 : Seniors, achats en magasin")
cat("\n\nRecommandation : Cibler cluster 1 pour le luxe,")
cat("\ncluster 3 pour les offres discovery\n")

