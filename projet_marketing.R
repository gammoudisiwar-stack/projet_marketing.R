# =============================================================================
# Projet : Prédiction des ventes en fonction des dépenses publicitaires
# Dataset : marketing (package datarium)
# Auteur  : Siwar
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Installation (à exécuter une seule fois si les packages ne sont pas installés)
# -----------------------------------------------------------------------------
# install.packages(c("tidyverse", "datarium", "lmtest", "car", "ggplot2", "GGally"))

# -----------------------------------------------------------------------------
# 1. Importation des packages et exploration
# -----------------------------------------------------------------------------
library(tidyverse)
library(datarium)
library(lmtest)
library(car)
library(GGally)

data("marketing")

cat("\n--- Aperçu des données ---\n")
print(head(marketing))

cat("\n--- Dimensions ---\n")
print(dim(marketing))

cat("\n--- Résumé statistique ---\n")
print(summary(marketing))

cat("\n--- Valeurs manquantes ---\n")
print(colSums(is.na(marketing)))

# Visualisations exploratoires
ggpairs(marketing,
        title = "Matrice de dispersion - marketing")

par(mfrow = c(1, 3))
plot(marketing$youtube,   marketing$sales, main = "Sales vs YouTube",
     xlab = "YouTube",   ylab = "Sales", pch = 19, col = "steelblue")
plot(marketing$facebook,  marketing$sales, main = "Sales vs Facebook",
     xlab = "Facebook",  ylab = "Sales", pch = 19, col = "tomato")
plot(marketing$newspaper, marketing$sales, main = "Sales vs Newspaper",
     xlab = "Newspaper", ylab = "Sales", pch = 19, col = "darkgreen")
par(mfrow = c(1, 1))

# Matrice de corrélations
cat("\n--- Corrélations ---\n")
print(round(cor(marketing), 3))

# -----------------------------------------------------------------------------
# 2. Régression linéaire multiple
# -----------------------------------------------------------------------------
reg1 <- lm(sales ~ youtube + facebook + newspaper, data = marketing)

cat("\n--- Résumé du modèle complet ---\n")
print(summary(reg1))

# -----------------------------------------------------------------------------
# 3. Test de significativité globale (F-test)
# -----------------------------------------------------------------------------
# H0 : beta_youtube = beta_facebook = beta_newspaper = 0
# H1 : au moins un coefficient est non nul
cat("\n--- Test de Fisher (significativité globale) ---\n")
f_stat <- summary(reg1)$fstatistic
cat("F =", round(f_stat[1], 3),
    "  ddl1 =", f_stat[2], "  ddl2 =", f_stat[3], "\n")
p_global <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
cat("p-value globale =", format.pval(p_global), "\n")

# -----------------------------------------------------------------------------
# 4. Analyse des coefficients (t-tests individuels)
# -----------------------------------------------------------------------------
cat("\n--- Coefficients et intervalles de confiance ---\n")
print(round(coef(summary(reg1)), 4))
print(confint(reg1))

# Modèle réduit sans newspaper (souvent non significatif)
reg2 <- lm(sales ~ youtube + facebook, data = marketing)
cat("\n--- Modèle réduit (sans newspaper) ---\n")
print(summary(reg2))

# Comparaison des modèles
cat("\n--- ANOVA : reg1 vs reg2 ---\n")
print(anova(reg2, reg1))

# -----------------------------------------------------------------------------
# 5. Vérification des hypothèses MCO
# -----------------------------------------------------------------------------
par(mfrow = c(2, 2))
plot(reg2)
par(mfrow = c(1, 1))

# Normalité des résidus
cat("\n--- Test de Shapiro-Wilk (normalité des résidus) ---\n")
print(shapiro.test(residuals(reg2)))

# Homoscédasticité
cat("\n--- Test de Breusch-Pagan (homoscédasticité) ---\n")
print(bptest(reg2))

# Indépendance des résidus
cat("\n--- Test de Durbin-Watson (autocorrélation) ---\n")
print(dwtest(reg2))

# Multicolinéarité
cat("\n--- VIF (multicolinéarité) ---\n")
print(vif(reg2))

# -----------------------------------------------------------------------------
# 6. Évaluation et comparaison des modèles
# -----------------------------------------------------------------------------
cat("\n--- Comparaison R^2, R^2 ajusté, AIC, BIC ---\n")
comparaison <- data.frame(
  Modele     = c("reg1 (3 variables)", "reg2 (sans newspaper)"),
  R2         = c(summary(reg1)$r.squared,     summary(reg2)$r.squared),
  R2_ajuste  = c(summary(reg1)$adj.r.squared, summary(reg2)$adj.r.squared),
  AIC        = c(AIC(reg1), AIC(reg2)),
  BIC        = c(BIC(reg1), BIC(reg2))
)
print(comparaison)

# -----------------------------------------------------------------------------
# 7. Prédiction (exemple business)
# -----------------------------------------------------------------------------
nouveaux_budgets <- data.frame(
  youtube  = c(100, 200, 300),
  facebook = c( 20,  30,  40)
)
pred <- predict(reg2, newdata = nouveaux_budgets,
                interval = "confidence", level = 0.95)
cat("\n--- Prédictions pour de nouveaux budgets ---\n")
print(cbind(nouveaux_budgets, round(pred, 2)))

# -----------------------------------------------------------------------------
# 8. Interprétation business (résumé textuel)
# -----------------------------------------------------------------------------
cat("\n=============================================================\n")
cat(" CONCLUSIONS BUSINESS\n")
cat("=============================================================\n")
cat("- YouTube et Facebook ont un effet positif et significatif sur\n",
    "  les ventes ; Newspaper n'est pas significatif.\n",
    "- Pour 1 000 $ supplémentaires investis :\n",
    "    * YouTube  -> +", round(coef(reg2)["youtube"]*1000, 2), " unites de ventes\n",
    "    * Facebook -> +", round(coef(reg2)["facebook"]*1000, 2), " unites de ventes\n",
    "- Recommandation : reallouer le budget Newspaper vers Facebook\n",
    "  (ROI marginal le plus eleve) puis YouTube.\n",
    "- Limites : effets d'interaction non modelises, donnees limitees\n",
    "  a 200 observations, pas de dimension temporelle.\n", sep = "")
