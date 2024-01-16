## TP 1 ARBRES DE DECISIONS PROBLEME III ##


# Question 3 : Tracer la courbe LIFT

# Définir les données
se_values <- c(0, 1, 0.647, 0.883, 1)     # RPP
rpp_values <- c(0, 1, 0.2345, 0.31, 0.494)  # Se

# Créer un data frame avec les valeurs RPP et Se
data <- data.frame(RPP = rpp_values, Se = se_values)

# Créer le graphique avec une courbe d'interpolation polynomiale
lift_plot <- ggplot(data, aes(x = RPP, y = Se)) +
  geom_point() +  # ajoute les points
  geom_smooth(method = 'lm', formula = y ~ poly(x,3 ), se = FALSE) +  # ajoute une courbe polynomiale
  xlim(0, 1) +  # limite de l'axe des x
  ylim(0, 1) +  # limite de l'axe des y
  xlab("RPP (Rate of Positive Predictions)") +  # étiquette de l'axe des x
  ylab("Se (Sensibility)") +  # étiquette de l'axe des y
  ggtitle("Courbe LIFT") +  # titre du graphique
  theme_minimal()  # thème minimal pour le graphique

# Afficher la courbe LIFT
print(lift_plot)

# Question 4 : Ajouter la bissectrice à la courbe LIFT

lift_plot <- ggplot(data, aes(x = RPP, y = Se)) +
  geom_point() +  # ajoute les points
  geom_smooth(method = 'lm', formula = y ~ poly(x, 3), se = FALSE) +  # ajoute une courbe polynomiale
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +  # ajoute la première bissectrice
  xlim(0, 1) +  # limite de l'axe des x
  ylim(0, 1) +  # limite de l'axe des y
  xlab("RPP (Rate of Positive Predictions)") +  # étiquette de l'axe des x
  ylab("Se (Sensibility)") +  # étiquette de l'axe des y
  ggtitle("Courbe LIFT") +  # titre du graphique
  theme_minimal()  # thème minimal pour le graphique

# Afficher la courbe LIFT
print(lift_plot)

# Question 5 :

# Définir les données
se_values <- c(0, 1, 0.647, 0.883, 1)     # Se
rpp_values <- c(0, 1, 0.2345, 0.31, 0.494)  # RPP

# Créer un data frame avec les valeurs RPP et Se
data <- data.frame(RPP = rpp_values, Se = se_values)

# Ajuster un modèle de régression polynomiale
model <- lm(Se ~ poly(RPP, 2), data = data)

# Utiliser le modèle pour prédire la valeur de Se pour RPP = 0.1 (10%)
predicted_se <- predict(model, newdata = data.frame(RPP = 0.1))

# Créer le graphique avec les annotations
lift_plot <- ggplot(data, aes(x = RPP, y = Se)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ poly(x, 2), se = FALSE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  geom_point(aes(x = 0.1, y = predicted_se), color = "red", size = 3) +
  annotate("text", x = 0.1, y = predicted_se, label = paste("Lift à 10%:", round(predicted_se, 2)), vjust = -1) +
  xlim(0, 1) +
  ylim(0, 1) +
  xlab("RPP (Rate of Positive Predictions)") +
  ylab("Se (Sensibility)") +
  ggtitle("Courbe LIFT") +
  theme_minimal()

# Afficher la courbe LIFT
print(lift_plot)

# Question 7 : Normalisation de la courbe LIFT

# Calculer Se normalisé par RPP
normalized_se <- se_values / rpp_values
normalized_se[is.infinite(normalized_se)] <- NA  # Remplacer les infinis par NA pour éviter les erreurs graphiques

# Créer un dataframe pour ggplot
data <- data.frame(RPP = rpp_values, NormalizedSe = normalized_se)

# Tracer la courbe LIFT normalisée
ggplot(data, aes(x = RPP, y = NormalizedSe)) +
  geom_point() +  # Ajouter les points
  geom_line() +  # Ajouter une ligne pour relier les points
  geom_hline(yintercept = 1, linetype = "dashed") +  # Ajouter une ligne horizontale en pointillés à y = 1
  xlim(0, 1) +  # Limite de l'axe des abscisses
  ylim(0, max(normalized_se, na.rm = TRUE)) +  # Limite de l'axe des ordonnées
  xlab("RPP (Rate of Positive Predictions)") +  # Étiquette de l'axe des abscisses
  ylab("Normalized Se (Sensitivity)") +  # Étiquette de l'axe des ordonnées
  ggtitle("Courbe LIFT normalisée") +  # Titre du graphique
  theme_minimal()  # Utiliser un thème minimaliste pour le graphique



ggplot(data, aes(x = RPP, y = NormalizedSe)) +
  geom_point() +  # Ajouter les points
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "blue") +  # Ajouter une courbe d'interpolation polynomiale d'ordre 3
  geom_hline(yintercept = 1, linetype = "dashed") +  # Ajouter une ligne horizontale en pointillés à y = 1
  xlim(0, 1) +  # Limite de l'axe des abscisses
  ylim(0, max(normalized_se, na.rm = TRUE)) +  # Limite de l'axe des ordonnées
  xlab("RPP (Rate of Positive Predictions)") +  # Étiquette de l'axe des abscisses
  ylab("Normalized Se (Sensitivity)") +  # Étiquette de l'axe des ordonnées
  ggtitle("Courbe LIFT normalisée avec interpolation polynomiale") +  # Titre du graphique
  theme_minimal()  # Utiliser un thème minimaliste pour le graphique

# Question 9 

# Pour répondre à la question 9a, on suppose qu'il faut contacter une proportion égale de la clientèle pour toucher une proportion égale de bons clients.
# Cela signifie que si l'on veut toucher 80% des bons clients, on doit contacter 80% de la clientèle.

# Pour la question 9b, on va utiliser la courbe LIFT pour être plus efficace.

# Interpolation linéaire pour trouver Se (Sensibilité) pour plusieurs valeurs de RPP
rpp_range <- seq(0, 1, by = 0.01)  # Création d'une séquence de RPP de 0 à 1
se_interpolated <- approx(rpp_values, se_values, xout = rpp_range)$y

# Calcul de LIFT pour chaque RPP dans le rpp_range
lift_values <- se_interpolated / rpp_range
lift_values[is.infinite(lift_values)] <- NA  # Remplacer les infinis par NA pour éviter les erreurs graphiques



# Créer un dataframe pour le LIFT à différentes valeurs de RPP
lift_data <- data.frame(RPP = rpp_range, Lift = lift_values)

# Trouver le RPP le plus bas pour lequel le Lift est suffisant pour toucher 80% des bons clients
# On veut Lift * RPP >= 0.80
required_lift <- 0.80 / rpp_range
closest_rpp_index <- which.min(abs(lift_values - required_lift))
closest_rpp <- rpp_range[closest_rpp_index]

# Répondre à la question 9b
print(paste("Pour toucher 80% des bons clients en utilisant la courbe LIFT, le vendeur devrait contacter environ", round(closest_rpp * 100, 2), "% de sa clientèle."))

# Tracé de la courbe LIFT avec le point trouvé
ggplot(lift_data, aes(x = RPP, y = Lift)) +
  geom_line() +  # Ajouter une ligne pour relier les points
  geom_point(aes(x = closest_rpp, y = lift_values[closest_rpp_index]), color = "red") +
  geom_text(aes(x = closest_rpp, y = lift_values[closest_rpp_index], label = paste("RPP pour 80% Se: ", round(closest_rpp * 100, 2), "%")), 
            hjust = 1.2, vjust = 1.2, color = "red") +  # Étiquette du point
  xlim(0, 1) + ylim(0, max(lift_values, na.rm = TRUE)) +
  xlab("RPP (Rate of Positive Predictions)") +
  ylab("LIFT") +
  ggtitle("Courbe LIFT avec le point pour atteindre 80% des bons clients") +
  theme_minimal()


