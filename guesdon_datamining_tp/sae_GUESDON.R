##################################
# TP Data Mining - Pratique sous R avec Pokémon #
##################################

# INITIALISATION
library(class)
library(MASS)
library(klaR)
library(caret)

source("tp.R")
# Charger les données Pokémon
pokemon_data <- read.csv("Pokemon.csv")


# QUESTION 2 - Préparer les données pour le KNN
set.seed(123) # Pour la reproductibilité

# Utilisation de 'Type 1' pour la classification
indices <- createDataPartition(pokemon_data$Type.1, p = 0.7, list = TRUE)
train_set <- pokemon_data[indices[[1]], c("Total", "HP", "Attack", "Defense", "Sp..Atk", "Sp..Def", "Speed", "Type.1")]
test_set <- pokemon_data[-indices[[1]], c("Total", "HP", "Attack", "Defense", "Sp..Atk", "Sp..Def", "Speed", "Type.1")]

# QUESTION 3 - Fonction pour le test KNN
fait_un_test <- function(donnertrain, donnertest, classetrain, classetest, k) {
    # Adaptation de la fonction pour les données Pokémon
    test_resultat <- knn(train = donnertrain[, -ncol(donnertrain)], 
                         test = donnertest[, -ncol(donnertest)], 
                         cl = classetrain, 
                         k = k, use.all = FALSE)
    tableau_confusion <- table(classetest, test_resultat)
    precision <- sum(diag(tableau_confusion)) / sum(tableau_confusion)
    return(precision)
}

# QUESTION 4 - Tester la fonction sur les données Pokémon
# Exemple: trouver le k optimal
liste_k_voisin <- c(3, 5, 7)
k_optimale <- trouve_k_opt(train_set, liste_k_voisin)

# QUESTION 5 
etude_reechantillonnage <- function(data, variable_predire, variables_independantes, ks, N) {
  taille <- nrow(data)
  resultats <- vector("list", N)
  
  for (i in 1:N) {
    # Création d'un échantillon aléatoire pour l'apprentissage et le test
    indices <- sample(1:taille, size = floor(0.7 * taille))
    train_set <- data[indices, ]
    test_set <- data[-indices, ]
    
    y_train <- train_set[[variable_predire]]
    y_test <- test_set[[variable_predire]]
    
    X_train <- train_set[variables_independantes]
    X_test <- test_set[variables_independantes]
    
    # Boucle sur les différentes valeurs de k
    erreurs <- numeric(length(ks))
    for (j in seq_along(ks)) {
      k <- ks[j]
      predictions <- knn(train = X_train, test = X_test, cl = y_train, k = k)
      erreurs[j] <- mean(predictions != y_test)
    }
    
    resultats[[i]] <- erreurs
  }
  
  return(resultats)
}

#QUESTION 6
# Paramètres pour l'étude de rééchantillonnage
ks <- c(3, 5, 7, 9, 11, 13)  # différentes valeurs de k à tester
N <- 100  # nombre de répétitions pour l'étude

# Exécution de l'étude de rééchantillonnage
resultats_reechantillonnage <- etude_reechantillonnage(pokemon_data, "Type.1", 
                                                       c("Total", "HP", "Attack", "Defense", "Sp..Atk", "Sp..Def", "Speed"), 
                                                       ks, N)

# Analyser les résultats
taux_erreurs_moyens <- sapply(resultats_reechantillonnage, mean)

# Afficher les résultats
print(taux_erreurs_moyens)
