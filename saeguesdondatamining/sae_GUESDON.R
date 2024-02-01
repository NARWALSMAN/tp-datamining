##################################
# TP Data Mining - Pratique sous R avec Pokémon #
##################################

# INITIALISATION
# Installer et charger les packages nécessaires
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
library(class)
library(MASS)
library(klaR)
library(caret)
library(ggplot2)

source("tp.R")
# Charger les données Pokémon
pokemon_data <- read.csv("Pokemon.csv")


#Préparer les données pour le KNN
set.seed(123) # Pour la reproductibilité

# Utilisation de 'Type 1' pour la classification
indices <- createDataPartition(pokemon_data$Type.1, p = 0.7, list = TRUE)
train_set <- pokemon_data[indices[[1]], c("Total", "HP", "Attack", "Defense", "Sp..Atk", "Sp..Def", "Speed", "Type.1")]
test_set <- pokemon_data[-indices[[1]], c("Total", "HP", "Attack", "Defense", "Sp..Atk", "Sp..Def", "Speed", "Type.1")]

#Fonction pour le test KNN
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

#Tester la fonction sur les données Pokémon
#trouver le k optimal
liste_k_voisin <- c(3, 5, 7)
k_optimale <- trouve_k_opt(train_set, liste_k_voisin)
k_optimale
 
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

# Fonction pour l'étude de rééchantillonnage avec LDA
etude_reechantillonnage_lda <- function(data, variable_predire, variables_independantes, N) {
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
    
    # Appliquer LDA et prédire
    modele_lda <- lda(as.formula(paste(variable_predire, "~", paste(variables_independantes, collapse = "+"))), 
                      data = train_set)
    predictions <- predict(modele_lda, X_test)$class
    
    # Calculer le taux d'erreur
    erreur <- mean(predictions != y_test)
    resultats[[i]] <- erreur
  }
  
  return(resultats)
}


N <- 100  # Nombre de répétitions pour l'étude
# Exécution de l'étude de réée avec LDA
resultats_reechantillonnage_lda <- etude_reechantillonnage_lda(pokemon_data, "Type.1", 
                                                               c("Total", "HP", "Attack", "Defense", "Sp..Atk", "Sp..Def", "Speed"), 
                                                               N)

# Analyser les résultats
taux_erreurs_moyens_lda <- mean(unlist(resultats_reechantillonnage_lda))
ggplot(data.frame(Taux_Erreur = unlist(resultats_reechantillonnage_lda)), aes(x = Taux_Erreur)) +
  geom_histogram(binwidth = 0.01, fill = "blue", color = "black") +
  ggtitle("Distribution des Taux d'Erreur pour LDA") +
  xlab("Taux d'Erreur") +
  ylab("Fréquence")



# Construire l'arbre de décision
arbre_decision <- rpart(Type.1 ~ Total + HP + Attack + Defense + Sp..Atk + Sp..Def + Speed, 
                        data = pokemon_data, 
                        method = "class")

# Visualiser l'arbre de décision
rpart.plot(arbre_decision, main="Arbre de Décision - Pokémon Type 1", extra=102)
