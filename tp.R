
##################################
#TP Data Mining - Pratique sous R#
##################################
# INITIALISATION
library(class)
library(MASS)
library(klaR)
library(caret)
data_iris<-iris

#QUESTION 1  Decrire comment fonctionne cette procedure

#fonction qui vas chercher par la methode des knn les objet 
#en fonction d'un echantillions d'entrainement
# en entrée
#     train : dataframe  
#     le dataset des echantillions prit en donnée pour entrainer le modele
#     test : dataframe
#     le dataset des objet dont on cherche la classe
#     cl :  liste
#     la liste des classes
#     k :  integer
#     le chois du nombre de k voisin
# en sortie
#     liste
#     une liste des valeurs de classe trouver de chaque objet
#

#QUESTION 2  Tester cette procedure avec echantillon
#train
data_train_versicolor <- subset(data_iris, Species == "versicolor")[1:25, ]
data_train_virginica<-subset(data_iris, Species == "virginica")[1:25, ]
data_train_setosa<-subset(data_iris, Species == "setosa")[1:25, ]
data_train<-rbind(data_train_setosa,data_train_versicolor,data_train_virginica)[,1:4]
label_train<-c(rep("setosa",25),rep("versicolor",25),rep("virginica",25))
#test
data_test_versicolor <- subset(data_iris, Species == "versicolor")[26:50, ]
data_test_virginica<-subset(data_iris, Species == "virginica")[26:50, ]
data_test_setosa<-subset(data_iris, Species == "setosa")[26:50, ]
data_test<-rbind(data_test_setosa,data_test_versicolor,data_test_virginica)[,1:4]
label_test<-c(rep("setosa",25),rep("versicolor",25),rep("virginica",25))
#test des knn
test_resultat<-knn(train = data_train, test = data_test,cl = label_train,k = 3,use.all = FALSE,set.seed(4))
test_resultat
#calcule du tableau de confusion
tableau_confusion<-table(label_test,test_resultat)
#calcule du taux d'erreur
tauxerreur<-(tableau_confusion[1,2]+tableau_confusion[1,3]+tableau_confusion[2,3]+
  tableau_confusion[2,1]+tableau_confusion[3,1]+tableau_confusion[3,2])/75
#calcule de la precision
precision<-(tableau_confusion[1,1]+tableau_confusion[2,2]+tableau_confusion[3,3])/
  (tableau_confusion[1,2]+tableau_confusion[1,3]+tableau_confusion[2,3]+
     tableau_confusion[2,1]+tableau_confusion[3,1]+tableau_confusion[3,2]+
     tableau_confusion[1,1]+tableau_confusion[2,2]+tableau_confusion[3,3])
#resultat
tauxerreur
precision
#QUESTION 3  Programmer une fonction 
fait_un_test <- function(donnertrain, donnertest, classetrain, classetest, k) {
  # Crée un test de knn et teste avec une matrice de confusion la précision
  test_resultat <- knn(train = donnertrain, test = donnertest, cl = classetrain, k = k, use.all = FALSE)
  tableau_confusion <- table(classetest, test_resultat)
  
  # Calcul de la précision
  precision <- sum(diag(tableau_confusion)) / sum(tableau_confusion)
  return(precision)
}


creer_les_donner<-function(data){
  #a partir d'un tableau de donner creer un echantillions aleatoire 
  #entree: data   dataframe   donner a echantillionner
  #sortie: data_ech dataframe donner echantillionner (data)
  
  #creer les valeur aleatoire
  x<-sample(15:25, 1)
  y<-sample(15:25, 1)
  z<-sample(15:25, 1)
  x<-2*x
  y<-2*y
  z<-2*z
  data_versicolor <- subset(data, Species == "versicolor")[1:x, ]
  data_virginica<-subset(data, Species == "virginica")[1:y, ]
  data_setosa<-subset(data, Species == "setosa")[1:z, ]
  data_ech<-rbind(data_train_setosa,data_train_versicolor,data_train_virginica)[,1:5]

  return(data_ech)
}
test_10_k<-function(donner,k){
  #fonction qui fait le test des knn 10 foix
  #entree: donner   dataframe   donnerbrut
  #        k        integer     kvoisin
  #sortie: la precision trouver
    donner_ech<-creer_les_donner(donner)
    donner_ech<-donner_ech[sample(nrow(donner_ech)), ]
    donnertrain<-donner_ech[1:nrow(donner_ech)/2,1:4]
    classtrain<-donner_ech[1:nrow(donner_ech)/2,5]
    donnertest<-donner_ech[(nrow(donner_ech)/2):nrow(donner_ech),1:4]
    classtest<-donner_ech[(nrow(donner_ech)/2):nrow(donner_ech),5]
    prec<-fait_un_test(donnertrain,donnertest,classtrain,classtest,k)
  for (i in 2:10){
    donner_ech<-creer_les_donner(donner)
    donner_ech<-donner_ech[sample(nrow(donner_ech)), ]
    donnertrain<-donner_ech[1:nrow(donner_ech)/2,1:4]
    classtrain<-donner_ech[1:nrow(donner_ech)/2,5]
    donnertest<-donner_ech[(nrow(donner_ech)/2):nrow(donner_ech),1:4]
    classtest<-donner_ech[(nrow(donner_ech)/2):nrow(donner_ech),5]
    newprec<-fait_un_test(donnertrain,donnertest,classtrain,classtest,k)
    if(newprec>=prec){prec<-newprec}
  }
    return(prec)
}

trouve_k_opt<-function(data,listek){
  #trouve le k optimal par des test de knn
  #entrer: data   dataframe donner
  #        listek liste     liste des k a tester
  k_opt<-listek[1]
  precision<-test_10_k(data,k_opt)
  for (i in 2:length(listek)){
    new_precision<-test_10_k(data,listek[i])
    if (new_precision>=precision){
      k_opt<-listek[i]
      precision<-new_precision
    }
  }
  return(k_opt)
}


#QUESTION 4  Tester votre fonction sur le jeu entier
liste_k_voisin<-c(3,5,7)
k_optimale<-trouve_k_opt(data_iris,liste_k_voisin)
k_optimale

#QUESTION 5  Programmer l’etude de reechantillonnage

etude_reechantillonnage <- function(y, X, ks, N) {
  n <- length(y)
  taille_apprentissage <- floor(0.7 * n)
  
  set.seed(123) # Pour rendre les résultats reproductibles
  indices_apprentissage <- matrix(nrow = N, ncol = taille_apprentissage)
  
  # Stocker les indices des échantillons d'apprentissage
  for (i in 1:N) {
    indices_apprentissage[i,] <- sample(1:n, taille_apprentissage)
  }
  
  resultats <- list()
  
  # Boucle sur les N répétitions
  for (i in 1:N) {
    indices_test <- setdiff(1:n, indices_apprentissage[i,])
    
    X_apprentissage <- X[indices_apprentissage[i,],]
    y_apprentissage <- y[indices_apprentissage[i,]]
    
    X_test <- X[indices_test,]
    y_test <- y[indices_test]
    
    erreurs <- numeric(length(ks))
    precisions <- numeric(length(ks))
    
    for (j in 1:length(ks)) {
      k <- ks[j]
      predictions <- knn(X_apprentissage, X_test, y_apprentissage, k = k)
      mat_confusion <- table(y_test, predictions)
      erreurs[j] <- 1 - sum(diag(mat_confusion)) / sum(mat_confusion)
      precisions[j] <- sum(diag(mat_confusion)) / sum(mat_confusion)
    }
    
    k_opt <- ks[which.min(erreurs)]
    erreur_opt <- min(erreurs)
    precision_opt <- max(precisions)
    
    resultats[[i]] <- list(k_optimal = k_opt, erreur = erreur_opt, precision = precision_opt)
  }
  
  return(resultats)
}


#QUESTION 6  Tester votre programme sur le jeu entier

# Mise à jour des valeurs de k et de N
ks <- c(3, 5, 7, 9, 11, 13)  # Grille des valeurs de k à tester
N <- 500                     # Nombre de répétitions pour l'étude de rééchantillonnage
data(iris)
X <- as.matrix(iris[, 1:4])  # Convertir les prédicteurs en matrice
y <- iris[, 5]              # La réponse est la colonne des espèces
# Exécution de l'étude de rééchantillonnage
resultats <- etude_reechantillonnage(y, X, ks, N)

# Extraction des taux d'erreurs et des précisions à partir des résultats
error_rates <- sapply(resultats, function(x) x$erreur)  # Taux d'erreur pour chaque répétition
precisions <- sapply(resultats, function(x) x$precision)  # Précision pour chaque répétition
k_optimal <- sapply(resultats, function(x) x$k_optimal)  # Valeur optimale de k pour chaque répétition

# Configuration de l'espace de tracé pour afficher deux graphiques côte à côte
par(mfrow = c(1, 2))

# Tracé des boxplots pour les taux d'erreurs et les précisions
boxplot(error_rates, main = "Taux d'Erreurs", ylab = "Taux d'Erreur")
boxplot(precisions, main = "Précisions", ylab = "Précision")

par(mfrow = c(1, 1))
# Tracé de l'histogramme de la distribution des valeurs optimales de k
hist(k_optimal, main = "Distribution des Valeurs Optimales de k", xlab = "k", breaks = length(unique(k_optimal)))

# Analyser les résultats et conclure sur l'utilisation de la méthode k-NN




#####
#LDA#
#####

#QUESTION 1 D´ecrire `a quoi correspondent les entrée et sorties
ind.train <- c(1:25,51:75,101:125)
attach(iris)
res <- lda(formula=Species ~ ., data = iris, prior = c(1,1,1)/3,
           subset = ind.train)
respredict <- predict(res, iris[-ind.train, ])

#formula  = spécifie le model utiliser
#data     = tableau de donner ici les donner iris
#prior    = les probabilité de chaque classe ici chaque espèce a la meeme proba 1/3
respredict$class 
#donne les classes par ordre de proximité 
respredict$posterior
# donne la valeurs pour la proximité

#QUESTION 2 Construire la matrice de confusion associee aux donnees “test”


ind.train <- c(1:25, 51:75, 101:125)
res.lda <- lda(Species ~ ., data = iris, subset = ind.train)
predictions <- predict(res.lda, iris[-ind.train, ])

# Créer la matrice de confusion
confusion_matrix <- table(predictions$class, iris[-ind.train, "Species"])

# Calculer le taux d'erreur
error_rate <- 1 - sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Calculer la précision pour chaque espèce
precision_setosa <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
precision_virginica <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
precision_versicolor <- confusion_matrix[3, 3] / sum(confusion_matrix[3, ])


list(confusion_matrix = confusion_matrix, 
     error_rate = error_rate, 
     precision_setosa = precision_setosa,
     precision_virginica = precision_virginica,
     precision_versicolor = precision_versicolor)

#QUESTION 3
# Sélection des variables avec greedy.wilks
disc.forward <- greedy.wilks(Species ~ ., data = iris, niveau = 0.01)

# Exécuter LDA avec les variables sélectionnées
res.lda <- lda(disc.forward$formula, data = iris)

# Résultats de la sélection de variables
disc.forward$results
disc.forward$results[,1]

#QUESTION 4
selected_vars <- c("Sepal.Length", "Sepal.Width")
formula <- as.formula(paste("Species ~", paste(selected_vars, collapse = "+")))

# Exécuter l'analyse LDA avec la sélection de variables
res.lda <- lda(formula = formula, data = iris, prior = c(1, 1, 1) / 3, subset = ind.train)
respredict <- predict(res.lda, iris[-ind.train, ])
respredict

#QUESTION 5
# Fonction pour effectuer l'étude de rééchantillonnage avec LDA
effectuer_reechantillonnage_lda <- function(data, n_splits) {
  resultats <- list()  # Pour stocker les résultats
  
  for (i in 1:n_splits) {
    # Diviser les données
    set.seed(i)  # Assurer la reproductibilité
    indices <- createDataPartition(data$Species, p = 0.7, list = FALSE)
    formation <- data[indices, ]
    test <- data[-indices, ]
    
    # Appliquer LDA
    modele_lda <- lda(Species ~ ., data = formation)
    predictions <- predict(modele_lda, test)$class
    
    # Calculer le taux d'erreur et la précision
    matrice_confusion <- table(predictions, test$Species)
    taux_erreur <- 1 - sum(diag(matrice_confusion)) / sum(matrice_confusion)
    precision <- diag(matrice_confusion) / rowSums(matrice_confusion)
    
    # Stocker les résultats
    resultats[[i]] <- list(taux_erreur = taux_erreur, precision = precision)
  }
  
  return(resultats)
}
#QUESTION 6
# Exécuter l'étude de rééchantillonnage
resultats_reechantillonnage_lda <- effectuer_reechantillonnage_lda(data_iris, n_splits = 500)
resultats_reechantillonnage_lda
# Extraction des taux d'erreurs à partir des résultats
taux_erreurs <- sapply(resultats_reechantillonnage_lda, function(x) x$taux_erreur)

# Extraction des précisions pour chaque classe (setosa, virginica, versicolor)
# en supposant que l'ordre des classes est le même pour toutes les précisions
precisions <- do.call(rbind, lapply(resultats_reechantillonnage_lda, function(x) x$precision))
precisions_setosa <- precisions[, "setosa"]
precisions_virginica <- precisions[, "virginica"]
precisions_versicolor <- precisions[, "versicolor"]

# Configuration de l'espace de tracé pour afficher trois graphiques côte à côte
par(mfrow = c(1, 4))

# Tracé du boxplot pour les taux d'erreurs
boxplot(taux_erreurs, main = "Taux d'Erreurs", ylab = "Taux d'Erreur")

# Tracé des boxplots pour les précisions de chaque classe
boxplot(precisions_setosa, main = "Précision Setosa", ylab = "Précision")
boxplot(precisions_virginica, main = "Précision Virginica", ylab = "Précision")
boxplot(precisions_versicolor, main = "Précision Versicolor", ylab = "Précision")

# Restaurer la configuration par défaut de l'espace de tracé
par(mfrow = c(1, 1))

#QUESTION 7
selection_results <- resultats_reechantillonnage_lda
# Créer une matrice pour stocker les résultats
var_names <- unique(unlist(selection_results))  # Toutes les variables uniques
selection_matrix <- matrix(0, nrow = length(var_names), ncol = length(var_names))
rownames(selection_matrix) <- var_names

# Remplir la matrice avec les occurrences
for (selection in selection_results) {
  for (i in seq_along(selection)) {
    variable <- selection[i]
    selection_matrix[variable, i] <- selection_matrix[variable, i] + 1
  }
}

# Créer le tableau de contingence
contingency_table <- as.table(selection_matrix)

# Afficher le tableau3
print(contingency_table)

#ARBRE QUESTION 1
# Load necessary libraries
library(rpart)
library(rpart.plot)

# Prepare the data
data(iris)
X <- iris[, 1:4]   # Selecting the features
y <- iris[, 5]     # Selecting the target variable

# Splitting the data into training and test sets
# First 25 observations of each species for training
ind.train <- c(1:25, 51:75, 101:125)
X.train <- X[ind.train, ]
y.train <- y[ind.train]

# Create a data frame for training
dat.train <- data.frame(cbind(X.train, y.train = y.train))

# Building the decision tree
arbre <- rpart(y.train ~ ., data = dat.train)

# Print the tree structure
print(arbre)

# Visualizing the tree
rpart.plot(arbre)
