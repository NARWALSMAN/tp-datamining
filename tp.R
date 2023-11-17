
##################################
#TP Data Mining - Pratique sous R#
##################################
# INITIALISATION
library(class)
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
data_train<-rbind(data_train_setosa,data_train_versicolor,data_train_virginica)
label_train<-c(rep("setosa",25),rep("versicolor",25),rep("virginica",25))
#test
data_test_versicolor <- subset(data_iris, Species == "versicolor")[26:50, ]
data_test_virginica<-subset(data_iris, Species == "virginica")[26:50, ]
data_test_setosa<-subset(data_iris, Species == "setosa")[26:50, ]
data_test<-rbind(data_test_setosa,data_test_versicolor,data_test_virginica)
label_test<-c(rep("setosa",25),rep("versicolor",25),rep("virginica",25))

test_resultat<-knn(train = data_train[,1:4], test = data_test[,1:4],cl = label_train,k = 3,use.all = FALSE)
test_resultat

table(test_resultat,data_test[,5])
#QUESTION 3  Programmer une fonction 


#QUESTION 4  Tester votre fonction sur le jeu entier


#QUESTION 5  Programmer l’etude de reechantillonnage


#QUESTION 6  Tester votre programme sur le jeu entier
