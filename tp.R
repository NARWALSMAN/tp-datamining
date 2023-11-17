
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
fait_un_test<-function(donnertrain,donnertest,classetrain,classetest,k){
  #creer un test de knn et test avec une matrice de confusion la precision
  #entrée: donnertrain :  dataframe  donner de training du modele
  #        donnertest:    dataframe  donner a tester
  #        classetrain:   liste      mliste des classe du datatrain
  #        classetest:    liste      liste des classe du donnertest
  #        k: integer     k          voisin 
  #sortie: precision:     intezger   renvooi la precision calculer du test
  test_resultat_1<-knn(train = donnertrain, test = donnertest,cl = classetrain,k = k,use.all = FALSE,set.seed(4))
  tableau_confusion_1<-table(classetest,test_resultat_1)
  precision_1<-(tableau_confusion_1[1,1]+tableau_confusion_1[2,2]+tableau_confusion_1[3,3])/
    (tableau_confusion_1[1,2]+tableau_confusion_1[1,3]+tableau_confusion_1[2,3]+
       tableau_confusion_1[2,1]+tableau_confusion_1[3,1]+tableau_confusion_1[3,2]+
       tableau_confusion_1[1,1]+tableau_confusion_1[2,2]+tableau_confusion_1[3,3])
  return(precision_1)
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
    donnertrain<-donner_ech[1:length(donner_ech)/2,1:4]
    classtrain<-donner_ech[1:length(donner_ech)/2,5]
    donnertest<-donner_ech[(length(donner_ech)/2):length(donner_ech),1:4]
    classtest<-donner_ech[(length(donner_ech)/2):length(donner_ech),5]
    prec<-fait_un_test(donnertrain,donnertest,classtrain,classtest,k)
  for (i in 2:10){
    donner_ech<-creer_les_donner(donner)
    donnertrain<-donner_ech[1:length(donner_ech)/2,1:4]
    classtrain<-donner_ech[1:length(donner_ech)/2,5]
    donnertest<-donner_ech[(length(donner_ech)/2):length(donner_ech),1:4]
    classtest<-donner_ech[(length(donner_ech)/2):length(donner_ech),5]
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

#test
liste_k_voisin<-c(3,5,7)
k_optimale<-test_10_k(data_iris,3)
k_optimale
#QUESTION 4  Tester votre fonction sur le jeu entier


#QUESTION 5  Programmer l’etude de reechantillonnage


#QUESTION 6  Tester votre programme sur le jeu entier

