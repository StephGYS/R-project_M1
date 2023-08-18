###################################### projet ######################################################
######## Les bÃ©nÃ©ficiaires de la prime d'excellence scientifique Idex de 1993 Ã  2012 ##################
####### ProblÃ©matique:Le nombre de beneficiaires du programme idex est-il influencé par le secteur-disciplinaire et le sexe des chercheurs ########################
#-------------------------------PrÃ©-taitement----------------------------------------------
source(file="C:/Users/gnago/Documents/tp R l3/ressources/outils_stat_desc.R")
proj <- read.csv("C:/Users/gnago/Documents/tp R l3/projett/fr-esr-pes-pedr-beneficiaires.csv", encoding="UTF-8", sep=";")
View(proj)
str(proj)
summary(proj)
library("DescTools") 
proj<-proj[,(-19),drop=FALSE]
proj<-proj[,(-16),drop=FALSE]
proj<-proj[,(-14),drop=FALSE]
proj<-proj[,(-12),drop=FALSE]
proj<-proj[,(-2),drop=FALSE]
proj<-proj[,(-3),drop=FALSE]
proj<-proj[,(-4),drop=FALSE]
proj<-proj[,(-5),drop=FALSE]
proj<-proj[,(-6),drop=FALSE]

proj$distance_univ <- character(nrow(proj))
proj$distance_univ <- ifelse(proj$pres == 'Hors PRES', 'Hors PRES', 'PRES')
proj<-proj[,(-8),drop=FALSE]

proj$ideX <- character(nrow(proj))
proj$ideX <- ifelse(proj$idex == 'Hors IDEX', 'Hors IDEX', 'IDEX')
proj<-proj[,(-8),drop=FALSE]
id<-1:nrow(proj)
proj<-cbind(id,(proj))
View(proj)

str(proj)
summary(proj)
PlotMiss(proj, main = "DonnÃ©es manquantes")

#------------------------Etude univariÃ©-------------------------------------
############### annee ################### 
#L'annÃ©e d'obtention de la prime.
str(proj$annee)
summary(proj$annee)
Desc(proj$annee)
psych::describe(proj$annee)
#l'annÃ©e avec le plus de bÃ©nÃ©ficiares est 1994 Ã  UniversitÃ© Paris 6.
subset(proj, subset = (beneficiaires == max(proj$beneficiaires)))
  #boite moustaches
par(mar = c(1.1, 4.1, 3.1, 1.1))
boxplot(x = proj$annee,main = 'Distribution des années', 
        ylab = 'Année', outcol = 2, outpch = 4) 
points(x = 1, y = mean( proj$annee, na.rm = TRUE),
       col = 4, pch = 3, lwd = 2)
############### sexe ###############
#Le sexe des bÃ©nÃ©ficaires.
str(proj$sexe)
summary(proj$sexe)
summary(factor(proj$sexe))
Desc(proj$sexe)
    #Il y'a 62.9% hommes et 37.1% femmes.
proj_femme<-subset(proj, subset = (sexe == "Femmes"))
subset(proj_femme, subset = (beneficiaires == max(proj_femme$beneficiaires)))
# Les annÃ©es avec le plus de bÃ©nÃ©ficiares femmes sont 1994 et 2009
#diagramme circulaire.
eff_sexe <- table(proj$sexe) 
my_pie(freq = eff_sexe, cex = 0.7)
  #Il y'a 62.9% hommes et 37.1% femmes.
############### seteur_disciplinaire ###############
   #c'est secteur de section CNU(Conseil national des universitÃ©s).
str(proj$secteur_disciplinaire)
summary((proj$secteur_disciplinaire))
summary(factor(proj$secteur_disciplinaire))
x11()
Desc(proj$secteur_disciplinaire)
   #Les candidats qui participent le plus sont en sciences, 
   #aprÃ©s c'est ceux de Lettres et sciences humaines.
eff_secteur <- table(proj$secteur_disciplinaire) 
#diagramme en barre
my_barplot(eff_secteur,cex=0.7)
############### groupe de corps ###############
   #la profession des participants,il y'a:Professeurs et assimilÃ©s.MaÃ®tres de confÃ©rences et assimilÃ©s.particuliers.
str(proj$groupe_de_corps)
summary((proj$groupe_de_corps))
summary(factor(proj$groupe_de_corps))
Desc(proj$groupe_de_corps)
   #il y'a 63.8% Professeurs et assimilÃ©s.
   #il y'a 36.0% MaÃ®tres de confÃ©rences et assimilÃ©s.
   #0.2% de particuliers.
eff_group <- table(proj$groupe_de_corps) 
   #diagramme en bande
   #par(mar = c(2.1, 2.1, 1.1, 1.1), cex = 0.5)
my_barplot(freq=eff_group,stack = TRUE,cex=0.7)
################ region ###############
   #la rÃ©gion des participants.
str(proj$region)
summary((proj$region))
summary(factor(proj$region))
Desc(proj$region)
   #La rÃ©gion la plus rÃ©presentÃ© est l'Ile de de france, puis Rhone-alpes.
eff_region <- table(proj$region) 
   #diagramme en barres horizentale
my_barplot(eff_region, pareto = TRUE,cex=0.7)
############### beneficiare ###############
   #le nombre de beneficiaires par universite.
str(proj$beneficiaires)
summary((proj$beneficiaires))
Desc(proj$beneficiaires)
   #l'annÃ©e avec le plus de bÃ©nÃ©ficiares est 1994 avec 148 bÃ©nÃ©ficiares de l'UniversitÃ© Paris 6.
subset(proj, subset = (beneficiaires == max(proj$beneficiaires)))
psych::describe(proj$beneficiaires)
############### distance_univ ###############
  #LibellÃ© du pÃ´le de recherche et dâenseignement supÃ©rieur.
  #pres: pole de recherche est dans l'etablissement.
  #hors pres: pole de recherche est en dehors l'etablissement.
str(proj$distance_univ)
summary((proj$distance_univ))
summary(factor(proj$distance_univ))
Desc(proj$distance_univ)
  #il y'a 84.7% pres.
  #il y'a 15.3% hors pres.
eff_dist <- table(proj$distance_univ)     
my_pie(eff_dist,cex=0.7)
#### idex ####
  #lâinitiative dâexcellence est un programmes d'investissement de 
  #l'Ãtat franÃ§ais dont le but est de crÃ©er en France des ensembles
  #pluridisciplinaires d'enseignement supÃ©rieur et de recherche qui 
  #soient de rang mondial. 
str(proj$ideX)
summary((proj$ideX))
summary(factor(proj$ideX))
Desc(proj$ideX)
  #il y'a 75.4% Hors IDEX.
  #il y'a 24.6% IDEX.
eff_idex <- table(proj$ideX)     
my_pie(eff_idex)

------------------------------Etude bivariÃ©-------------------------------------
############# Idex et secteur_disciplinaire___2 var qualitatives
eff_idex_secteur <- table(proj$ideX, proj$secteur_disciplinaire)
eff_idex_secteur
## Profils lignes 
profl <- prop.table(eff_idex_secteur, margin = 1)
t(apply(X = profl*100, MARGIN = 1, FUN = round_perc, digits = 1))
## Profils colonnes
profc <- prop.table(eff_idex_secteur, margin = 2)
t(apply(X = profc*100, MARGIN = 1, FUN = round_perc, digits = 1))
## ReprÃ©sentation des profils lignes et colonnes
#x11()
my_barplot(freq = eff_idex_secteur, margin = 1,cex=0.7) # Profils lignes
my_barplot(freq = eff_idex_secteur, margin = 2,cex=0.7) # Profils colonnes
# InterprÃ©tation
# Il ne semble pas Ã©merger de tendance de dÃ©pendance entre idex et le secteur_discipline.
# H1=indÃ©pendance entre les variables (p_value> seuil).
# H0=dÃ©pendance entre les variables (p_value< seuil).
## Test du khi-deux
chisq.test(eff_idex_secteur)
# InterprÃ©tation :
# p-value = 6.495e-06 < 0.05
# Au niveau de significativitÃ© de 5 %, on dÃ©cÃ¨le une relation 
#de dÃ©pendance significative entre les variables idex et le secteur_disciplinaires.
res.test <-chisq.test((eff_idex_secteur))
res.test$expected  #Le test chi2 est bien applicables (effectifs espÃ©rÃ©s < 5).
res.test$residuals
#Interpretation: 
##Visualisation des rÃ©sidus associÃ©s aux donnÃ©es
par(mar = c(2.1, 2.1, 1.1, 3.1), cex = 0.7)
#x11()
mosaicplot(ideX ~ secteur_disciplinaire, data = proj,
           color = TRUE, shade = TRUE, type = "pearson",
           main = "",
           xlab = "Statut hors idex/idex",
           ylab = "secteur_disciplinaire") #Profils lignes
#Interpretation
#les principales contributions Ã  lâinertie sont dÃ»es aux couples (IDEX, Sciences)
#et (IDEX,SantÃ©),(IDEX,Droit et sciences Ã©conomiques) respectivement 
#sous-reprÃ©sentÃ© et sur-reprÃ©sentÃ© par rapport Ã  la situation thÃ©orique de dÃ©pendance.

############# ***** Idex et sexe___2 var qualitatives
eff_idex_sexe <- table(proj$ideX, proj$sexe,dnn=list("idex","sexe"))
eff_idex_sexe
## Profils lignes 
profl <- prop.table(eff_idex_sexe, margin = 1)
t(apply(X = profl*100, MARGIN = 1, FUN = round_perc, digits = 1))
## Profils colonnes
profc <- prop.table(eff_idex_sexe, margin = 2)
t(apply(X = profc*100, MARGIN = 1, FUN = round_perc, digits = 1))
## ReprÃ©sentation des profils lignes et colonnes
#x11()
my_barplot(freq = eff_idex_sexe, margin = 1,cex=0.7) # Profils lignes
my_barplot(freq = eff_idex_sexe, margin = 2,cex=0.7) # Profils colonnes
# InterprÃ©tation
# Il ne semble pas Ã©merger de tendance de dÃ©pendance entre idex et le sexe.
# H1=indÃ©pendance entre les variables (p_value> seuil).
# H0=dÃ©pendance entre les variables (p_value< seuil).
## Test du khi-deux
chisq.test(eff_idex_sexe)
# InterprÃ©tation :
# p-value =  0.000002302 < 0.05
# Au niveau de significativitÃ© de 5 %, on dÃ©cÃ¨le une relation 
#de dÃ©pendance significative entre les variables idex et le sexe.
res.test <-chisq.test((eff_idex_sexe))
res.test$expected  #Le test chi2 est bien applicables (effectifs espÃ©rÃ©s < 5).
res.test$residuals
#Interpretation: 
##Visualisation des rÃ©sidus associÃ©s aux donnÃ©es
library(ggmosaic)
cl2<-as.data.frame(eff_idex_sexe)
cl2
ggplot(data = cl2) +
  geom_mosaic(aes(weight= Freq, x = product(idex),      fill=sexe), na.rm=TRUE) 
###
par(mar = c(2.1, 2.1, 1.1, 3.1), cex = 0.7)
#x11()
mosaicplot(ideX ~ sexe, data = proj,
           color = TRUE, shade = TRUE, type = "pearson",
           main = "",
           xlab = "Statut hors idex/idex",
           ylab = "sexe") #Profils lignes
#Interpretation
#les principales contributions Ã  lâinertie sont dÃ»es aux couples (IDEX, Homme) et (IDEx , Femme)
#respectivement sur-reprÃ©sentÃ© et sous-reprÃ©sentÃ© par rapport Ã  la situation thÃ©orique de dÃ©pendance.

############## Analyse de la problematique:Le nbr_idex est-il impacté par le secteur-disciplinaire et le sex?
library("dplyr")
require(dplyr)
proj %>%
  filter(ideX == "IDEX")%>%
  select(c(ideX,sexe,secteur_disciplinaire)) -> tt
View(tt)
table(tt$sexe,tt$secteur_disciplinaire)
names(eff) <- c('f_droit', 'h_droit','f_lsh','h_lsh','f_sante','h_sante','f_sciences','h_sciences','f_staps','h_staps')
my_pie(eff) #repartition des beneficiaires d'IDEX selon le sexe dans chaque secteur

     #problematique: Le nbr_idex est-il impacté par le secteur-disciplinaire et le sex
#anova factorielle 
library(afex)
proj$secteur_disciplinaire<- as.factor(proj$secteur_disciplinaire)
proj$sexe<- as.factor(proj$sexe)
aov.out<-aov_4(beneficiaires~secteur_disciplinaire*sexe+(1|id ), data=proj, type= "3")
summary(aov.out)
#les effets d'interactions
#En regardant les effets et pvalue du secteur_disciplinaire et sexe, on constate que les effets d'interactions sont significatives
#cela signifie que les moyennes entre les groupes ne sont équivalentes
#le secteur_disciplinaire et le sexe influence fortement l'accompagnement avec Idex
   #Graphiques de style interaction pour les moyennes marginales estimées
library(emmeans)
emmip(aov.out, secteur_disciplinaire~sexe, CI=T)
#On observe que les beneficiaires du programmes IDEX sont en moyenne plus representé chez les HOMMES.
#C'est le secteur des sciences qui beneficient de plus d'accompagnement ensuite il y'a les lettres et sciences humaines.
  