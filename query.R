#Principales factores de riesgo: Presion arterial alta(HTA), colesterol(DLP), diabetes(DBT), obesidad(OBES),
#tabaquismo(TBQ), genero(Genero) y herencia(AHF), edad mayor a 65(Edad>65), 

library(plotrix)
library(ggridges)
library(ggplot2)
library(treemap)
library(dplyr)
library(babynames)
library(maps)
library(geosphere)
library(plotly)
library(gapminder)
library(gganimate)

colnames(dataset)[2] <- "Genero"

# Question 1 : Rango de edad de personas con eventos cardiacos fumadores
pacientsWithEvents = subset(dataset, Evento == 1)
pacientsSmokersEvent = subset(pacientsWithEvents, TBQ == 1)
pacientsQ1 = subset(pacientsWithEvents, TBQ == 1)
table1 = table(pacientsSmokersEvent$Edad)

ggplot(pacientsQ1, aes(x = Edad)) + 
  geom_histogram(binwidth = 3, fill = "lightblue", colour = "black", alpha = 0.4) +
  ggtitle("Smoking Patients")+
  theme_ridges()

#Question 2 :  Rango de edad de diabeticos, alteracion de lipidos y obesos con eventos cardiacos
pacientsQ2 = subset(pacientsWithEvents, OBES == 1 & DLP == 1 & DBT == 1)
pacientsQ2 = subset(pacientsQ2, select = c(Edad, Genero))
pacientsQ2$Genero[pacientsQ2$Genero == "1"] = "Hombre"
pacientsQ2$Genero[pacientsQ2$Genero == "0"] = "Mujer"

ggplot(pacientsQ2, aes(x = Edad, fill = Genero)) +
  geom_density(alpha = 0.4) +
  scale_fill_discrete(name = "Gender", labels = c("Female","Male")) +
  theme(legend.position = "top") +
  theme_ridges() 

#Question 3 : Frecuencia de edad de hipertencion en gente con eventos cardiacos
pacientsQ3 = subset(pacientsWithEvents, HTA == 1)

#Question 4 : Relacion genero con eventos cardiacos
pacientsQ4 = subset(pacientsWithEvents, select = c(Edad, Genero))
pacientsQ4$Genero[pacientsQ4$Genero == "1"] = "Hombre"
pacientsQ4$Genero[pacientsQ4$Genero == "0"] = "Mujer"

ggplot(pacientsQ4, aes(x = Edad, fill = Genero)) +
  geom_density(alpha = 0.4) +
  scale_fill_discrete(name = "Gender", labels = c("Female","Male")) +
  theme(legend.position = "top") +
  theme_ridges() + 
  xlab("Age") +
  ylab("Density") +
  ggtitle("Range of ages from patients with DBT, DLP and OBES")

#Question 5 : Cantidad de mujeres y varones con eventos (Data Study)
patientsQ5 = subset(pacientsWithEvents, select = (Genero))
patientsQ5$Genero[patientsQ5$Genero == "1"] = "Hombre"
patientsQ5$Genero[patientsQ5$Genero == "0"] = "Mujer"
slices = c(sum(patientsQ5$Genero == "Hombre"), sum(patientsQ5$Genero == "Mujer"))
lbls = c("Men", "Women")
pct = round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")

pie3D(slices,labels=lbls ,main="Patients with events", col=c("#66ccff", "#ff99cc"), explode = 0.1, theta = 1, mar = c(5,5,5,5))


#Question 8 : Cantidad pacientes con eventos >65años y <65años:
patientsQ8 = subset(pacientsWithEvents, select = (Edad))
more65 <- round(sum(patientsQ8$Edad>65)/length(patientsQ8$Edad)*100)
less65 <- round(sum(patientsQ8$Edad<=50)/length(patientsQ8$Edad)*100)
pie3D(c(more65,less65),main = "Patients with events age´s", labels = c(paste(more65,"%"),paste(less65,"%")),col = c("lightgreen","#ffff99"),labelcex = 1, explode = 0.1, theta = 1, mar = c(5,5,5,5))
legend(.3,1,c("Age >65","Age <=65"),fill = c("lightgreen","#ffff99"))

#Question 6 : Rango de edades de personas con un evento cardiaco, fumadores y que presente hipertension arterial
pacientsQ6 = subset(pacientsWithEvents, TBQ == 1 & HTA == 1)
tableQ6 = table(pacientsQ6$Edad)

#Question 7 : colesterol(DLP), diabetes(DBT), obesidad(OBES) y relacion entre ellos
patientsWithEventsDLP = subset(pacientsWithEvents, DLP == 1)
patientsWithEventsDBT = subset(pacientsWithEvents, DBT == 1)
patientsWithEventsOBES = subset(pacientsWithEvents, OBES == 1)
patientsWithEventsDDO = subset(subset(patientsWithEventsDBT, DLP == 1), OBES == 1)
y = c(nrow(patientsWithEventsDLP), nrow(patientsWithEventsDBT), nrow(patientsWithEventsOBES), nrow(patientsWithEventsDDO))
x = c("DLP", "DBT", "OBES", "Confluence")
data = data.frame(x,y)

ggplot(data, aes(x=x, y=y)) + 
  geom_bar(stat = "identity", width=0.7,fill="#6699ff",colour="black", alpha=.6) +
  coord_flip() +
  xlab("") +
  ylab("Density") +
  ggtitle("Relation between DLP, DBT and OBES") + 
  theme_ridges()
  

