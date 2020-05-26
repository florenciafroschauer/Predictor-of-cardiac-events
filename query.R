#Principales factores de riesgo: Presion arterial alta(HTA), colesterol(DLP), diabetes(DBT), obesidad(OBES),
#tabaquismo(TBQ), genero(Genero) y herencia(AHF), edad mayor a 65(Edad>65), 

# Question 1 : Rango de edad de personas con eventos cardiacos fumadores
pacientsWithEvents = subset(dataset, Evento == 1)
pacientsSmokersEvent = subset(pacientsWithEvents, TBQ == 1)
pacientsQ1 = subset(pacientsWithEvents, TBQ == 1)
table1 = table(pacientsSmokersEvent$Edad)
plot(table1, xlab = "Age", ylab = "Frequency")

ggplot(pacientsQ1, aes(x = Edad)) + 
  geom_histogram(binwidth = 3, fill = "lightblue", colour = "black", alpha = 0.4) +
  ggtitle("Smoking Patients")

#Question 2 :  Rango de edad de diabeticos, alteracion de lipidos y obesos con eventos cardiacos
pacientsQ2 = subset(pacientsWithEvents, OBES == 1 & DLP == 1 & DBT == 1)

#Question 3 : Frecuencia de edad de hipertencion en gente con eventos cardiacos
pacientsQ3 = subset(pacientsWithEvents, HTA == 1)

#Question 4 : Relacion genero con eventos cardiacos
pacientsQ4 = subset(pacientsWithEvents, select = c(Edad, Género))
pacientsQ4$Género[pacientsQ4$Género == "1"] = "Hombre"
pacientsQ4$Género[pacientsQ4$Género == "0"] = "Mujer"

ggplot(pacientsQ4, aes(x = Edad, fill = Género)) +
  geom_density(alpha = 0.4) +
  scale_fill_discrete(name = "Gender", labels = c("Female","Male")) +
  theme(legend.position = "top") +
  theme_ridges()

#Question 5 :  Relacion entre caracteristicas del dolor, mismo dolor previo y cantidad de episodios de eventos cardiacos

#Question 6 : Rango de edades de personas con un evento cardiaco, fumadores y que presente hipertension arterial
pacientsQ6 = subset(pacientsWithEvents, TBQ == 1 & HTA == 1)
tableQ6 = table(pacientsQ6$Edad)

#Question 7 : colesterol(DLP), diabetes(DBT), obesidad(OBES) y relacion entre ellos
patientsWithEventsDLP = subset(pacientsWithEvents, DLP == 1)
patientsWithEventsDBT = subset(pacientsWithEvents, DBT == 1)
patientsWithEventsOBES = subset(pacientsWithEvents, OBES == 1)
patientsWithEventsDDO = subset(subset(patientsWithEventsDBT, DLP == 1), OBES == 1)
y = c(nrow(patientsWithEventsDLP), nrow(patientsWithEventsDBT), nrow(patientsWithEventsOBES), nrow(patientsWithEventsDDO))
x = c("Colesterol", "Diabetes", "Obesidad", "Mezcla")
data = data.frame(x,y)

ggplot(data, aes(x=x, y=y)) + 
  geom_bar(stat = "identity", width=0.7,fill="#6699ff", alpha=.6) +
  coord_flip() +
  xlab("Patient quantity") +
  ylab("Resting blood preassure") +
  ggtitle("RBP mean by CPT") + 
  theme_ridges()
  

