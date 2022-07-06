
pacman::p_load(haven,dplyr,fastDummies,lmtest)

Base_de_datos_clase_6_de_julio <- read_excel("~/DCCS/Fundamentos Biológicos y Culturales del Comportamiento Humano/Trabajo PSAP/tarea/Base de datos clase 6 de julio.xlsx")

Base_de_datos_clase_6_de_julio <- mutate(Base_de_datos_clase_6_de_julio,mujeres = sex_response=='Mujer')

psap<-lm(P_ponderado ~  mujeres + age_response , Base_de_datos_clase_6_de_julio)
summary(psap)

psap2<-lm(P_ponderado ~  mujeres , Base_de_datos_clase_6_de_julio)
summary(psap)
