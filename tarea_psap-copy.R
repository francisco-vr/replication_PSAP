
pacman::p_load(haven,dplyr,fastDummies,lmtest, readxl, MASS, ggplot2, wesanderson, ggpubr, stargazer)

BBDD_PSAP <- readxl::read_excel("Base de datos clase 6 de julio.xlsx")

BBDD_PSAP<- mutate(BBDD_PSAP,mujeres = sex_response=='Mujer')

psap<-rlm(P_ponderado ~  mujeres + age_response , BBDD_PSAP)
summary(psap)

psap2<-rlm(P_ponderado ~  mujeres , BBDD_PSAP)
summary(psap)

stargazer::stargazer(psap,psap2,
                     title = "Regresión robusta de porcentaje PSAP por sexo",
                     type = "html",
                     dep.var.labels = "Porcentaje",
                     covariate.labels = c("Mujeres", "Edad", "Constante"),
                     star.cutoffs = c(0.05, 0.01, 0.001),
                     notes = "Categoría Base Mujeres: Hombres",
                     notes.label = "Niveles de significancia",
                     out = "regresion1.html")

#Plots from type OF ACTION

test2 <-read_excel("bbdd.xlsx")

compE2 <-list(c("Hombre","Mujer"))

type<-test2%>%
  ggplot(data = test2, mapping = aes(x = kind, y = as.numeric(Rate), fill = sex)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot() +
  geom_hline(yintercept = mean(as.numeric(test2$Rate)), linetype = 2) +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=7, vjust = -2) +
  labs(title = "Porcentaje de Agresividad",
       x = "Sexo", y = "Porcentraje",
       caption = "NS = No Statistical significance; * ≤.05; ** ≤.01; *** ≤.001") +
  theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE2, label = "p.signif")

#General

general<-BBDD_PSAP%>%
  ggplot(data = BBDD_PSAP, mapping = aes(x = sex_response, y = as.numeric(P_ponderado), fill = sex_response)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot() +
  geom_hline(yintercept = mean(as.numeric(BBDD_PSAP$P_ponderado)), linetype = 2) +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=7, vjust = -2) +
  labs(title = "Porcentaje de Agresividad",
       x = "Sexo", y = "Porcentraje",
       caption = "NS = No Statistical significance; * ≤.05; ** ≤.01; *** ≤.001") +
  theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE2, label = "p.signif")

ggsave(general, filename = "plot1.png",
       dpi = 250, width = 8, height = 6)











#
