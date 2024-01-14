library(tidyverse)
library(modelr)

# QUE GENERO VA A SER (MEJOR PAGO, MAS TENDENCIOSO) EN EL FUTURO (2020/2030)

data = read_csv('/home/laura/Escritorio/Introduccion a Cs. de Datos/PRESENTACION 2DO PARCIAL/movies.csv')

data <- mutate(data, decada = case_when(
  year %in% c(1980:1989) ~ "80's",
  year %in% c(1990:1999) ~ "90's",
  year %in% c(2000:2009) ~ "00's",
  year %in% c(2010:2020) ~ "10's",
))

data <- mutate(data, votos_cantidad = case_when(votes %in% c(0: 27000) ~ "No populares",
                                                votes %in% c(27000:92100) ~ "Populares",
                                                votes %in% c(92100:2500000) ~ "Muy Populares"))

data <- mutate(data, puntuacion = case_when(score %in% c(0:4) ~ "Malos",
                                                score %in% c(4:7) ~ "Medio",
                                                score %in% c(7:10) ~ "Buenos"))

data <- mutate(data, duracion = case_when(runtime %in% c(0:80) ~ "Corto",
                                            runtime %in% c(80:180) ~ "Medio",
                                            runtime %in% c(180:280) ~ "Largo"))



# LIMPIEZA DE DATOS
data <- data %>%
  filter(budget != is.na(budget),
         gross != is.na(gross),
         votes != is.na(votes),
         score != is.na(score),
         year != is.na(year))

# Agrupado x Genero ----  
sum_genero <- data %>%
  group_by(genre) %>%
  summarise(cantidad = n(),
            ganancia = gross,
            presupuesto = budget,
            año = year,
            ganancia_real = (ganancia - presupuesto),
            pelicula = name,
            puntuacion = puntuacion,
            decada = decada,
            votos = votes,
            compania = company, 
            popularidad = votos_cantidad, 
            pais = country,
            director = director,
            rating = rating,
            duracion = duracion,
            runtime = runtime,
            score = score,
            sum_votos = sum(votes)) %>%
  filter(cantidad > 100, genre == 'Adventure' | genre == 'Action' | genre == 'Animation')


# GRAFICOS NORMALES X GENERO ----
sum_genero %>%
  ggplot(aes(x = año)) +
  geom_vline(xintercept = seq(1980, 2020, by = 10), linetype = 'dashed', color = 'red', size = 0.5, alpha = 0.5) +
  #geom_smooth(aes(color = genre)) +
  geom_density(adjust = 0.5, aes(fill = genre, alpha = 0.5)) +
  #geom_jitter(aes(color = genre)) +
  facet_wrap(vars(genre))

sum_genero %>%
  ggplot(aes(x = ganancia_real, y = presupuesto, fill = genre)) +
  #geom_jitter() +
  geom_smooth(aes(color = genre)) +
  #geom_jitter(aes(color = genre)) +
  facet_wrap(vars(genre))


# NUEVO ANALISIS DE MODELADO REEENTREGA ----

# MODELADOS

mod_cuad_pres1 <- lm(ganancia_real ~ poly(presupuesto, 2, raw = TRUE) * genre, data = sum_genero)
summary(mod_cuad_pres1)

mod_cuad_pres2 <- lm(ganancia_real ~ poly(presupuesto, 2, raw = TRUE) * genre * año, data = sum_genero)
summary(mod_cuad_pres2)

mod_lineal_pres1 <- lm(ganancia_real ~ poly(presupuesto, raw = TRUE) * genre, data = sum_genero)
summary(mod_lineal_pres1)

mod_lineal_pres2 <- lm(ganancia_real ~ poly(presupuesto, raw = TRUE) * genre * año, data = sum_genero)
summary(mod_lineal_pres2)

anova(mod_lineal_pres1, mod_cuad_pres1, mod_lineal_pres2, mod_cuad_pres2)


# PREDD
sum_genero = sum_genero %>%
  add_predictions(model = mod_cuad_pres2)

sum_genero = sum_genero %>%
  add_residuals(model = mod_cuad_pres2)


# Generar valores de presupuesto para predecir
pres_values <- seq(min(sum_genero$presupuesto), max(sum_genero$presupuesto), length.out = 100)

# Predecir ganancias utilizando el modelo
predictions <- predict(mod_cuad_pres2, newdata = data.frame(presupuesto = pres_values))

# Graficar los puntos de datos observados
plot(sum_genero$presupuesto, sum_genero$ganancia_real, pch = 16, col = "blue", xlab = "Presupuesto", ylab = "Ganancia Real")

# Graficar la línea de regresión ajustada
lines(pres_values, predictions, col = "red", lwd = 2)



# GRAF
mod_lineal_pres2%>%
  ggplot(aes(x = presupuesto , y = ganancia_real, fill = genre)) +
  geom_jitter(data = mod_lineal_pres2, aes(color = genre), alpha = 0.5) +
  geom_line(data = sum_genero, aes()) +
  #geom_jitter(aes(color = genre)) +
  #coord_cartesian(ylim = c(-200000000, 700000000)) +
  facet_wrap(vars(genre))

sum_genero %>%
  add_predictions(mod_cuad_pres2) %>%
  add_residuals((mod_cuad_pres2)) %>%
  ggplot(aes(x = presupuesto, y = ganancia_real, fill = genre)) +
  geom_line(aes(x = presupuesto, y = pred, color = genre)) +
  geom_jitter(aes(color = genre)) +
  facet_wrap(vars(genre))

sum_genero %>%
  add_predictions(mod_cuad_pres2) %>%
  add_residuals(mod_cuad_pres2) %>%
  ggplot(mapping=aes(x = presupuesto, y = ganancia_real, fill = genre)) +
  geom_point(data = mod_cuad_pres2) +
  geom_line(data = sum_genero, aes(y = pred, group = genre)) +
  facet_wrap(vars(genre))


sum_genero%>%
  ggplot(aes(x = , y = ganancia_real, fill = genre)) +
  geom_jitter(aes(color = genre), alpha = 0.5) +
  geom_line(aes(y=pred),color='red') +
  #geom_jitter(aes(color = genre)) +
  #coord_cartesian(ylim = c(-200000000, 700000000)) +
  facet_wrap(vars(genre))


# GRAFICOS CON MODELADO PRED ----


lin_sum_genero%>%
  ggplot(aes(x = año, y = ganancia_real, fill = genre)) +
  geom_jitter(aes(color = genre), alpha = 0.5) +
  geom_abline(color='red') +
  #geom_jitter(aes(color = genre)) +
  coord_cartesian(ylim = c(-200000000, 700000000)) +
  facet_wrap(vars(genre))

cuad_sum_genero %>%
  ggplot(aes(x = año, y = ganancia_real, fill = genre)) +
  geom_jitter(aes(color = genre), alpha = 0.5) +
  geom_line(aes(y=pred), color='red') +
  #geom_jitter(aes(color = genre)) +
  coord_cartesian(ylim = c(-200000000, 700000000)) +
  facet_wrap(vars(genre))

