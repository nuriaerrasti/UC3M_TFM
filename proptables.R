library(readxl) # para abrir los excels
library(dplyr) # para usar el %>%
library(purrr)
library(tidyr)
library(clipr)
library(data.table)
library(ggplot2) # para los graficos
library(GGally) # para el ggpairs
library(caret) # para el createdatapartition
library(VIM) # para la funcion aggr


datos <- read_excel("data/datos.xlsx")

# set seed for reproducibility
set.seed(123)

# pasar a factor las variables categoricas
datos$Test<- as.factor(datos$Test)
datos$Sex<- as.factor(datos$Sex)
datos$ICU_stay<- as.factor(datos$ICU_stay)
datos$Pain_level<- as.factor(datos$Pain_level)
datos$Blood_pressure<- as.factor(datos$Blood_pressure)
datos$Capillary_sat<- as.factor(datos$Capillary_sat)
datos$Sat_type<- as.factor(datos$Sat_type)
datos$death <- as.factor(datos$death)

nas <- sapply(datos, function(x) 100*sum(is.na(x))/1642) # Calcular el % de NAs por columna
tabla_nas <- data.frame(Variable = names(nas), NAs = nas)
variables_a_eliminar <- tabla_nas$Variable[(tabla_nas$NAs > 30)]
datos <- datos[, !names(datos) %in% variables_a_eliminar]

nonsurvivors <- subset(datos, death == "Yes")
survivors <- subset(datos, death == "No")


calculate_proportions_and_p_value <- function(var) {
  table_nonsurvivors <- table(nonsurvivors[[var]])
  table_survivors <- table(survivors[[var]])
  total_nonsurvivors <- sum(table_nonsurvivors)
  total_survivors <- sum(table_survivors)
  
  proportions_nonsurvivors <- prop.table(table_nonsurvivors)
  proportions_survivors <- prop.table(table_survivors)
  
  # Combinar tablas para prueba de chi-cuadrado
  combined_table <- rbind(table_nonsurvivors, table_survivors)
  chi_square_test <- chisq.test(combined_table)
  
  list(
    total_nonsurvivors = table_nonsurvivors,
    total_survivors = table_survivors,
    proportions_nonsurvivors = proportions_nonsurvivors,
    proportions_survivors = proportions_survivors,
    p_value = chi_square_test$p.value
  )
}

# Calcular proporciones y valores p para sexo, etnicidad, y raza
calculate_proportions_and_p_value("Sat_type")


# Media y desviación estándar para no sobrevivientes
mean_sd_nonsurvivors <- c(mean = mean(nonsurvivors$Days_of_stay, na.rm = TRUE), sd = sd(nonsurvivors$Days_of_stay, na.rm = TRUE))

# Media y desviación estándar para sobrevivientes
mean_sd_survivors <- c(mean = mean(survivors$Days_of_stay, na.rm = TRUE), sd = sd(survivors$Days_of_stay, na.rm = TRUE))

# Prueba mann whitney para comparar medias de edad
t_test_result <- wilcox.test(nonsurvivors$Days_of_stay, survivors$Days_of_stay)
t_test_result$p.value
mean_sd_nonsurvivors
mean_sd_survivors


# VOY A HACER UNA TABLA COMPARANDO LAS VARIABLES CATEGORICAS PARA VER SI HAY DIFERENCIA
# SIGNIFICATIVA EN LOS DAYS OF STAY

# Filtrar datos por variable categorica (de ds categorias) y extraer la columna deseada
df2 <- datos %>% filter(Capillary_sat == "Normal") %>% pull(Days_of_stay)
df1 <- datos %>% filter(Capillary_sat == "Hypoxemia") %>% pull(Days_of_stay)

# Realizar la prueba de Mann-Whitney U
wilcox.test(na.omit(df1), na.omit(df2))

# para variables con mas de 2 categorias hacemos krustal wallis
kruskal.test(Days_of_stay ~ Pain_level, data = datos)

stats_summary <- datos %>%
  group_by(Sat_type) %>%
  summarise(
    mean = mean(Days_of_stay, na.rm = TRUE),
    sd = sd(Days_of_stay, na.rm = TRUE)
  )

# Añadir el intervalo media ± sd
stats_summary <- stats_summary %>%
  mutate(
    lower_bound = mean - sd,
    upper_bound = mean + sd
  )

print(stats_summary)





