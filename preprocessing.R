library(readxl) # para abrir los excels
library(dplyr) # para usar el %>%
library(tidyr) # para usar pivot_wider
library(writexl) # para guardar el excel


###Import datasets and transform variables to their type
##Basic data
basic_data <- read_excel("data/basic_data.xlsx")

#Transformar a formato fecha
basic_data <- basic_data %>%
  mutate(Fecha_validaci_n_prueba = as.Date(Fecha_validaci_n_prueba, format = "%Y-%m-%d"))

basic_data <- basic_data %>%
  mutate(Fecha__ingreso_ = as.Date(Fecha__ingreso_, format = "%Y-%m-%d"))

basic_data <- basic_data %>%
  mutate(Fecha__alta_ = as.Date(Fecha__alta_, format = "%Y-%m-%d"))

basic_data <- basic_data %>%
  mutate(fecha_fallece = as.Date(fecha_fallece, format = "%Y-%m-%d"))

#Transformar a factor Id_Paciente, Desc_Prueba, Sexo, paso_uci
basic_data$Id_Paciente<- as.factor(basic_data$Id_Paciente)
basic_data$Desc_Prueba<- as.factor(basic_data$Desc_Prueba) # tipo de prueba Covid
basic_data$Sexo<- as.factor(basic_data$Sexo)
basic_data$paso_uci<- as.factor(basic_data$paso_uci) # estuvo en uci

# Calcular los días desde el diagnóstico hasta la muerte
basic_data <- basic_data %>%
  mutate(dias_hasta_muerte = as.numeric(difftime(fecha_fallece, Fecha_validaci_n_prueba, units = "days")))

summary(basic_data) # 1642 pacientes distintos

##Labs
labs <- read_excel("data/labs.xlsx", 
                   col_types = c("numeric", "text", "numeric", "text", "date",
                                 "numeric", "numeric"))

#transformar Id_Paciente, Desc_Prueba, Resultado_Patol_gico a factor
labs$Id_Paciente<- as.factor(labs$Id_Paciente)
labs$Desc_Prueba<- as.factor(labs$Desc_Prueba)
labs$Resultado_Patol_gico<- as.factor(labs$Resultado_Patol_gico)

summary(labs) # hay pacientes con muchisimas observaciones


##Vital signs
vital_signs <- read_excel("data/vital_signs.xlsx", 
                          col_types = c("numeric", "text", "text", "date",
                                        "numeric", "numeric"))

#transformar Id_Paciente, Item__form_cte_, Valor__form_cte_ a factor
vital_signs$Id_Paciente<- as.factor(vital_signs$Id_Paciente)
vital_signs$Item__form_cte_<- as.factor(vital_signs$Item__form_cte_)
vital_signs$Valor__form_cte_<- as.factor(vital_signs$Valor__form_cte_)

summary(vital_signs) # otra vez muchas observaciones para un mismo paciente

# hay que mirar si tenemos datos repetidos. 


# vamos a resumir los desc_prueba y resultado_n_mero. Para ello, lo que vamos a hacer es crear nuevas
# variables, una para cada tipo de prueba. PAra cada paciente se guardará la media de las pruebas
# que se le han hecho y si alguna prueba no se le ha hecho se pone NA

# tenemos 35 pruebas distinas creo.

#Vamos a crear una nueva base de datos sobre la que luego haremos el analisis. 

datos <- basic_data

# vamos a añadir a la base de datos los resultados de los labs. los vamos a resumir
# con la media para cada paciente. 
df_mean <- labs %>%
  group_by(Id_Paciente, Desc_Prueba) %>%
  summarise(Media_Resultado = mean(Resultado_n_mero, na.rm = TRUE)) %>%
  ungroup()

# Pivotar la tabla
df_wide <- df_mean %>%
  pivot_wider(names_from = Desc_Prueba, values_from = Media_Resultado)

# Unir df_existente con df_wide usando left_join
datos <- datos %>%
  left_join(df_wide, by = "Id_Paciente")


# ahora vamos a hacer nuevas variables a partir de las categorias de Item__form_cte_
# sin embargo como categoria esta recogida de distintas maneras (algunas numericas, otras categoricas etc)
# no podemos hacer la media para cada paciente. Vamos a ir una por una. 


######################## Cantidad de Sat ######################## 
# Estan guardados los datos de distintas maneras. Vamos a cogerlos como porcen-
# tajes, la equivalencia la he cogido buscando aporte externo de o2 en litros o porcentajes
# https://www.pediatriaintegral.es//wp-content/uploads/2021/xxv01/05/n1-037-043_RB_AlbGcia_TablaIII.jpg

# Definir una función para mapear los valores 
sat_values <- function(value) {
  value <- as.character(value)
  if (value %in% c("1L", "1", 1)) {
    return(0.24)
  } else if (value == "2L") {
    return(0.28)
  } else if (value == "3L") {
    return(0.32)
  } else if (value == "4L") {
    return(0.36)
  } else if (value == "5L") {
    return(0.40)
  } else if (value == "6L") {
    return(0.44)
  } else if (value == "Sin O2") {
    return(0)
  } else {
    return(as.numeric(value))
  }
}

vital_signs$Valor__form_cte_ <- as.character(vital_signs$Valor__form_cte_)

# Iteramos sobre cada fila
for (i in 1:nrow(vital_signs)) {
  # Verificamos la condición
  if (vital_signs$Item__form_cte_[i] == "Cantidad de Sat" & !is.na(vital_signs$Valor__form_cte_[i])) {
    # Aplicamos la transformación
    vital_signs$Valor__form_cte_[i] <- sat_values(vital_signs$Valor__form_cte_[i])
  }
}

# Filtrar los datos para obtener solo las filas donde Item__form_cte_ es 'Cantidad de Sat'
sat_data <- vital_signs %>%
  filter(Item__form_cte_ == 'Cantidad de Sat')

# Calcular la media de Valor__form_cte_ para cada Id_Paciente
media_sat <- sat_data %>%
  group_by(Id_Paciente) %>%
  summarise(Cantidad_Sat = mean(as.numeric(Valor__form_cte_), na.rm = TRUE))

# Unir la media calculada con la base de datos original
datos <- datos %>%
  left_join(media_sat, by = "Id_Paciente")


######################## Diuresis Espont ######################## 
# resumimos con la media
diuresis_data <- vital_signs %>%
  filter(Item__form_cte_ == 'Diuresis Espont')

# Calcular la media de Valor__form_cte_ para cada Id_Paciente
media_diuresis <- diuresis_data %>%
  group_by(Id_Paciente) %>%
  summarise(Diuresis_Espont = mean(as.numeric(Valor__form_cte_), na.rm = TRUE))

# Unir la media calculada con la base de datos original
datos <- datos %>%
  left_join(media_diuresis, by = "Id_Paciente")


######################## Escala de dolor ######################## 
# no se si es mejor poner esta como ordinal 

# vamos a resumir los valores de cada individuo con la moda
moda <- function(x) {
  x <- x[!is.na(x)]  # Eliminar NAs
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# primero vamos a reclasificar los valores en no dolor, dolor leve, dolor moderado,
# dolor severo y no valorable



dolor_data <- vital_signs %>%
  filter(Item__form_cte_ == 'Escala de dolor')

escala_dolor <- factor(dolor_data$Valor__form_cte_,
                       levels = c("No dolor - 0" , "Dolor leve - 1", 
                                  "Dolor leve - 2", "Dolor moderado - 3", 
                                  "Dolor moderado - 4", "Dolor moderado - 5",
                                  "Dolor severo - 6", "Dolor severo - 7", 
                                  "Dolor severo - 8", "Dolor insoportable - 9",
                                  "Dolor insoportable - 10", "No valorable"),
                       labels = c("no dolor", "dolor leve", "dolor leve",
                                  "dolor moderado", "dolor moderado", 
                                  "dolor moderado", "dolor severo", 
                                  "dolor severo", "dolor severo", 
                                  "dolor insoportable", "dolor insoportable",
                                  "no valorable"))

# Calcular la media de Valor__form_cte_ para cada Id_Paciente
moda_dolor <- dolor_data %>%
  group_by(Id_Paciente) %>%
  summarise(Escala_Dolor = moda(as.factor(Valor__form_cte_)))

# Unir la media calculada con la base de datos original
datos <- datos %>%
  left_join(moda_dolor, by = "Id_Paciente")


######################## Frecuencia Card ######################## 
# usamos la media

card_data <- vital_signs %>%
  filter(Item__form_cte_ == 'Frecuencia Card')

# Calcular la media de Valor__form_cte_ para cada Id_Paciente
media_card <- card_data %>%
  group_by(Id_Paciente) %>%
  summarise(Frecuencia_Card = mean(as.numeric(Valor__form_cte_), na.rm = TRUE))

# Unir la media calculada con la base de datos original
datos <- datos %>%
  left_join(media_card, by = "Id_Paciente")


######################## Frecuencia Resp ######################## 
# usamos la media

resp_data <- vital_signs %>%
  filter(Item__form_cte_ == 'Frecuencia Resp')

# Calcular la media de Valor__form_cte_ para cada Id_Paciente
media_resp <- resp_data %>%
  group_by(Id_Paciente) %>%
  summarise(Frecuencia_Resp = mean(as.numeric(Valor__form_cte_), na.rm = TRUE))

# Unir la media calculada con la base de datos original
datos <- datos %>%
  left_join(media_resp, by = "Id_Paciente")


######################## glucemia_basal ######################## 
# la vamos a definir como categorica
# normal es entre 70 y 110
# HI es mayor de 200 aprox      # seria mejor poner esta como muy alta?
# alta mas de 110 aprox
# baja menos de 70
# los valores que son 0 estan mal recogidos

gluc_values <- function(value) {
  value <- as.character(value)
  if (value == "HI") {
    return("high")
  } else if (value == "0") {
    return(NA)
  } else {
    num_value <- as.numeric(value)
    if (!is.na(num_value)) {
      if (num_value > 110) {
        return("High")
      } else if (num_value < 70) {
        return("Low")
      } else {
        return("Normal")
      }
    } else {
      return(NA)
    }
  }
}

# Iteramos sobre cada fila
for (i in 1:nrow(vital_signs)) {
  # Verificamos la condición
  if (vital_signs$Item__form_cte_[i] == "glucemia_basal" & !is.na(vital_signs$Valor__form_cte_[i])) {
    # Aplicamos la transformación
    vital_signs$Valor__form_cte_[i] <- gluc_values(vital_signs$Valor__form_cte_[i])
  }
}

# Filtrar los datos para obtener solo las filas donde Item__form_cte_ es 'glucemia_basal'
gluc_data <- vital_signs %>%
  filter(Item__form_cte_ == 'glucemia_basal')

# Calcular la media de Valor__form_cte_ para cada Id_Paciente
moda_gluc <- gluc_data %>%
  group_by(Id_Paciente) %>%
  summarise(Glucemia_Basal = moda(as.factor(Valor__form_cte_)))

# Unir la moda calculada con la base de datos original
datos <- datos %>%
  left_join(moda_gluc, by = "Id_Paciente")


######################## IMC ######################## 
# como es combinacion lineal de peso y talla no lo vamos a coger. Ademas solo tiene 37 instancias

######################## peso ########################  
# 159 observaciones o algo asi
# voy a tener que quitar esta variable tmb

peso_data <- vital_signs %>%
  filter(Item__form_cte_ == 'Peso')

# Reemplazar comas con puntos y convertir a numérico
peso_data <- peso_data %>%
  mutate(Valor__form_cte_ = as.numeric(gsub(",", ".", Valor__form_cte_)))


# Calcular la media de Valor__form_cte_ para cada Id_Paciente
media_peso <- peso_data %>%
  group_by(Id_Paciente) %>%
  summarise(peso = mean(as.numeric(Valor__form_cte_), na.rm = TRUE))

# Unir la media calculada con la base de datos original
datos <- datos %>%
  left_join(media_peso, by = "Id_Paciente")


######################## presion arterial ########################  

# hipotenso menos de 100
# normotenso entre 100 y 150
# hipertenso mas de 150

#tenemos valores que son 0 que estan mal registrados asi que los ponemos como NAs
# no se si seria mejor dejarlo en numerico o hacer categorias 

pres_values <- function(value) {
  value <- as.numeric(value)
  if (value == 0) {
    return(NA)
  }
  else if (!is.na(value)) {
    if (value < 100) {
      return("Hypotensive")
    } else if (value > 150) {
      return("Hypertensive")
    } else {
      return("Normotensive")
    }
  } 
  else {
    return(NA)
  }
}

# Iteramos sobre cada fila
for (i in 1:nrow(vital_signs)) {
  # Verificamos la condición
  if (vital_signs$Item__form_cte_[i] == "Presión arteria" & !is.na(vital_signs$Valor__form_cte_[i])) {
    # Aplicamos la transformación
    vital_signs$Valor__form_cte_[i] <- pres_values(vital_signs$Valor__form_cte_[i])
  }
}

# Filtrar los datos para obtener solo las filas donde Item__form_cte_ es 'presion arterial'
pres_data <- vital_signs %>%
  filter(Item__form_cte_ == 'Presión arteria')

# Calcular la moda de Valor__form_cte_ para cada Id_Paciente
moda_pres <- pres_data %>%
  group_by(Id_Paciente) %>%
  summarise(Presion_Arterial = moda(as.factor(Valor__form_cte_)))

# Unir la moda calculada con la base de datos original
datos <- datos %>%
  left_join(moda_pres, by = "Id_Paciente")


######################## saturacion capi ########################

# hipoxemia debajo de 94                      # comprobar que esto sea el valor
# saturacion normal por encima de 94 (pero hasta 100)

# el 925 esta mal recogido
# el 37.4 esta mal recogido, sera la temperatura
# 349 mal recogido, temperatura de 34.9

satcap_values <- function(value) {
  value <- as.numeric(value)
  if (value == 0 | value > 100 | value == 37.4) {
    return(NA)
  } else if (!is.na(value)) {
    if (value < 94) {
      return("Hypoxemia")
    } else if (value >= 94) {
      return("Normal")
    }
  }
  return(NA)
}

# Iteramos sobre cada fila
for (i in 1:nrow(vital_signs)) {
  # Verificamos la condición
  if (vital_signs$Item__form_cte_[i] == "Saturación capi" & !is.na(vital_signs$Valor__form_cte_[i])) {
    # Aplicamos la transformación
    vital_signs$Valor__form_cte_[i] <- satcap_values(vital_signs$Valor__form_cte_[i])
  }
}

# Filtrar los datos para obtener solo las filas donde Item__form_cte_ es 'presion arterial'
satcapi_data <- vital_signs %>%
  filter(Item__form_cte_ == 'Saturación capi')

# Calcular la moda de Valor__form_cte_ para cada Id_Paciente
moda_satcapi <- satcapi_data %>%
  group_by(Id_Paciente) %>%
  summarise(Saturacion_Capilar = moda(as.factor(Valor__form_cte_)))

# Unir la moda calculada con la base de datos original
datos <- datos %>%
  left_join(moda_satcapi, by = "Id_Paciente")


######################## talla (cm) ########################
# no vamos a usar esta variable porqye solo hat 35 observaciones y por lo tanto
# no me voy a poder usarla mas adelante en nuestras predicciones

talla_data <- vital_signs %>%
  filter(Item__form_cte_ == 'talla (cm)')

# Reemplazar comas con puntos y convertir a numérico
talla_data <- talla_data %>%
  mutate(Valor__form_cte_ = as.numeric(Valor__form_cte_))


# Calcular la media de Valor__form_cte_ para cada Id_Paciente
media_talla <- talla_data %>%
  group_by(Id_Paciente) %>%
  summarise(talla = mean(as.numeric(Valor__form_cte_), na.rm = TRUE))

# Unir la media calculada con la base de datos original
datos <- datos %>%
  left_join(media_talla, by = "Id_Paciente")

######################## temperatura ########################
# recomendacion: que los que tomen los datos usen solo punto para los decimales no comas
# los valores de 53 a 98 estan mal recogidos, los valores de 90 y algo pueden ser la tension

# el 20 y 1500 tmb estan mal

# no se si para esta variable a lo mejor seria mejor coger el maximo de temperatura que alcanzan 

temp_data <- vital_signs %>%
  filter(Item__form_cte_ == 'Temperatura ºC')

# Reemplazar comas con puntos y convertir a numérico
temp_data <- temp_data %>%
  mutate(Valor__form_cte_ = as.numeric(gsub(",", ".", Valor__form_cte_)))

# Iteramos sobre cada fila
for (i in 1:nrow(temp_data)) {
  # Verificamos si el valor no es NA y cumple la condición
  if (!is.na(temp_data$Valor__form_cte_[i]) && (temp_data$Valor__form_cte_[i] > 42 | temp_data$Valor__form_cte_[i] < 32)) {
    # Aplicamos la transformación
    temp_data$Valor__form_cte_[i] <- NA
  }
}


# Calcular la media de Valor__form_cte_ para cada Id_Paciente
media_temp <- temp_data %>%
  group_by(Id_Paciente) %>%
  summarise(Temperatura = mean(as.numeric(Valor__form_cte_), na.rm = TRUE))

# Unir la media calculada con la base de datos original
datos <- datos %>%
  left_join(media_temp, by = "Id_Paciente")

######################## tipo de saturacion ########################

# Gafas O2
# Aire ambiente 
# VMK (ventimasc)
# VMNI (ventilacion mecanica no invasiva)
# Optiflow
# IOT

tiposat_data <- vital_signs %>%
  filter(Item__form_cte_ == 'Tipo de Saturac')

# Calcular la media de Valor__form_cte_ para cada Id_Paciente
moda_tiposat <- tiposat_data %>%
  group_by(Id_Paciente) %>%
  summarise(Tipo_Saturacion = moda(as.factor(Valor__form_cte_)))

# Unir la moda calculada con la base de datos original
datos <- datos %>%
  left_join(moda_tiposat, by = "Id_Paciente")






# quito el ID, cambio el nombre de las variables largas y las que tienen guiones 

datos <- datos[, -1]
datos <- datos %>% rename(edad = Edad_a_os__actual_)

names(datos)[names(datos) == "Pla-Tiempo (Indice) de protrombina (%)"] <- "Prothrombin"
names(datos)[names(datos) == "San-Basófilos"] <- "Basophils"
names(datos)[names(datos) == "San-Eosinófilos"] <- "Eosinophils"
names(datos)[names(datos) == "San-Linfocitos"] <- "Lymphocytes"
names(datos)[names(datos) == "San-Monocitos"] <- "Monocytes"
names(datos)[names(datos) == "San-Neutrófilos"] <- "Neutrophils"
names(datos)[names(datos) == "Srm-Creatinina"] <- "Creatinine"
names(datos)[names(datos) == "Srm-Creatinquinasa (CK)"] <- "CK"
names(datos)[names(datos) == "Srm-Glucosa"] <- "Glucose"
names(datos)[names(datos) == "Srm-GPT"] <- "GPT"
names(datos)[names(datos) == "Srm-Lactato deshidrogenasa"] <- "LD"
names(datos)[names(datos) == "Srm-Potasio"] <- "Potassium"
names(datos)[names(datos) == "Srm-Procalcitonina (PCT)"] <- "PCT"
names(datos)[names(datos) == "Srm-Proteina C reactiva (PCR)"] <- "CRP"
names(datos)[names(datos) == "Srm-Sodio"] <- "Sodium"
names(datos)[names(datos) == "Srm-Troponina T cardiaca ultrasensible"] <- "TnT"
names(datos)[names(datos) == "Srm-Urea"] <- "Urea"
names(datos)[names(datos) == "Pla-Dimero D"] <- "D_dimer"
names(datos)[names(datos) == "Pla-Fibrinogeno (derivado)"] <- "Derived_fibrinogen"
names(datos)[names(datos) == "Srm-Bilirrubina"] <- "BR"
names(datos)[names(datos) == "Srm-GOT"] <- "GOT"
names(datos)[names(datos) == "Srm-Ferritina"] <- "Ferritina"
names(datos)[names(datos) == "Pla-Fibrinogeno (funcional)"] <- "Functional_fibrinogen"
names(datos)[names(datos) == "Srm-Tasa de Filtracion Glomerular (CKD"] <- "GFR"
names(datos)[names(datos) == "Srm-GGT"] <- "GGT"
names(datos)[names(datos) == "Srm-Inmunoglobulina A (IgA)"] <- "IgA"
names(datos)[names(datos) == "Srm-Inmunoglobulina G (IgG)"] <- "IgG"
names(datos)[names(datos) == "Srm-Inmunoglobulina M (IgM)"] <- "IgM"
names(datos)[names(datos) == "Srm-Calcidiol (25 Hidroxi Vitamina D)"] <- "Calcidiol"
names(datos)[names(datos) == "San-Leucocitos"] <- "Leukocytes"
names(datos)[names(datos) == "San-Hematíes"] <- "RBCs"
names(datos)[names(datos) == "San-Hematocrito"] <- "Hematocrit"
names(datos)[names(datos) == "San-Hemoglobina"] <- "Hemoglobine"
names(datos)[names(datos) == "San-Volumen corpuscular medio (VCM)"] <- "MCV"



names(datos)
# eliminamos las variables de fechas porque ya las hemos resumido y no nos interesan mas
datos <- datos[, !grepl("^fecha", names(datos)), with = FALSE]
datos <- datos[, !grepl("^Fecha", names(datos)), with = FALSE]

names(datos)[names(datos) == "Desc_Prueba"] <- "Test"
names(datos)[names(datos) == "Sexo"] <- "Sex"
names(datos)[names(datos) == "edad"] <- "Age"
names(datos)[names(datos) == "dias_hasta_ingreso"] <- "Days_until_admission"
names(datos)[names(datos) == "dias_estancia"] <- "Days_of_stay"
names(datos)[names(datos) == "paso_uci"] <- "ICU_stay"
names(datos)[names(datos) == "dias_hasta_muerte"] <- "Days_until_death"
names(datos)[names(datos) == "San-Plaquetas"] <- "Platelets"
names(datos)[names(datos) == "Diuresis_Espont"] <- "Spont_diuresis"
names(datos)[names(datos) == "Escala_Dolor"] <- "Pain_level"
names(datos)[names(datos) == "Frecuencia_Card"] <- "Heart_rate"
names(datos)[names(datos) == "Frecuencia_Resp"] <- "Resp_rate"
names(datos)[names(datos) == "Glucemia_Basal"] <- "Basal_glycemia"
names(datos)[names(datos) == "peso"] <- "Weight"
names(datos)[names(datos) == "Presion_Arterial"] <- "Blood_pressure"
names(datos)[names(datos) == "Saturacion_Capilar"] <- "Capillary_sat"
names(datos)[names(datos) == "talla"] <- "Height"
names(datos)[names(datos) == "Temperatura"] <- "Temperature"
names(datos)[names(datos) == "Tipo_Saturacion"] <- "Sat_type"

datos$death <- ifelse(!is.na(datos$Days_until_death) & datos$Days_until_death > 0, 1, 0)
datos$death <- as.factor(datos$death)
datos$death <- factor(datos$death, levels = c(0, 1), labels = c("No", "Yes"))

datos$ICU_stay <- factor(datos$ICU_stay, levels = c(0, 1), labels = c("No", "Yes"))

datos$Prothrombin[datos$Prothrombin > 100] <- NA

levels(datos$Test) <- make.names(levels(datos$Test))
levels(datos$Sat_type) <- make.names(levels(datos$Sat_type))

# vuelvo a categorizar escala de dolor: no dolor, dolor, no valorable
datos$Pain_level <- factor(datos$Pain_level,
                             levels = c("No dolor - 0", "Dolor leve - 1", 
                                        "Dolor leve - 2", "Dolor moderado - 3", 
                                        "Dolor moderado - 4", "Dolor moderado - 5",
                                        "Dolor severo - 6", "Dolor insoportable - 9",
                                        "No valorable"),
                             labels = c("nopain", "pain", "pain", "pain", "pain",
                                        "pain", "pain", "pain", "notapplicable"))


datos$Sex <- factor(datos$Sex, levels = c("Hombre", "Mujer"), labels = c("Man", "Woman"))


datos <- subset(datos, select = -Days_until_death)



# Guardar el data frame en un archivo Excel
write_xlsx(datos, "data/datos.xlsx")



