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
library(pROC) #para roc



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


summary(datos)

############################### ---- 1. SUMMARY NUMERICAS ---- 


# voy a guardar un excel con el summary de las variables numericas
datos_numericos <- datos %>% select(where(is.numeric))

ddt = data.table(datos_numericos)

write_clip(ddt[, list(
  "Age",
  Min = min(Age, na.rm = TRUE), Max = max(Age, na.rm = TRUE),
  Range = max(Age, na.rm = TRUE) - min(Age, na.rm = TRUE),
  Mean = mean(Age, na.rm = TRUE), sd = sd(Age, na.rm = TRUE),
  Median = median(Age, na.rm = TRUE), IQR = IQR(Age, na.rm = TRUE),
  NAs = sum(is.na(Age)))])

write_clip(ddt[, list(
  "Prothrombin",
  Min = min(Prothrombin, na.rm = TRUE), Max = max(Prothrombin, na.rm = TRUE),
  Range = max(Prothrombin, na.rm = TRUE) - min(Prothrombin, na.rm = TRUE),
  Mean = mean(Prothrombin, na.rm = TRUE), sd = sd(Prothrombin, na.rm = TRUE),
  Median = median(Prothrombin, na.rm = TRUE), IQR = IQR(Prothrombin, na.rm = TRUE),
  NAs = sum(is.na(Prothrombin)))])


# voy a quitar variables que no me interesan por que la mayoria de sus valores son NAs

# imprimo una tabla con el % de NAs de cada variable de la base nueva (tengo 57 variables)

nas <- sapply(datos, function(x) 100*sum(is.na(x))/1642) # Calcular el % de NAs por columna

tabla_nas <- data.frame(Variable = names(nas), NAs = nas)

write_clip(tabla_nas)

# las fechas las voy a quitar por que no me interesan y las que tienen mas de 40% NAs

# Filtrar variables con más del 30% de NAs (menos dias hasta death ) y aquellas que contienen "fecha" en su nombre
variables_a_eliminar <- tabla_nas$Variable[(tabla_nas$NAs > 30)]

# Crear un nuevo data frame sin las variables a eliminar
datos <- datos[, !names(datos) %in% variables_a_eliminar]

datos_numericos <- datos %>% select(where(is.numeric))


############################### ---- 2. VARIABLES  OBJETIVO ---- 

ggplot(data.frame(x = datos$Days_of_stay), aes(sample = x)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ plot of length of hospitalization") + 
  theme_minimal() # esta no es lineal yo creo



num_df_scaled <- as.data.frame(scale(datos_numericos)) # Scale the variables
summary(num_df_scaled) #Once scaled
outliers_age <- abs(num_df_scaled$Derived_fibrinogen) > 3

count_outliers_age<- sum(outliers_age)
cat("Number of outliers in the variable age:", count_outliers_age, "\n")


############################### ---- 3. GRÁFICAS EDA ---- 

# variables respuesta: dias_estancia, death , uci

# correaltion plot
# windows()
# ggpairs(datos_numericos, aes(fill = "darkblue"),
#         lower = list(continuous = "points"),
#         diag = list(continuous = "barDiag"),
#         title = "Scatter Plot Matrix") +
#   scale_fill_manual(values = "darkblue") + 
#   theme_minimal()

# variables vs. death 

ggplot(datos, aes(x = Age, fill = death )) +
  geom_density(alpha = 0.5, color = "black") +
  scale_fill_manual(values = c("darkblue", "cornflowerblue")) +
  labs(title = "Density Plot of Age") +
  guides(fill=FALSE) +
  theme_minimal()

ggplot(datos, aes(x = Age, fill = death )) +
  geom_density(alpha = 0.5, color = "black") +
  scale_fill_manual(values = c("darkblue", "cornflowerblue"), 
                    name = "Death",            
                    labels = c("No", "Yes")) +   
  labs(title = "Density Plot of Age", x = "Age", y = "Density") +
  theme_minimal()


# variables categoricas vs. death 
ggplot(na.omit(datos), aes(x = Sex, fill = death )) +
  geom_bar(position = "fill") +
  theme_minimal()+
  scale_fill_manual(values = c("darkblue", "cornflowerblue"), 
                    name = "Death",            
                    labels = c("No", "Yes")) +
  scale_x_discrete(labels = c("Hombre" = "Man", "Mujer" = "Woman")) +   
  labs(title = "Bar Plot of Sex vs. Death",
       x = "Sex",           
       y = "Proportion") +
  theme_minimal()

#diferencias significativas? 
tabla <- table(datos$death , datos$Sex)
prop.test(tabla[2, ], tabla[1, ] + tabla[2, ]) # pvalor= 0.1728. no hay diferencias significativas




ggplot(na.omit(datos), aes(x = ICU_stay, fill = death )) +
  geom_bar(position = "fill") +
  theme_minimal()+
  scale_fill_manual(values = c("darkblue", "cornflowerblue"), 
                    name = "Death",            
                    labels = c("No", "Yes")) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +   
  labs(title = "Bar Plot of ICU stay vs. Death",
       x = "ICU stay",           
       y = "Proportion") +
  theme_minimal()

#diferencias significativas? 
tabla <- table(datos$death , datos$ICU_stay)
prop.test(tabla[2, ], tabla[1, ] + tabla[2, ]) # pvalor=0.05739 no hay diferencias significativas



ggplot(na.omit(datos), aes(x = Pain_level, fill = death )) +
  geom_bar(position = "fill") +
  theme_minimal()+
  scale_fill_manual(values = c("darkblue", "cornflowerblue"), 
                    name = "Death",            
                    labels = c("No", "Yes")) +
  scale_x_discrete(labels = c("dolor" = "Pain", "no dolor" = "No pain", "no valorable" = "Not applicable")) +   
  labs(title = "Bar Plot of Pain Level vs. Death",
       x = "Pain level",           
       y = "Proportion") +
  theme_minimal()

#diferencias significativas? 
tabla <- table(datos$death , datos$Pain_level)
chisq.test(tabla) # p-value = 2.979e-14 si que hay diferencias

prop.test(tabla[2, ], tabla[1, ] + tabla[2, ])


# me falta por hacer variables vs. dias estancia



############################### ---- 4. TRASFORMACIONES ---- 

ggplot(datos, aes(x = Days_of_stay)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Days of hospitalization distribution",
       x = "Days of hospitalization",
       y = "Frequency") + 
  theme_minimal()

ggplot(datos, aes(x = log(Days_of_stay+1))) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Days of hospitalization distribution",
       x = "Days of hospitalization",
       y = "Frequency") + 
  theme_minimal() # con el logaritmo porque no sigue una distr normal


#en realidad como voy a hacer modelos de machine learning no hace falta
# "normalizar" la vaariable respuesta. Pero como ya he calculadoo todos los modelos
# lo dejo asi. Paara los errores si que voy a utilizar el valor real de days_of_stay
# asi que guardo los observed values en una nueva variable


# para los modelos de death voy a dejar estta variable assi ya. pero para
# los modelos de days of stay voy a dejarla en su escala original. 
datos$Days_of_stay <- log(datos$Days_of_stay +1)



############################### ---- 5. TRAIN/TEST & NAs ---- 

spl <- createDataPartition(datos$death,  p = 0.7, list = FALSE) 
datatrain <- datos[spl,]
datatest<- datos[-spl,]

windows()
result<- aggr(datatrain, 
              numbers = TRUE, 
              sortVars = TRUE, 
              labels = names(data),
              cex.axis = .5, 
              gap = 1,
              ylab= c('Missing data','Pattern'),
              col = c("darkblue", "cornflowerblue"))

datatrain <- kNN(data = datatrain, k = 7)[1:34] # pongo los subindices porque sino me crea nuevas variables con true false
datatest <- kNN(data = datatest, k = 7)[1:34]

datatest$death <- as.factor(datatest$death)

result<- aggr(datatrain, 
              numbers = TRUE, 
              sortVars = TRUE, 
              labels = names(data),
              cex.axis = .5, 
              gap = 1,
              ylab= c('Missing data','Pattern'),
              col = c("darkblue", "cornflowerblue"))





############################### ---- 6. MODELOS ---- 

set.seed(123)
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 5,
                     number = 10,
                     classProbs =  TRUE,
                     returnResamp = "final")



############################### ---- 6.1. death  ---- 

# UNBALANCED DATASET: el train tiene 996 vivos y 154 muertos 
# como no esta muy balanceado el dataset, voy a hacer roc curves al final

#KNN
knnFit <- train(death ~ ., 
                method = "knn", 
                data = datatrain,
                preProcess = c("center", "scale"),
                tuneGrid = expand.grid(k = 1:25),
                metric = "Accuracy",
                trControl = ctrl)

saveRDS(knnFit, "modelos/knnFit.rds")

knnFit <- readRDS("modelos/knnFit.rds")

windows()
plot(varImp(knnFit, scale = F))

knnProb <- predict(knnFit, datatest, type="prob")
knnPred <- predict(knnFit, datatest)    

knnCM <- confusionMatrix(knnPred, datatest$death)

accuracy_knn <- round(knnCM$overall[1], digits = 4)
kappa_knn <- round(knnCM$overall[2], digits= 4)
sens_knn <- 0.9977
spec_knn <- 0.2121

table(knnPred, datatest$death)

windows()
ggplot(knnFit, highlight = TRUE) +
  labs(title = "Evolution of the accuracy of the kNN model") +
  theme_bw()

#SVM

svmFit <- train(death ~., 
                method = "svmRadial", 
                data = datatrain,
                preProcess = c("center", "scale"),
                tuneGrid = expand.grid(C = seq(0, 4, length = 20),
                                       sigma = c(0.005, 0.01,.05, 1)), 
                metric = "Accuracy",
                trControl = ctrl)

saveRDS(svmFit, "modelos/svmFit")
svmFit <- readRDS("modelos/svmFit")

plot(varImp(svmFit, scale = F))

svmProb <- predict(svmFit, datatest, type="prob")
svmPred <- predict(svmFit, datatest)

svmCM <- confusionMatrix(svmPred, datatest$death)

accuracy_svm <- round(svmCM$overall[1], digits = 4)
kappa_svm <- round(svmCM$overall[2], digits = 4)
sens_svm <- 0.9859
spec_svm <- 0.4848

table(svmPred, datatest$death)

ggplot(svmFit, highlight = TRUE) +
  labs(title = "Evolution of the accuracy of the SVM Radial model") +
  theme_bw()


# DECISION TREES
grid <- expand.grid( .winnow = c(TRUE,FALSE),
                     .trials=c(1, 5, 10, 15, 20, 25), .model = "tree" )

dtFit <- train(death ~.,
               data = datatrain,
               method = "C5.0",
               metric = "Accuracy",
               tuneGrid = grid,
               trControl = ctrl)

saveRDS(dtFit, "modelos/dtFit")
dtFit <- readRDS("modelos/dtFIt")
plot(varImp(dtFit, scale = F))

dtProb <- predict(dtFit, datatest, type="prob")
dtPred <- predict(dtFit, datatest)

dtCM <- confusionMatrix(dtPred, datatest$death) 

accuracy_dt <- round(dtCM$overall[1], digits = 4)
kappa_dt <- round(dtCM$overall[2], digits = 4)
sens_dt <- 0.9906
spec_dt <- 0.3788


table(dtPred, datatest$death)

ggplot(dtFit, highlight = TRUE) +
  labs(title = "Evolution of the accuracy of the decision tree model") +
  theme_bw()

# RANDOM FOREST
rfFit <- train(death ~ ., 
               method = "rf", 
               data = datatrain,
               preProcess = c("center", "scale"),
               ntree = 400,
               tuneGrid = expand.grid(mtry=c(4, 6, 8, 10, 12, 14)),                 
               metric = "Accuracy",
               trControl = ctrl)

saveRDS(rfFit, "modelos/rfFit")
rfFit <- readRDS("modelos/rfFit")
plot(varImp(rfFit, scale = F))

rfProb <- predict(rfFit, datatest, type="prob")
rfPred <- predict(rfFit, datatest)

rfCM <- confusionMatrix(rfPred, datatest$death)

accuracy_rf <- round(rfCM$overall[1], digits = 4) #0.9004
kappa_rf <- round(rfCM$overall[2], digits = 4)
sens_rf <- 0.9906
spec_rf <- 0.3788


table(rfPred, datatest$death)

ggplot(rfFit, highlight = TRUE) +
  labs(title = "Evolution of the accuracy of the random forest model") +
  theme_bw()


# GRADIENT BOOSTING
xgb_grid = expand.grid(
  nrounds = c(100, 200, 300),
  eta = c(0.001, 0.01, 0.05), 
  max_depth = c(2, 4, 6),
  gamma = c(1),
  colsample_bytree = c(0.2, 0.4, 0.6),
  min_child_weight = c(1, 5),
  subsample = 1
)

xgbFit <- train(death ~ .,  
                data = datatrain,
                trControl = ctrl,
                metric="Accuracy",
                maximize = F,
                tuneGrid = xgb_grid,
                preProcess = c("center", "scale"),
                method = "xgbTree")

saveRDS(xgbFit, file = "modelos/xgbFit.rds")
xgbFit <- readRDS("modelos/xgbFit.rds")
plot(varImp(xgbFit, scale = F))

xgbProb <- predict(xgbFit, datatest, type="prob")
xgbPred <- predict(xgbFit, datatest) 

xgbCM <- confusionMatrix(xgbPred, datatest$death) 

accuracy_xgb <- round(xgbCM$overall[1], digits = 4)
kappa_xgb <- round(xgbCM$overall[2], digits = 4)
sens_xgb <- 1
spec_xgb <- 0.09091


table(xgbPred, datatest$death)

results <- xgbFit$results
windows()
# Crear un gráfico personalizado
ggplot(results, aes(x = nrounds, y = Accuracy, color = as.factor(eta))) +
  geom_point() +
  geom_line() +
  labs(title = "Evolution of the accuracy of the gradient boosting model",
       x = "nrounds",
       y = "Accuracy",
       color = "eta") +
  theme_minimal()


ggplot(results, aes(x = colsample_bytree, y = Accuracy, color = as.factor(eta))) +
  geom_line(aes(group = eta)) +
  geom_point() +
  labs(x = "Número de Iteraciones (nrounds)", y = "Accuracy", color = "Tasa de Aprendizaje (eta)") +
  theme_minimal()

ggplot(results, aes(x = interaction(nrounds, eta), y = Accuracy, color = as.factor(max_depth), shape = as.factor(colsample_bytree))) +
  geom_point(size = 3) +
  labs(x = "nrounds:eta", y = "Accuracy", color = "Max Depth", shape = "Colsample by Tree") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# NEURAL NETWORKS
nnFit <- train(death ~., 
               method = "nnet", 
               data = datatrain,
               preProcess = c("center", "scale"),
               MaxNWts = 1000,
               maxit = 100,
               tuneGrid = expand.grid(size=c(12, 16, 20, 24), 
                                      decay=c(0.001, 0.01, 0.1, 0.5)), 
               metric = "Accuracy",
               trControl = ctrl)

saveRDS(nnFit, file = "modelos/nnFit.rds")
nnFit <- readRDS("modelos/nnFit.rds")
plot(varImp(nnFit, scale = F))

nnProb <- predict(nnFit, datatest, type="prob")
nnPred <- predict(nnFit, datatest) 

nnCM <- confusionMatrix(nnPred, datatest$death) 
accuracy_nn <- round(nnCM$overall[1], digits = 4)
kappa_nn <- round(nnCM$overall[2], digits = 4)
sens_nn <- 0.9789
spec_nn <- 0.4848

table(nnPred, datatest$death)

ggplot(nnFit, highlight = TRUE) +
  labs(title = "Evolution of the accuracy of the neural networks model") +
  theme_bw()

# DEEP NEURAL NETWORKS
dnnFit <- train(death ~., 
                method = "dnn", 
                data = datatrain,
                preProcess = c("center", "scale"),
                numepochs = 30, # number of iterations on the whole trainset
                tuneGrid = expand.grid(layer1 = 1:4,
                                       layer2 = 0:2,
                                       layer3 = 0:2,
                                       hidden_dropout = 0, 
                                       visible_dropout = 0),
                metric = "Accuracy",
                trControl = ctrl)

saveRDS(dnnFit, "modelos/dnnFit")
dnnFit <- readRDS("modelos/dnnFit")

plot(varImp(dnnFit, scale = F))

dnnProb <- predict(dnnFit, datatest, type="prob")
dnnPred <- predict(dnnFit, datatest) 

dnnCM <- confusionMatrix(dnnPred, datatest$death) 
accuracy_dnn <- round(dnnCM$overall[1], digits = 4)
kappa_dnn <- round(dnnCM$overall[2], digits = 4)
sens_dnn <- 0.9812
spec_dnn <- 0.4697

table(dnnPred, datatest$death)

ggplot(dnnFit, highlight = TRUE) +
  labs(title = "Evolution of the accuracy of the deep neural networks model") +
  theme_bw()


#RESULTADOS
modelos_death <- list(
  "kNN" = c(accuracy_knn, kappa_knn),
  "SVM" = c(accuracy_svm, kappa_svm),
  "Decision Trees" = c(accuracy_dt, kappa_dt),
  "Random Forest" = c(accuracy_rf, kappa_rf),
  "Gradient Boosting" = c(accuracy_xgb, kappa_xgb),
  "Neural Networks" = c(accuracy_nn, kappa_nn),
  "Deep Neural Networks" = c(accuracy_dnn, kappa_dnn)
)

# Convertir la lista a un marco de datos
df_modelos <- data.frame(Method = names(modelos_death), Accuracy = sapply(modelos_death, "[", 1), Kappa = sapply(modelos_death, "[", 2))

# Calculate ROC objects for each model
knnROC <- roc(datatest$death ~ knnProb[, 2])
svmROC <- roc(datatest$death ~ svmProb[, 2])
dtROC <- roc(datatest$death ~ dtProb[, 2])
rfROC <- roc(datatest$death ~ rfProb[, 2])
xgbROC <- roc(datatest$death ~ xgbProb[, 2])
nnROC <- roc(datatest$death ~ nnProb[, 2])
dnnROC <- roc(datatest$death ~ dnnProb[, 2])

knnauc <- auc(knnROC)
svmauc <- auc(svmROC)
dtauc <- auc(dtROC)
rfauc <- auc(rfROC)
xgbauc <- auc(xgbROC)
nnauc <- auc(nnROC)
dnnauc <- auc(dnnROC)

windows()
plot(knnROC, col = "salmon", print.thres = FALSE)
plot(svmROC, add = TRUE, col = 'goldenrod2', print.thres = T)
plot(dtROC, add = TRUE, col = 'yellowgreen', print.thres = F)
plot(rfROC, add = TRUE, col = 'mediumseagreen', print.thres = F)
plot(xgbROC, add = TRUE, col = 'darkturquoise', print.thres = F)
plot(nnROC, add = TRUE, col = 'dodgerblue', print.thres = F)
plot(dnnROC, add = TRUE, col = 'orchid', print.thres = F)
legend("bottomright", legend = c("kNN", "SVM", "Decision Trees", "Random Forest", "Gradient Boosting", "Neural Networks", "Deep Neural Networks"), col=c("palevioletred1", "yellow", "springgreen", "steelblue1", "tomato1", "blueviolet", "darkslategray1"), lwd=2, cex=0.6)


techniques <- c("KNN", "SVM", "DT", "RF", "XGB", "NN", "DNN")  
acc_values <- c(accuracy_knn, accuracy_svm, accuracy_dt, accuracy_rf, accuracy_xgb, accuracy_nn, accuracy_dnn) 
kappa_values <- c(kappa_knn, kappa_svm, kappa_dt, kappa_rf, kappa_xgb, kappa_nn, kappa_dnn)
sens_values <- c(sens_knn, sens_svm, sens_dt, sens_rf, sens_xgb, sens_nn, sens_dnn)
spec_values <- c(spec_knn, spec_svm, spec_dt, spec_rf, spec_xgb, spec_nn, spec_dnn)
auc_values <- c(knnauc, svmauc, dtauc, rfauc, xgbauc, nnauc, dnnauc)

df <- data.frame(
  Technique = techniques,
  Accuracy = acc_values,
  Kappa = kappa_values, 
  Sensitivity = sens_values, 
  Specificity = spec_values, 
  AUC = auc_values
)

library(data.table)
df_long <- reshape2::melt(df, id.vars = "Technique", variable.name = "Metric", value.name = "Value")

windows()
ggplot(df_long, aes(x = Metric, y = Value, color = Technique, group = Technique)) +
  geom_line() +
  geom_point() +
  labs(title = "Performance Metrics for Different Techniques", x = "Metric", y = "Value") +
  theme_minimal()+
  theme(
    legend.title = element_text(size = 18),   
    legend.text = element_text(size = 16),    
    axis.title.x = element_text(size = 18),   
    axis.title.y = element_text(size = 18),   
    axis.text.x = element_text(size = 16,  angle = 45, hjust = 1),    
    axis.text.y = element_text(size = 16))


## MODELO MEJORADO

bestFit <- train(death ~ ., 
                 method = "rf", 
                 data = datatrain,
                 preProcess = c("center", "scale"),
                 ntree = 400,
                 tuneGrid = expand.grid(mtry=c(4, 6, 8, 10, 12, 14)),                 
                 metric = "Accuracy",
                 trControl = ctrl)

saveRDS(bestFit, file = "modelos/bestFit.rds")
bestFit <- readRDS("modelos/rfFit")
windows()
plot(varImp(bestFit, scale = F))

bestProb <- predict(bestFit, datatest, type="prob")
bestPred <- as.factor(ifelse(bestProb$Yes > 0.189, "Yes", "No"))


bestCM <- confusionMatrix(bestPred, datatest$death) 
accuracy_best <- round(bestCM$overall[1], digits = 4)
kappa_best <- round(bestCM$overall[2], digits = 4)
sens_best <- 0.8709
spec_best <- 0.8030
bestROC <- roc(datatest$death ~ bestProb[, 2])
bestauc <- auc(bestROC)

plot(bestROC, col = "palevioletred1", print.thres = T)

cat(accuracy_best, kappa_best, sens_best, spec_best, bestauc)

table(bestPred, datatest$death)

ggplot(bestFit, highlight = TRUE) +
  labs(title = "Evolution of the accuracy of the improved neural networks model") +
  theme_bw()



resultados <- data.frame(
  Model = factor(c("KNN", "SVM", "DT", "RF", "XGB", "NN", "DNN", "ImprovedRF"), levels =c("KNN", "SVM", "DT", "RF", "XGB", "NN", "DNN", "ImprovedRF") ),
  Sensitivity = c(sens_knn, sens_svm, sens_dt, sens_rf, sens_xgb, sens_nn, sens_dnn, sens_best),
  Specificity = c(spec_knn, spec_svm, spec_dt, spec_rf, spec_xgb, spec_nn, spec_dnn, spec_best) 
)

windows()

ggplot(resultados, aes(x = Model, y = Sensitivity, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "Model's Sensitivity Chart", y = "Sensitivity") +
  theme_minimal()

ggplot(resultados, aes(x = Model, y = Specificity, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "Model's Specificity Chart", y = "Specificity") +
  theme_minimal()


############################### ---- 6.2. dias estancia ---- 

set.seed(123)
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 5,
                     number = 10,
                     returnResamp = "final")

#devuelvo la variable a su escala original 
datatest$Days_of_stay <- exp(datatest$Days_of_stay)-1
datatrain$Days_of_stay <- exp(datatrain$Days_of_stay)-1


#defino las funciones para calcular los errores:


mae <- function(observed, predicted) {
  mean(abs(observed - predicted))
}

mse <- function(observed, predicted) {
  mean((observed - predicted)^2)
}

rmse <- function(observed, predicted) {
  sqrt(mean((observed - predicted)^2))
}

r2 <- function(observed, predicted) {
  sst <- sum((observed - mean(observed))^2)
  sse <- sum((observed - predicted)^2)
  rsq <- 1 - (sse / sst)
  
  return(rsq)
}

#KNN
knnFit2 <- train(Days_of_stay ~ ., 
                method = "knn", 
                data = datatrain,
                preProcess = c("center", "scale"),
                tuneGrid = expand.grid(k = 1:25),
                trControl = ctrl)

saveRDS(knnFit2, "modelos/knnFit2.rds")
knnFit2 <- readRDS("modelos/knnFit2.rds")

windows()
plot(varImp(knnFit2, scale = F))

knnPred2 <- predict(knnFit2, datatest)  

mae_knn <- mae(datatest$Days_of_stay, knnPred2)
mse_knn <- mse(datatest$Days_of_stay, knnPred2)
rmse_knn <- rmse(datatest$Days_of_stay, knnPred2)
r2_knn <- r2(datatest$Days_of_stay, knnPred2)
cat(mae_knn, mse_knn, rmse_knn, r2_knn)


windows()
ggplot(knnFit2, highlight = TRUE) +
  labs(title = "Evolution of the RMSE of the kNN model") +
  theme_bw()

#svm
# svmFit2 <- train(Days_of_stay ~ ., 
#                  method = "svmLinear", 
#                  data = datatrain,
#                  preProcess = c("center", "scale"),
#                  tuneGrid = expand.grid(C = seq(0, 4, length = 10)),
#                  trControl = ctrl)
# 
# saveRDS(svmFit2, "modelos/svmFit2.rds")
# svmFit2 <- readRDS("modelos/svmFit2.rds")
# 
# windows()
# plot(varImp(svmFit2, scale = F))
# 
# svmPred2 <- predict(svmFit2, datatest)    
# 
# mae_svm <- mae(datatest$Days_of_stay, svmPred2)
# mse_svm <- mse(datatest$Days_of_stay, svmPred2)
# rmse_svm <- rmse(datatest$Days_of_stay, svmPred2)
# r2_svm <- r2(datatest$Days_of_stay, svmPred2)
# 
# ggplot(svmFit2, highlight = TRUE) +
#   labs(title = "Evolution of the accuracy of the SVM Radial model") +
#   theme_bw()



library(e1071)
svmFit2<- svm(Days_of_stay ~. , data = datatrain,
           ranges = list(epsilon = c(0.05, 0.01, 0.1), cost = 2^(seq(-3,3,.5))))
saveRDS(svmFit2, "modelos/svmFit2.rds")
svmFit2 <- readRDS("modelos/svmFit2.rds")
svmPred2 <- predict(svmFit2, datatest)    

mae_svm <- mae(datatest$Days_of_stay, svmPred2)
mse_svm <- mse(datatest$Days_of_stay, svmPred2)
rmse_svm <- rmse(datatest$Days_of_stay, svmPred2)
r2_svm <- r2(datatest$Days_of_stay, svmPred2)
cat(mae_svm, mse_svm, rmse_svm, r2_svm)

ggplot(svmFit2, highlight = TRUE) +
  labs(title = "Evolution of the accuracy of the SVM model") +
  theme_bw()


# DECISION TREES

dtFit2 <- train(Days_of_stay ~ ., 
                method = "rpart", 
                data = datatrain,
                preProcess = c("center", "scale"),
                tuneGrid = expand.grid(cp = c(0.01, 0.05, 0.1)),
                maxdepth = 10,
                trControl = ctrl)

saveRDS(dtFit2, "modelos/dtFit2.rds")
dtFit2 <- readRDS("modelos/dtFit2.rds")

windows()
plot(varImp(dtFit2, scale = F))

dtPred2 <- predict(dtFit2, datatest)    

mae_dt <- mae(datatest$Days_of_stay, dtPred2)
mse_dt <- mse(datatest$Days_of_stay, dtPred2)
rmse_dt <- rmse(datatest$Days_of_stay, dtPred2)
r2_dt <- r2(datatest$Days_of_stay, dtPred2)
cat(mae_dt, mse_dt, rmse_dt, r2_dt)


ggplot(dtFit2, highlight = TRUE) +
  labs(title = "Evolution of the RMSE of the decision tree model") +
  theme_bw()

# random forest
rfFit2 <- train(Days_of_stay ~ ., 
                method = "rf", 
                data = datatrain,
                preProcess = c("center", "scale"),
                tuneGrid = expand.grid(mtry=c(4, 6, 8, 10, 12, 14)),
                trControl = ctrl)

saveRDS(rfFit2, "modelos/rfFit2.rds")
rfFit2 <- readRDS("modelos/rfFit2.rds")

windows()
plot(varImp(rfFit2, scale = F))

rfPred2 <- predict(rfFit2, datatest)    

mae_rf <- mae(datatest$Days_of_stay, rfPred2)
mse_rf <- mse(datatest$Days_of_stay, rfPred2)
rmse_rf <- rmse(datatest$Days_of_stay, rfPred2)
r2_rf <- r2(datatest$Days_of_stay, rfPred2)
cat(mae_rf, mse_rf, rmse_rf, r2_rf)


windows()
ggplot(rfFit2, highlight = TRUE) +
  labs(title = "Evolution of the RMSE of the random forest model") +
  theme_bw()

#GRADIENT BOOSTING
xgb_grid = expand.grid(
  nrounds = c(100, 200, 300),
  eta = c(0.001, 0.01, 0.05), 
  max_depth = c(2, 4, 6),
  gamma = c(1),
  colsample_bytree = c(0.2, 0.4, 0.6),
  min_child_weight = c(1, 5),
  subsample = 1
)

xgbFit2 <- train(Days_of_stay ~ .,  
                data = datatrain,
                trControl = ctrl,
                maximize = F,
                tuneGrid = xgb_grid,
                preProcess = c("center", "scale"),
                method = "xgbTree")

saveRDS(xgbFit2, "modelos/xgbFit2.rds")
xgbFit2 <- readRDS("modelos/xgbFit2.rds")

windows()
plot(varImp(xgbFit2, scale = F))

xgbPred2 <- predict(xgbFit2, datatest)    

mae_xgb <- mae(datatest$Days_of_stay, xgbPred2)
mse_xgb <- mse(datatest$Days_of_stay, xgbPred2)
rmse_xgb <- rmse(datatest$Days_of_stay, xgbPred2)
r2_xgb <- r2(datatest$Days_of_stay, xgbPred2)
cat(mae_xgb, mse_xgb, rmse_xgb, r2_xgb)


results2 <- xgbFit2$results

# Crear un gráfico personalizado
ggplot(results2, aes(x = nrounds, y = RMSE, color = as.factor(eta))) +
  geom_point() +
  geom_line() +
  labs(title = "Evolution of the RMSE of the gradient boosting model",
       x = "nrounds",
       y = "RMSE",
       color = "eta") +
  theme_minimal()

#NEURAL NETWORKS
nnFit2 <- train(Days_of_stay ~ ., 
                method = "nnet", 
                data = datatrain,
                preProcess = c("center", "scale"), MaxNWts = 1000,
                maxit = 100,
                tuneGrid = expand.grid(size=c(12, 16, 20, 24), 
                                       decay=c(0.001, 0.01, 0.1, 0.5)),
                trControl = ctrl)

saveRDS(nnFit2, "modelos/nnFit2.rds")
nnFit2 <- readRDS("modelos/nnFit2.rds")

windows()
plot(varImp(nnFit2, scale = F))

nnPred2 <- predict(nnFit2, datatest)    

mae_nn <- mae(datatest$Days_of_stay, nnPred2)
mse_nn <- mse(datatest$Days_of_stay, nnPred2)
rmse_nn <- rmse(datatest$Days_of_stay, nnPred2)
r2_nn <- r2(datatest$Days_of_stay, nnPred2)
cat(mae_nn, mse_nn, rmse_nn, r2_nn)


ggplot(nnFit2, highlight = TRUE) +
  labs(title = "Evolution of the RMSE of the neural network model") +
  theme_bw()


# DEEP NEURAL NETWORKS

dnnFit2 <- train(Days_of_stay ~., 
                method = "dnn", 
                data = datatrain,
                preProcess = c("center", "scale"),
                numepochs = 30, # number of iterations on the whole trainset
                tuneGrid = expand.grid(layer1 = 1:4,
                                       layer2 = 0:2,
                                       layer3 = 0:2,
                                       hidden_dropout = 0, 
                                       visible_dropout = 0),
                trControl = ctrl)

saveRDS(dnnFit2, "modelos/dnnFit2.rds")
dnnFit2 <- readRDS("modelos/dnnFit2.rds")

windows()
plot(varImp(dnnFit2, scale = F))

dnnPred2 <- predict(dnnFit2, datatest)    

mae_dnn <- mae(datatest$Days_of_stay, dnnPred2)
mse_dnn <- mse(datatest$Days_of_stay, dnnPred2)
rmse_dnn <- rmse(datatest$Days_of_stay, dnnPred2)
r2_dnn <- r2(datatest$Days_of_stay, dnnPred2)
cat(mae_dnn, mse_dnn, rmse_dnn, r2_dnn)


ggplot(dnnFit2, highlight = TRUE) +
  labs(title = "Evolution of the RMSE of the deep neural network model") +
  theme_bw()

# PERFORMANCE METRICS

techniques <- factor(c("KNN", "SVM", "DT", "RF", "XGB", "NN", "DNN"), levels =c("KNN", "SVM", "DT", "RF", "XGB", "NN", "DNN"))
mae_values <- c(mae_knn, mae_svm, mae_dt, mae_rf, mae_xgb, mae_nn, mae_dnn) 
mse_values <- c(mse_knn, mse_svm, mse_dt, mse_rf, mse_xgb, mse_nn, mse_dnn)
rmse_values <- c(rmse_knn, rmse_svm, rmse_dt, rmse_rf, rmse_xgb, rmse_nn, rmse_dnn)
r2_values <- c(r2_knn, r2_svm, r2_dt, r2_rf, r2_xgb, r2_nn, r2_dnn)

df <- data.frame(
  Technique = techniques,
  MAE = mae_values,
  MSE = mse_values, 
  RMSE = rmse_values, 
  R2 = r2_values
)

library(data.table)
df_long <- reshape2::melt(df, id.vars = "Technique", variable.name = "Metric", value.name = "Value")

windows()
ggplot(df_long, aes(x = Metric, y = Value, color = Technique, group = Technique)) +
  geom_line() +
  geom_point() +
  labs(title = "Performance Metrics for Different Techniques", x = "Metric", y = "Value") +
  theme_minimal()+
  theme(
    legend.title = element_text(size = 18),   # Tamaño del título de la leyenda
    legend.text = element_text(size = 16),    # Tamaño del texto de la leyenda
    axis.title.x = element_text(size = 18),   # Tamaño del título del eje x
    axis.title.y = element_text(size = 18),   # Tamaño del título del eje y
    axis.text.x = element_text(size = 16,  angle = 45, hjust = 1),    # Tamaño del texto de los valores del eje x
    axis.text.y = element_text(size = 16))



windows()

ggplot(df, aes(x = Technique, y = RMSE, fill = Technique)) +
  geom_bar(stat = "identity") +
  labs(title = "Model's RMSE Chart", y = "RMSE") +
  theme_minimal()

ggplot(df, aes(x = Technique, y = R2, fill = Technique)) +
  geom_bar(stat = "identity") +
  labs(title = "Model's R2 Chart", y = "R2") +
  theme_minimal()

