library(dplyr)

# =====Merge de bases de datos=====

# Verificar nombres de columnas en ambos data frames
print(names(Percent_mexicans))
print(names(Votes_2020))

# Asegurarme de que 'ID' es único y está presente en ambos
if("GEO_ID" %in% names(Votes_2020) & "GEO_ID" %in% names(Percent_mexicans)) {
  print("La columna 'ID' está en ambos data frames.")
} else {
  print("La columna 'ID' no está en ambos data frames.")
}
# Asegurarme de que los tipos de datos son consistentes
Votes_2020$GEO_ID <- as.character(Votes_2020$GEO_ID)
Percent_mexicans$GEO_ID <- as.character(Percent_mexicans$GEO_ID)

# Hacer el merge con % Mexicanos
Variables_merged <- merge(Votes_2020, Percent_mexicans[, c("GEO_ID", "Percent_mexicans")], by = "GEO_ID")

# Mostrar las primeras filas del resultado
head(Variables_merged)

# Merge con el resto de las variables dependientes 
# Seleccionar la columna de interés de cada data frame
Edu_selected <- select(Edu, GEO_ID, S1501_C02_012E)
Emp_selected <- select(Emp, GEO_ID, DP03_0002PE)
Inc_selected <- select(percapita_personal_income, NAME, ppi)
Pob_selected <- select(Pob, GEO_ID, S1701_C03_001E)
Gini_selected <- select(Gini, GEO_ID, B19083_001E)
Asis_selected <- select(Asis, GEO_ID, "%Public assistance income")
Rel_selected <- select(Catholics_per_county_2020, GEO_ID, "Adherents as % of Total Population")
Rep12_selected <- select(countypres_2012, GEO_ID, repsh12)
Rur_selected <- select(Rur, GEO_ID,rur)

# Realizar el merge secuencial
Vardep_merged <- Edu_selected %>%
  inner_join(Emp_selected, by = "GEO_ID") %>%
  inner_join(Ing_selected, by = "GEO_ID") %>%
  inner_join(Pob_selected, by = "GEO_ID") %>%
  inner_join(Gini_selected, by = "GEO_ID") %>%
  inner_join(Asis_selected, by = "GEO_ID") %>%
  inner_join(Rel_selected, by ="GEO_ID")

# Merge final
Var_merged <- merge(Variables_merged, Vardep_merged, by = "GEO_ID")

#Guardé el archivo en CSV
write.csv(dt, "dt.csv", row.names = FALSE)

datos <- Var_data_v2

#Datos de 2012 y 2018
Merged12 <- merge(data, Rep12_selected, by = "GEO_ID")
Merged18 <- merge(Merged12, countypres_1980, by ="GEO_ID")
Merged18$dif1612 <- Merged18$repsh16-Merged18$repsh12
Merged18$dif2016 <- Merged18$repsh20-Merged18$repsh16
dt <- merge(Merged18, Inc_selected, by = "NAME")
dt <- merge(dt,Rur_selected, by = "GEO_ID")

stats <- dt %>%
  summarise_all(list(
    media = ~mean(.),
    desviacion_estandar = ~sd(.),
    valor_maximo = ~max(.),
    valor_minimo = ~min(.)
  ))
print(stats)
# Media, mediana, desviación estándar, etc.
summary(dt$variable_numerica)

# ====Correlaciones====

correlation1 <- cor(datos$repsh20, datos$mex2)
print(correlation1)

correlation2 <- cor(datos$repsh20, datos$edu)
print(correlation2)

correlation3 <- cor(datos$repsh20, datos$emp)
print(correlation3)

correlation4 <- cor(datos$repsh20, datos$inc)
print(correlation4)

correlation5 <- cor(datos$repsh20, datos$pov)
print(correlation5)

correlation6 <- cor(datos$repsh20, datos$gin)
print(correlation6)

correlation7 <- cor(datos$repsh20, datos$asis)
print(correlation7)

correlation8 <- cor(datos$repsh20, datos$rel)
print(correlation8)

correlation9 <- cor(datos$repsh20, datos$repsh16, use = "complete.obs")
print(correlation9)

install.packages("corrplot")
library(corrplot)

# Seleccionar las variables numéricas
numeric_vars <- dt %>%
  select(repsh20, mex, edu, emp, ppi, pov, gin, asis, rel, rur, repsh80)
# Calcular la matriz de correlación
cor_matrix <- cor(numeric_vars, use = "complete.obs")
# Visualizar la matriz de correlación
ggcorrplot(cor_matrix, method = "circle", type = "lower", lab = TRUE)
corrplot(cor_matrix, method="circle")

# ====LaTeX====

install.packages("tikzDevice")
install.packages("extrafont")

library(tikzDevice)
library(extrafont)
# Especifica la ruta completa al ejecutable de LaTeX
options(tikzLatex = "/Library/TeX/texbin/latex")

loadfonts(device = "tikz")
tikz('corrplot.tex', width = 5, height = 5)
corrplot(cor_matrix, method = "circle")
dev.off()

# =====Diagramas de dispersión====

# Diagrama de dispersión para cada par de variables

# repsh20 vs mex
plot(dt$repsh20, dt$mex2, main = "repsh20 vs mex2", xlab = "repsh20", ylab = "mex2", pch = 19, col = "red")
# repsh20 vs edu
plot(datos$repsh20, datos$edu, main = "repsh20 vs edu", xlab = "repsh20", ylab = "edu", pch = 19, col = "red")
# repsh20 vs emp
plot(datos$repsh20, datos$emp, main = "repsh20 vs emp", xlab = "repsh20", ylab = "emp", pch = 19, col = "red")
# repsh20 vs inc
plot(datos$repsh20, datos$inc, main = "repsh20 vs inc", xlab = "repsh20", ylab = "inc", pch = 19, col = "red")
# repsh20 vs pov
plot(datos$repsh20, datos$pov, main = "repsh20 vs pov", xlab = "repsh20", ylab = "pov", pch = 19, col = "red")
# repsh20 vs gin
plot(datos$repsh20, datos$gin, main = "repsh20 vs gin", xlab = "repsh20", ylab = "gin", pch = 19, col = "red")
# repsh20 vs asis
plot(datos$repsh20, datos$asis, main = "repsh20 vs asis", xlab = "repsh20", ylab = "asis", pch = 19, col = "red")
# repsh20 vs rel
plot(datos$repsh20, datos$rel, main = "repsh20 vs rel", xlab = "repsh20", ylab = "rel", pch = 19, col = "red")
# repsh20 vs repsh16
plot(datos$repsh20, datos$repsh16, main = "repsh20 vs repsh16", xlab = "repsh20", ylab = "repsh16", pch = 19, col = "red")
# repsh20 vs rur
plot(dt$repsh20, dt$rur, main = "repsh20 vs rur", xlab = "repsh20", ylab = "rur", pch=19, col="red")
# repsh20 vs ppi
plot(dt$repsh20, dt$ppi2, main = "repsh20 vs ppi2", xlab = "repsh20", ylab = "ppi2", pch=19, col="red")
# =====Regresiones=====

install.packages("ggplot2")
library(ggplot2)
library(car)

RLM1 <- lm(repsh20 ~ mex+mex2+edu+emp+ppi+pov+gin+asis+rel+repsh16+repsh80+rur, data=dt);summary(RLM1)
RLM2 <- lm(repsh20 ~ mex+mex2+edu+emp+ppi+pov+gin+asis+rel+repsh16+repsh80+exp(rur), data=dt);summary(RLM2)

# Prueba con simulaciones 

dt$mex2 <- dt$mex*dt$mex
datos$inc2 <- datos$inc*datos$inc
RLM3 <- lm(repsh20 ~ mex+edu+emp+ppi+pov+gin+asis+rel+rur, data=dt);summary(RLM3)
newd <- data.frame(mex=seq(0,1,0.01))
newd$mex2 <- newd$mex*newd$mex
newd$edu <- mean(datos$edu)
newd$emp <- mean(datos$emp)
newd$pov <- mean(datos$pov)
newd$gin <- mean(datos$gin)
newd$asis <- mean(datos$asis)
newd$rel <- mean(datos$rel)
pred <- predict.lm(object=RLM2, newdata=newd, interval = "prediction")
pred <- as.data.frame(pred)
plot(seq(0,1,0.01),pred$fit)
par(mfrow = c(1, 1))

plot(seq(0,1,0.01), pred$fit, type = "o", xlab = "Proporción de Mex", ylab = "Voto por el Partido Republicano", main = "Efecto de la Proporción de Mexicanos en el Voto Republicano")
par(family = "")

install.packages("tikzDevice")
library(tikzDevice)
tikz("coeficientes_modelo_regresion.tex", width = 7, height = 5)
plot(seq(0,1,0.01), pred$fit, type = "o", 
     xlab = "Proporción de Mex", 
     ylab = "Voto por el Partido Republicano", 
     main = "Efecto de la Proporción de Mex en el Voto Republicano")
dev.off()

coeficientes <- summary(RLM)$coefficients
write.csv(coeficientes, "coeficientes.csv")

#Otros modelos 
RLM4 <- lm(dif1612~ mex+edu+ppi+emp+pov+gin+asis+rel+rur+repsh80, data=dt);summary(RLM4)
RLM5 <- lm(dif2016~ mex+edu+ppi+ppi2+emp+pov+gin+asis+rel+repsh80, data=dt);summary(RLM5)
dt$ppi2 <- dt$ppi*dt$ppi
RLM6 <- lm(repsh16~ mex+edu+ppi+emp+pov-pov+gin+asis+rel+rur+repsh80, data=dt);summary(RLM6)

# Gráfico de los residuos del modelo
ggplot(data, aes(x = fitted(RLM1), y = resid(RLM1))) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Valores ajustados", y = "Residuos") +
  ggtitle("Gráfico de residuos")
par(mfrow = c(2, 2)) # Disposición de los gráficos en una matriz 2x2
plot(RLM1)

# Gráfico de los coeficientes del modelo
coefficients <- summary(RLM1)$coefficients
coef_df <- as.data.frame(coefficients)
coef_df$Variable <- rownames(coef_df)

ggplot(coef_df, aes(x = Variable, y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = Estimate - 1.96 * `Std. Error`, ymax = Estimate + 1.96 * `Std. Error`), width = 0.2) +
  coord_flip() +
  labs(title = "Coeficientes del Modelo de Regresión", y = "Estimación", x = "Variable") +
  theme_minimal()

# Tabla en LaTeX
install.packages("stargazer")
library(stargazer)
stargazer(RLM1, RLM2, RLM3, type = "latex",
          title = "Modelos de Regresión",
          dep.var.labels = c("M1", "M2", "M3"),
          covariate.labels = c("Determinantes del voto por Trump"),
          out = "resultados_regresiones.tex")

library(xtable)
#Prueba F
anova(RLM1,RLM2)
print(BIC)
print(xtable(BIC(RLM1,RLM2)), include.rownames = FALSE)

# Criterios de Información
AIC(RLM1, RLM2)
BIC(RLM1, RLM2)


# =====Inferencia ecológica=====

# Instalar paquetes necesarios
install.packages("ei")
install.packages("readxl")

# Cargar los paquetes
library(ei)
library(eiPack)
library(readxl)

# Mostrar las primeras filas del dataframe para entender su estructura
head(datos)

# Revisar si hay valores nulos en el dataframe
sum(is.na(datos))
# Eliminar filas con valores nulos
data <- na.omit(datos)
# Verificar nuevamente si hay valores nulos
sum(is.na(data))


# Preparar las variables agregadas (voto por republicanos y demócratas)
agg_vote <- data.frame(
  repsh20 = data$repsh20,
  demsh20 = data$demsh20
)

# Preparar las variables independientes (por ejemplo, proporción de población mexicana, educación, empleo)
ind_vars <- data.frame(
  mex = data$mex,
  edu = data$edu,
  emp = data$emp,
  inc = data$inc,
  pov = data$pov,
  gin = data$gin,
  asis = data$asis,
  rel = data$rel
)

# Asegurarse de que no haya problemas de dimensiones
print(dim(agg_vote))
print(dim(ind_vars))

# Ajustar el modelo de inferencia ecológica usando el paquete ei
ei_model <- ei(repsh20 + demsh20 ~ mex + edu + emp + inc + pov + gin + asis + rel, data = data)

# Resumen de los resultados
summary(ei_model)

# ====Regresión Logística====

install.packages("recipes")
library(recipes)
install.packages("caret")
library(caret)

# Convertir a dataframe
data1 <- as.data.frame(dt)

# Crear una variable dependiente binaria (por ejemplo, 1 si repsh20 > 0.5)
data1$repsh20_bin <- ifelse(data1$repsh20 > 0.5, 1, 0)

# Convertir la variable dependiente a factor
data1$repsh20_bin <- as.factor(data1$repsh20_bin)

# Ajustar el modelo de regresión logística
logistic_model <- glm(repsh20_bin ~ mex +mex2 + edu + emp + ppi + pov + gin + asis + rel + repsh16 +repsh80 + rur, data = data1, family = binomial)

# Resumen del modelo
summary(logistic_model)

# Instalar y cargar el paquete pROC para evaluar el modelo
install.packages("pROC")
library(pROC)

# Predecir probabilidades
predicted_probs <- predict(logistic_model, type = "response")
print(predicted_probs)

# Asegurarse de que no haya valores faltantes en las variables utilizadas
data1_complete <- data1[complete.cases(data1$repsh20_bin, predicted_probs), ]
umbral <- 1
data1_complete <- data1[rowSums(is.na(data1)) <= umbral, ]

# Recalcular las probabilidades predichas para los casos completos
predicted_probs_clean <- predict(logistic_model, newdata = data1_complete, type = "response")

# Asegurarse de que ambas variables son factores antes de la matriz de confusión
data1_complete$repsh20_bin <- as.factor(data1_complete$repsh20_bin)


# Curva ROC
roc_curve <- roc(data1_complete$repsh20_bin, predicted_probs_clean)

# Plot de la curva ROC
plot(roc_curve)
library(tikzDevice)
tikz("roc_curve.tex", width = 7, height = 5)
plot(seq(0,1,0.01), pred$fit, type = "o", 
     xlab = "Specificity", 
     ylab = "Sensitivity", 
     main = "Curva ROC")
dev.off()
tikz("curva_roc.tex", width = 7, height = 5)
plot(roc_curve, main = "Curva ROC", col = "#1f77b4", lwd = 2, 
     xlab = "Tasa de Falsos Positivos", ylab = "Tasa de Verdaderos Positivos")
dev.off()

# Área bajo la curva (AUC)
auc(roc_curve)

# Agregar las probabilidades predichas al dataframe original
data1$predicted_probs <- predicted_probs_clean

# Convertir probabilidades a predicciones binarias
data1$predicted_class <- ifelse(data1$predicted_probs > 0.5, 1, 0)

# Contar el número de condados predichos para votar republicano y no republicano
table(data1$predicted_class)

# Crear un gráfico de barras de las predicciones
ggplot(data1, aes(x = as.factor(predicted_class))) +
  geom_bar(fill = "red") +
  labs(title = "Predicción del voto por condado",
       x = "Predicción de voto (0 = No Republicano, 1 = Republicano)",
       y = "Número de condados")

# Asegurarse de que ambas variables son factores
data1$repsh20_bin <- as.factor(data1$repsh20_bin)
data1$predicted_class <- as.factor(data1$predicted_class)

# Matriz de confusión
library(caret)
library(xtable)
confusionMatrix(data1$predicted_class, data1$repsh20_bin)
# Crear la matriz de confusión
conf_matrix <- confusionMatrix(data = data1$predicted_class, reference = data1$repsh20_bin)
# Convertir la tabla de la matriz de confusión a una tabla LaTeX
conf_matrix_table <- xtable(conf_matrix$table, caption = "Matriz de Confusión", label = "tab:conf_matrix")

# Imprimir la tabla LaTeX
print(conf_matrix_table, include.rownames = FALSE)
# Extraer la tabla de la matriz de confusión
conf_matrix_table <- as.table(conf_matrix$table)

# Extraer las estadísticas
conf_matrix_stats <- conf_matrix$overall
conf_matrix_byclass <- conf_matrix$byClass

# Crear una lista con todas las estadísticas
stats_list <- c(conf_matrix_stats, conf_matrix_byclass)
stats_df <- as.data.frame(t(stats_list))

# Convertir la tabla de la matriz de confusión y las estadísticas a LaTeX
conf_matrix_xtable <- xtable(conf_matrix_table, caption = "Matriz de Confusión", label = "tab:conf_matrix")
stats_xtable <- xtable(stats_df, caption = "Estadísticas de la Matriz de Confusión", label = "tab:conf_matrix_stats")

# Guardar las tablas en archivos .tex
print(conf_matrix_xtable, type = "latex", file = "conf_matrix.tex")
print(stats_xtable, type = "latex", file = "conf_matrix_stats.tex")
# ===== Mapa =====

install.packages("sf")
install.packages("tmap")
library(sf)
library(tmap)

install.packages("tigris")
library(tigris)

# Obtener los datos geoespaciales de los condados
counties <- counties(cb = TRUE, resolution = "500k")  # Usar una resolución más alta

# Verificar la estructura de los datos geoespaciales
str(counties)

# Asegurar que los identificadores de condado coincidan en ambos conjuntos de datos
data$GEO_ID <- as.character(data$GEO_ID)
counties$GEOID <- as.character(counties$GEOID)

# Verificar si todos los GEOID en counties están en data
missing_ids <- setdiff(counties$GEOID, data$GEO_ID)
print(missing_ids)  # Verificar si hay identificadores faltantes

# Unir los datos
map_data <- merge(counties, data, by.x = "GEOID", by.y = "GEO_ID")

# Verificar la cantidad de registros en map_data
print(nrow(map_data))
print(nrow(counties))
print(nrow(data))

# Crear el mapa de calor con ggplot2
ggplot(data = map_data) +
  geom_sf(aes(fill = repsh20)) +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(title = "Mapa de Calor del Voto Republicano en 2020",
       fill = "Voto Republicano (%)")

# Mostrar el mapa
print(ggplot(data = map_data) +
        geom_sf(aes(fill = repsh20)) +
        scale_fill_viridis_c(option = "plasma") +
        theme_minimal() +
        labs(title = "Mapa de Calor del Voto Republicano en 2020",
             fill = "Voto Republicano (%)"))

#====Evaluación de modelos====

# Suponiendo que ya tienes tus modelos entrenados y tus predicciones
# model_lm: modelo de regresión lineal múltiple
# model_logistic: modelo de regresión logística (usado como modelo de regresión)
# model_rf: modelo de random forest
# y_true: valores reales
# y_pred_lm: predicciones del modelo de regresión lineal
# y_pred_logistic: predicciones del modelo de regresión logística
# y_pred_rf: predicciones del modelo random forest

# Instalar y cargar paquetes necesarios
install.packages("Metrics")
library(Metrics)

# Entrenar el modelo de regresión lineal múltiple
RLM1 <- lm(repsh20 ~ mex + mex2 + edu + emp + ppi + pov + gin + asis + rel + repsh16 + repsh80 + rur, data=dt)

# Obtener predicciones
pred_lm <- predict(RLM1, newdata=dt)
# Obtener predicciones
pred_logistic <- predict(logistic_model, newdata=dt, type="response")
pred_logistic_binary <- ifelse(pred_logistic > 0.5, 1, 0)  # Convertir probabilidades a clases binarias
pred_rf <- predict(rf_model, newdata=dt)

# Definir los valores reales
y_true <- dt$repsh20

# Calcular las métricas para regresión lineal múltiple
mse_lm <- mse(y_true, pred_lm)
rmse_lm <- rmse(y_true, pred_lm)
mae_lm <- mae(y_true, pred_lm)
r2_lm <- cor(y_true, pred_lm)^2


# Calcular las métricas para regresión logística
mse_logistic <- mse(y_true, y_pred_logistic)
rmse_logistic <- rmse(y_true, y_pred_logistic)
mae_logistic <- mae(y_true, y_pred_logistic)
r2_logistic <- cor(y_true, y_pred_logistic)^2

# Calcular las métricas para random forest
mse_rf <- mse(y_true, pred_rf)
rmse_rf <- rmse(y_true, pred_rf)
mae_rf <- mae(y_true, pred_rf)
r2_rf <- cor(y_true, pred_rf)^2

# Mostrar los resultados
cat("Regresión Lineal:\n")
cat("MSE:", mse_lm, "\n")
cat("RMSE:", rmse_lm, "\n")
cat("MAE:", mae_lm, "\n")
cat("R^2:", r2_lm, "\n\n")

cat("Regresión Logística:\n")
cat("MSE:", mse_logistic, "\n")
cat("RMSE:", rmse_logistic, "\n")
cat("MAE:", mae_logistic, "\n")
cat("R^2:", r2_logistic, "\n\n")

cat("Random Forest:\n")
cat("MSE:", mse_rf, "\n")
cat("RMSE:", rmse_rf, "\n")
cat("MAE:", mae_rf, "\n")
cat("R^2:", r2_rf, "\n")

