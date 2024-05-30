

install.packages("extrafont")
install.packages("tikzDevice")
library(extrafont)
library(tikzDevice)


# ===== Random Forest ====

# Instalar los paquetes necesarios
install.packages("randomForest")
install.packages("caret")

# Cargar los paquetes
library(randomForest)
library(caret)

# Suponiendo que tu dataset se llama 'data'
# Seleccionar las variables independientes y la variable dependiente
# Variable dependiente: repsh20
# Variables independientes: edu, emp, mex, mex2, pov, gin, asis, rur, rel, ppi, repsh80
data <- read.csv("path/to/your/dataset.csv")  # Ajusta el path al archivo adecuado si está en formato CSV

# Dividir los datos en conjunto de entrenamiento y prueba
set.seed(42)  # Para reproducibilidad
trainIndex <- createDataPartition(dt$repsh20, p = .7, 
                                  list = FALSE, 
                                  times = 1)
dataTrain <- dt[ trainIndex,]
dataTest  <- dt[-trainIndex,]
dataTrain <- na.omit(dataTrain)
dataTest <- na.omit(dataTest)

# Ajustar el modelo de Random Forest
rf_model <- randomForest(repsh20 ~ edu + emp + mex + mex2 + pov + gin + asis + rur + rel + ppi + repsh80, 
                         data = dataTrain, 
                         importance = TRUE, 
                         ntree = 500)

# Imprimir el modelo
print(rf_model)

# Evaluar el modelo
predictions <- predict(rf_model, newdata = dataTest)
mse <- mean((dataTest$repsh20 - predictions)^2)
print(paste("Mean Squared Error:", mse))

# Importancia de las variables
importance(rf_model)
importance_labels <- rownames(importance(rf_model))
escaped_labels <- gsub("%", "\\\\%", importance_labels)
tikz("varImpPlot.tex", standAlone = TRUE, width = 6, height = 4)
varImpPlot(rf_model, main = "Importancia de Variables", names = escaped_labels)
dev.off()


# Ajustar las etiquetas en el gráfico
imp <- importance(rf_model)
labels <- rownames(imp)
escaped_labels <- gsub("%", "\\\\%", labels)

# Vuelvo a generar el gráfico usando funciones de bajo nivel para ajustar etiquetas
dotchart(imp[order(imp[, 1]), 1], labels = escaped_labels[order(imp[, 1])], xlab = "Mean Decrease in Accuracy", main = "Variable Importance")
dev.off()

###

# Gráfico de error vs número de árboles
plot(rf_model, main = "Gráfico de error vs número de árboles")

# Instalar y cargar el paquete randomForestExplainer
install.packages("randomForestExplainer")
library(randomForestExplainer)

# Extraer y visualizar uno de los árboles
tree <- getTree(rf_model, k = 1, labelVar = TRUE)
print(tree)

# Instalar y cargar el paquete pdp
install.packages("pdp")
library(pdp)

# Partial Dependence Plot para una variable importante
pdp_edu <- partial(rf_model, pred.var = "edu", grid.resolution = 100)
plotPartial(pdp_edu)

#
tikz("pdp_edu.tex", width = 7, height = 5)
plotPartial(pdp_edu, main = "Partial Dependence Plot para 'edu'", xlab = "edu", ylab = "Dependencia Parcial")
dev.off()
#

# Hacer predicciones en el conjunto de prueba
predictions <- predict(rf_model, newdata = dataTest)
mse_test <- mean((dataTest$repsh20 - predictions)^2)
r_squared_test <- 1 - sum((dataTest$repsh20 - predictions)^2) / sum((dataTest$repsh20 - mean(dataTest$repsh20))^2)

print(paste("Mean Squared Error (Test):", mse_test))
print(paste("R² (Test):", r_squared_test))

# Comparar con un modelo de regresión lineal
lm_model <- lm(repsh20 ~ edu + emp + mex + mex2 + pov + gin + asis + rur + rel + ppi + repsh80, data = dataTrain)
lm_predictions <- predict(lm_model, newdata = dataTest)
lm_mse <- mean((dataTest$repsh20 - lm_predictions)^2)
lm_r_squared <- summary(lm_model)$r.squared

print(paste("Linear Model MSE:", lm_mse))
print(paste("Linear Model R²:", lm_r_squared))


