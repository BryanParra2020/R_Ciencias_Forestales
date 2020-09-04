#Importando los datos

#El siguiente algoritmo aplica una relacion lineal a una base de datos que almacena el costo de x especie maderable en funcion de la cantidad demandada en metros cubicos, con el fin de predecir a futuro el precio, costo o valor de la materia prima de acuerdo a la cantidad demandada

library(readxl)
tabla <- read_excel ("C:/Users/Administrador/Downloads/Regreslineal.xlsx", sheet = 2,col_names = TRUE, col_types = NULL, na = "", skip = 0)
View(tabla)

#Modelo de Regresion lineal
lmod <- lm (Costos ~ Metros_Cubicos, data = tabla) #primero Y dependiente de X
summary (lmod)
coe <- coef (lmod)#Muestra el valor de los coeficientes calculados 
b = coe [1] #punto de corte
m = coe [2] #pendiente

#Grafico de dispersion
win.graph () 
plot (Costos ~ Metros_Cubicos, data = tabla, main = "Metros Cubicos vs Costos", ylab = "Costos", xlab = "Cantidad (m3)", col = "blue")
abline (lmod) 

#Aplicando el modelo de regresion para 200 metros cubicos y estimar el costo  
# y = mx + b
x = 200
Costos = m*x + b
print (Costos)

points (x, Costos, pch = 20, col= "red") # Graficando la estimacion, como primer argumento coordenada x, como segundo argumento coordenada y, phc = anchura, col=color  


