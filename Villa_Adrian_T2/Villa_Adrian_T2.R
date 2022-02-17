
## Adrian Villa ##

  ## Tarea 2 ##

## 3. Elabora un programa en R que usando funciones calcule, a partir de una matriz de adyacencia 
## (Solo utiliza R base para resolver este problema).

MA <- matrix (rep (c (0,1), times = 10), ncol = 5, nrow = 5) ## Generar la matriz con "matix", y rellenar con "rep", esto para no hacer el innput manual, fijar el número de renglones y columnas.
MA ## Ver la matrix

rownames (MA) <- LETTERS [1:5] ## Cambiar nombres de renglones.
colnames (MA) <- LETTERS [1:5] ## Cambiar nombres de columnas.
MA ## Ver la matriz con los cambios. 

Red <- function (Red) { ## Definir función
  if (MA [1,2] == MA [2,1] & MA [1,3] == MA [3,1] & MA [1,4] == MA [4,1] & MA [1,5] == MA [5,1] & MA [2,3] == MA [3,2] & MA [2,4] == MA [4,2] & MA [2,5] == MA [5,2] & MA [3,4] == MA [4,3] & MA [3,5] == MA [5,3] & MA [4,5] == MA [5,4]) { ## Definir todas las posibles interacciones entre cada nodo
    print ("No Dirigida") 
  }else{
    print ("Dirigida")
  }
}

Red () ## La red NO es dirigida.

Weighted <- function (Wighted) { ## Definir función
  if (any (MA > 1) == T) { ## Si cualquier valor es mayor a 1 en la matriz.
    print ("Es pesada") ## Si hay valor mayor a 1.
  }else{ 
    print ("No es pesada") ## Si no hay valor mayor a 1.
  }
}

Weighted() ## No es pesada.

Degree <- function (Degree) { ## Definir función. 
  MA <- ifelse (MA > 1,1, MA)
  DG <- rowSums (MA[1:5,]) ## Sumar los valores para el Degree de los nodos.
  
}

Degree ()
View(Degree()) ## Tuve problemas para ver el Degree con la función, pero con "View" sí me permitió verlo.

hist (Degree(), xlab = "Degree", ylab = "Frecuencia", col = "lightblue", main = "Distribución de Conexiones") ## Los valores son 2 o 3 en la suma, por lo que hay solo dos columnas


## 4. A partir de la red de interacción de proteínas alojadas en la librería igraphdata que puedes llamar mediante data (yeast). Elabora un programa en R (acá sí puedes usar librerías especializadas de R)

library (igraph) ## Cargar librería con las funciones de igraph.
library (igraphdata) ## Cargar la librería con las bases de datos para usar en igraph.
data ("yeast") ## Base de datos a usar.

DC <- degree (yeast) ## Calcular la distribución de las conexiones.
print (DC) ## Ver la distribución. En un inicio, los datos no nos dicen mucho tal cual son dados.

hist (DC, xlab = "Distribución", ylab = "Frecuencia", col = "lightblue", main = "Distribución de Conexiones") ## Visualizar los resultados con un histograma.

boxplot (DC)

Mayores <- 0 ## Declaro mi vector vacío.

for (i in DC) { ## Proporciones en objeto DC.
  if (i >= 15) { ## Si es mayor o igual a 15.
    Mayores <- Mayores + 1 ## Suma 1 al vector vacío.
    Total <- (Mayores / gorder (yeast))*100 ## Para calcular el %, nodos con más de 15 conexiones sobre el total de nodos en yeast, multiplicado por 100.
  }else{ ## Si no.
    Mayores <- Mayores + 0 ## No hagas nada.
  }
}

print (Total)

max (DC) ## Ver cuál es valor máximo dentro de los Degrees de la base de datos.

diameter (yeast) 

Datos <- yeast ## Para no destruir la base de datos, nombro un nuevo objeto con la misma información.

Mayores10 <- sort (degree (Datos), decreasing = TRUE) [1:10] ## Poner en un nuevo objeto los 10 nodos más conectados.
Mayores10 ## Visualizar

DM <- c () ## Declarar un vector vacío. 

for (i in 1:10) { ## Para los objetos del 1 al 10
  DM [i] <- diameter (Datos) ## Guarda como objeto el diámetro en la base de datos.
  Damos <- delete_vertices (Datos, sample (Mayores10 [i])) ## Elimina los nodos más grandes de la red.
}

print (DM) ## Visualizar.

closeness (yeast)
sort (closeness (yeast), decreasing = TRUE) [1:10] ## 10 nodos más importantes 

centr_degree (yeast)
sort (closeness (yeast), decreasing = TRUE) [1:10] ## 10 nodos más importantes

estimate_closeness (yeast)
sort (estimate_closeness (yeast), decreasing = TRUE) [1:10] ## No funcionó "cutoff" sin valor por omisión.

## Método con el algoritmo de Leiden.

Leiden <- cluster_leiden (yeast) ## Cluster usando algoritmo de Leiden.
Leiden ## Imprimir para ver cuántos grupos fueron formados. 
table (membership (Leiden)) ## No estoy seguro cómo funciona esta algoritmo, pero literalmente cada nodo es un cluster. (2617 grupos)


## Método de Louvain. Comunidad estructural multi-nivel.
Louvain <- cluster_louvain (yeast) ## Imprimir para ver cuántos grupos fueron formados.
Louvain ## Con este método de cluster se formaron 114 grupos.

