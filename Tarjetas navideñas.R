

# Buscando códigos para hacer tarjetas navideñas

### Una manera muy sencilla de hacer tarjetas animadas es usando el paquete christmas


install.packages("christmas")

library(christmas)

### En la opción de ayuda encontraremos los diferentes tipos de tarjetas animadas que nos ofrece este paquete.

`help(package = 'christmas')`

#Por aquí les dejo el código de las que me parecieron más bonitas. #En él pueden modificar el año y el idioma.

xmasepitree(year = 2024, seed = NULL)

xmasgalton(year = 2024,language = "spanish",balls = 240, layers = 15,
           onlyBoard = FALSE, treeballs = 15,time = 0.02,seed = NULL)

xmasregression(year = 2024, language = "spanish", time = 2)


#En el siguiente código pueden modificar la forma del árbol #(columnar,oval, piramidal, round)


xmastreeshape(year = 2024, shape = "piramidal", nballs = 15,                      ballscolor = c("sienna2", "yellow2", "tomato"), seed =              1111) #Casi todas están bonitas, aquí va la última. 
xmashealth(year = 2024, seed = NULL)

#Una última y la más divertida
xmascaganer(year = 2024, language = "spanish", seed = NULL)

##Autor del paquete.

person(given = "Jose", family = "Barrera-Gomez", role = c("aut", "cre"),
                  email = "jose.barrera@isglobal.org",
                  comment = c(ORCID = "0000-0002-2688-6036"))
                  






