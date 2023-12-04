#Árbol de Navidad
#Para borrar todas las variables
rm(list = ls())

library(ggplot2)

#-----
# Árbol 1
# Fuente: https://r-craft.org/christmas-tree-with-ggplot/

# create data
x <- c(8,7,6,7,6,5,6,5,4,5,4,3,4,3,2,3,2,1,0.5,0.1)

dat1 <- data.frame(x1 = 1:length(x), x2 = x)
dat2 <- data.frame(x1 = 1:length(x), x2 = -x)
dat1$xvar <- dat2$xvar <- NA
dat1$yvar <- dat2$yvar <- NA
dat1$siz <- dat2$siz <- NA
dat1$col <- dat2$col <- NA

# establecer un umbral para las bolas navideñas
dec_threshold = -0.5

#Crea lugares, tamaños y colores aleatorios para bolas navideñas.
set.seed(2512)
for (row in 1:nrow(dat1)){
  
  if (rnorm(1) > dec_threshold){
    
    dat1$xvar[row] <- row
    dat1$yvar[row] <- sample(1:dat1$x2[row]-1,1)
    dat1$siz[row] <- runif(1,0.5,1.5)
    dat1$col[row] <- sample(1:5, 1)
  }
  
  if (rnorm(1) > dec_threshold){
    
    dat2$xvar[row] <- row
    dat2$yvar[row] <- sample(1:dat2$x2[row],1)
    dat2$siz[row] <- runif(1,0.5,1.5)
    dat2$col[row] <- sample(1:5, 1)
  }
}

# Plot del árbol de navidad
ggplot() +
  geom_bar(data = dat1, aes(x=x1, y=x2),stat = "identity", fill = '#31a354') +
  geom_bar(data = dat2, aes(x=x1, y=x2),stat = "identity", fill = '#31a354') +
  geom_point(data = dat1,aes(x = xvar, y = yvar, size = siz, colour = as.factor(col)) ) +
  geom_point(data = dat2,aes(x = xvar, y = yvar, size = siz, colour = as.factor(col)) ) +
  coord_flip() + theme_minimal()+ theme(legend.position="none",
                                        axis.title.x=element_blank(),
                                        axis.text.x=element_blank(),
                                        axis.ticks.x=element_blank(),
                                        axis.title.y=element_blank(),
                                        axis.text.y=element_blank(),
                                        axis.ticks.y=element_blank()) +
  ggtitle('R-Ladies Morelia les desea una Feliz Navidad!!')

#----
# Árbol 2
# Fuente: https://www.datawim.com/post/making-merry-christmas-tree-in-r/

library(tidyverse)
library(mlbench)
library(viridis)

set.seed(12345)
# diferentes formas que uno puede crear en 2d como cuadrados
# triangulos y formas
data <- mlbench.shapes(n=100000) 
plot(data)

# Extraer triangulos data = class 3
canopy <- data %>%
  as.data.frame() %>% 
  filter(classes == "3")

# plot canopy
plot(canopy$x.x4, canopy$x.V2, col = "darkgreen", xlab = "x", ylab = "y")


y <- c(0.8, 1, 1, 0.8)  
x <- c(00.8, 00.8, 1.2, 1.2)

# plot tronco del árbol
trunk = as.data.frame(cbind(x, y))

# plot data
plot(x, y)
polygon(x, y, col = 'brown')


p1 <- ggplot() +
  geom_polygon(aes(x = x, y = y), data = trunk, fill = "brown") +
  geom_point(aes(x = x.x4, y = x.V2), shape = 3, data = canopy, colour = "darkgreen") +
  theme_void()

p1

# con hexagonos
p2 <- ggplot() +
  geom_polygon(aes(x = x, y = y), data = trunk, fill = "brown") +
  geom_hex(aes(x = x.x4, y = x.V2), data = canopy) +
  scale_fill_viridis() +
  theme_void() +
  theme(legend.position = "none")

p2

# con rectangulos
p3 <- ggplot() +
  geom_polygon(aes(x = x, y = y), data = trunk, fill = "brown") +
  geom_bin2d(aes(x = x.x4, y = x.V2), data = canopy) +
  scale_fill_viridis() +
  theme_void() +
  theme(legend.position = "none")

p3

# con densidad
p4 <- ggplot() +
  geom_polygon(aes(x = x, y = y), data = trunk, fill = "brown") +
  stat_density_2d(aes(x = x.x4, y = x.V2, fill = ..level..), data = canopy, geom = "polygon") +
  scale_fill_viridis() +
  theme_void() +
  theme(legend.position = "none")

p4


library(patchwork)

p1 + p2 + p3 + p4 + plot_annotation(
  title = 'Feliz Navidad 2023',
  theme = theme(plot.title = element_text(size = 18, hjust = 0.5)))


#----
# Tarjeta de año nuevo
# Fuente

library(plotly)
#devtools::install_github("ropensci/plotly")
rm(list = ls())

# Optiones para el plot ----
x <- 0.2
y <- 0.72
speed <- 250
nbkdrops <- 100

# ColoresColorset for plot
# Más opciones http://colorhunt.co/
cols <- c("#FFC85B", "#379956","#234C63")
ncolors <- length(cols)


# Crear función para puntos random y añadirles ruido con jitter
n <- 1000  # Cantidad de puntos

# Vectores iniciales x y y
bkdrop.x <- runif(n, min = 0, max = 1)
bkdrop.y <- runif(n, min = 0, max = 1)

# Función para añadir ruido con jitter al vector original
bkdrop <- function(n = 1000, amount = 0.005){
  # jitter añade ruido a un vector numérico
  x <- jitter(bkdrop.x, amount = amount)
  y <- jitter(bkdrop.y, amount = amount)
  
  df <- data.frame(x, y)
  
  return(df)
  
}

# Crear los fondos ----
# Cada llamada crea un data frame diferente
# El número de frames se controla con bkdrop

bkdrop.df <- data.frame()
for(i in 1:nbkdrops){
  temp <- bkdrop()
  temp <- data.frame(temp, frame = i, color = sample(1:ncolors, size = nrow(temp), replace = T))
  bkdrop.df <- rbind(bkdrop.df, temp)
  
}

# Hacer las luces ----
# Coordenadas para los rectángulos
# Los plots son plots de líneas
bklight.x <- c(0.28, 0.18, 0.48)
bklight.y <- c(0.42, 0.62, 0.65)
bklight.xend <- c(0.63, 0.50, 0.75)
bklight.yend <- c(0.42, 0.62, 0.65)

# Función para crear un dataframe que contiene coordenadas, marco y
# color de cada segmento de retroiluminación
makebklight <- function(id){
  bklight <- data.frame()
  
  for(i in 1:nbkdrops){
    temp <- data.frame(x = bklight.x[id],
                       y = bklight.y[id],
                       xend = bklight.xend[id],
                       yend = bklight.yend[id],
                       frame = i, 
                       color = sample(1:ncolors, size = 1))
    
    bklight <- rbind(bklight, temp)
  }
  
  return(bklight)
}

# Crear los marcos
bklight1 <- makebklight(1)
bklight2 <- makebklight(2)
bklight3 <- makebklight(3)

# Inicializar el color de los marcos
bklight1$color[1] <- 1
bklight2$color[1] <- 2
bklight3$color[1] <- 3

# Plot !! ----
p <- plot_ly(height = 800, width = 1024, 
             colors = cols, 
             frame = ~frame,
             x = ~x, 
             y = ~y,
             color = ~factor(color)) %>%  
  
  # Fondo
  add_markers(data = bkdrop.df, 
              opacity = 0.8,
              marker = list(symbol = "star", size = 8),
              hoverinfo = "none") %>%
  
  # Agregar segmentos (para retroiluminación)
  add_segments(data = bklight1, 
               xend = ~xend, yend = ~yend, 
               line = list(width = 150)) %>%
  
  add_segments(data = bklight2, 
               xend = ~xend, yend = ~yend, 
               line = list(width = 150)) %>% 
  
  add_segments(data = bklight3, 
               xend = ~xend, yend = ~yend, 
               line = list(width = 150)) %>% 
  
  # Opciones de animación
  # Ver https://cpsievert.github.io/plotly_book/key-frame-animations.html
  
  animation_opts(speed, easing = "linear", transition = 0) %>%
  animation_button(x = 1, xanchor = "right", y = 1, yanchor = "bottom") %>%
  animation_slider(hide = T) %>%
  
  # Layout, anotaciones y formas
  
  layout(
    showlegend = F,
    
    xaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F, range = c(0, 1)),
    yaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F, range = c(0, 1)),
    
    annotations = list(
      
      # Para los fondos
      list(xref = "paper", yref = "paper",
           xanchor = "left", yanchor = "top",
           x = x + 0.002, y = y + 0.002, 
           showarrow = F,
           text = "Happy New<br>Year !",
           font = list(size = 100, family = "Times New Roman",
                       color = "black")),
      
      list(xref = "paper", yref = "paper",
           xanchor = "left", yanchor = "top",
           x = x + 0.003, y = y + 0.003, 
           showarrow = F,
           text = "Happy New<br>Year !",
           font = list(size = 100, family = "Times New Roman",
                       color = "black")),
      
      list(xref = "paper", yref = "paper",
           xanchor = "left", yanchor = "top",
           x = x + 0.004, y = y + 0.004, 
           showarrow = F,
           text = "Happy New<br>Year !",
           font = list(size = 100, family = "Times New Roman",
                       color = "black")),
      
      # Actual
      list(xref = "paper", yref = "paper",
           xanchor = "left", yanchor = "top",
           x = x, y = y, 
           showarrow = F,
           text = "Happy New<br>Year !",
           font = list(size = 100, family = "Times New Roman",
                       color = "#ff6666"))
    ),
    
    shapes = list(
      
      # Los marcos/bordes
      list(xref = "paper", yref = "paper",
           x0 = 0, y0 = 0, 
           x1 = 1, y1 = 1,
           type = "rect",
           line = list(width = 10, color = cols[1])),
      
      list(xref = "paper", yref = "paper",
           x0 = 0.01, y0 = 0.01, 
           x1 = 0.99, y1 = 0.99,
           type = "rect",
           line = list(width = 10, color = cols[2])),
      
      list(xref = "paper", yref = "paper",
           x0 = 0.02, y0 = 0.02, 
           x1 = 0.98, y1 = 0.98,
           type = "rect",
           line = list(width = 10, color = cols[3])),
      
      # Contorno
      list(xref = "plot", yref = "plot",
           path = "
           M 0.50 0.53
           L 0.50 0.50
           L 0.18 0.50 
           L 0.18 0.73
           L 0.48, 0.73",
           type = "path",
           line = list(width = 7, color = "black")),
      
      list(xref = "plot", yref = "plot",
           path = "
           M 0.50 0.535
           L 0.48 0.535
           L 0.48 0.77
           L 0.75 0.77
           L 0.75 0.535
           Z",
           type = "path",
           line = list(width = 7, color = "black")),
      
      list(xref = "plot", yref = "plot",
           path = "
           M 0.28 0.5
           L 0.28 0.31
           L 0.63 0.31
           L 0.63 0.535",
           type = "path",
           line = list(width = 7, color = "black"))
      
    )
  )

p

