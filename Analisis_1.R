
# Librerias necesarias ----------------------------------------------------
library(readxl)
library(fs)
library(dplyr)
library(purrr)
library(leaflet)
library(ggplot2)

library(sf)

# Carga de datos ----------------------------------------------------------
dir_ls(regexp = "xlsx")  
# Retorna nombre de archivos en la ruta del dir de trabajo
# regexp indica el tipo de archivo

excel_sheets("Mayo-2018-a-Mayo-2019.xlsx") 
# Extrae el nombre de las hojas del archivo Excel

# creo una funcion para leer todas las hojas de un archivo excel
leer_multiple_excel <- function(x) {
  x %>%
    excel_sheets() %>% 
    set_names() %>% 
    purrr::map_df(read_excel, path = x)
}

# aplico la funcion a todos los archivos y agrupo en un solo data.frame
 datos <- fs::dir_ls(regexp = "xlsx") %>%
  purrr::map(leer_multiple_excel) %>%
  dplyr::bind_rows()


# Limpieza y transformacion -----------------------------------------------
colnames(datos) <- c("Latitud", "Longitud", "Fecha_Hora", "Est_serv")
datos <- transform(datos,
                   Est_serv = factor(Est_serv)
)
str(datos)


# deteccion de atipicos
# Retirar NA
good <- complete.cases(datos)
datos <- datos[good,]
str(datos)

# acotar ubicacion delimimtando un rectangulo que contenga la zona de interes
# punto 1: (3.567873, -76.626525)
# punto 2: (3.567873, -76.441328)
# punto 3: (3.299807, -76.632459)
# punto 4: (3.299807, -76.441328)

Latitud_rango <- (datos$Latitud > 3.299807 & datos$Latitud < 3.567873)
datos.dep <- datos[Latitud_rango, ]

Longitud_rango <- (datos.dep$Longitud > -76.632459 & datos.dep$Longitud < -76.441328)
datos.dep <- datos.dep[Longitud_rango, ]

summary(datos.dep)
head(datos.dep, n = 10)
tail(datos.dep, n = 10)

write.csv2(datos.dep, file = "./datos_dep.csv", row.names = FALSE)

# identificar fechas por jerarquia
datos.dep <- read.csv2("./datos_dep.csv", header = T, sep = ";")
datos.dep <- datos.dep[,-1]

datos.1 <- datos.dep %>% 
  mutate(fecha = lubridate::date(Fecha_Hora),
        mes = lubridate::month(Fecha_Hora, label = T, abbr = T), 
         año = lubridate::year(Fecha_Hora),
         dia = lubridate::day(Fecha_Hora), 
         hora = lubridate::hour(Fecha_Hora),
         dia_sem = lubridate::wday(Fecha_Hora,
                                   week_start = getOption("lubridate.week.start", 1),
                                   label = T, abbr = F))

datos.1 <- transform(datos.1,
                     Fecha_Hora = lubridate::ymd_hms(Fecha_Hora),
                     Est_serv = as.factor(Est_serv)
                     )

summary(datos.1)


# Informacion complementaria ----------------------------------------------

# Festivos
fest <- read.csv2("./Festivos.csv", header = T)
str(fest)

fest <- fest %>%
  transform(fest, Fecha = lubridate::dmy(Fecha)) %>% 
  select(Fecha) %>% mutate(evento = "festivo")

colnames(fest) <- c("fecha", "evento")
  
# Paros
paros <- data.frame(fecha = lubridate::dmy("22-09-2018","10-10-2018","15-11-2018","28-11-2018"),
                    evento = c("Dia sin carro","Paro Nal estudiantes","Marcha Nal centrales obreras",
                               "Paro Nal centrales obreras"))  
str(paros)

# Influencia del clima

inf_clima <- data.frame(fecha = lubridate::dmy("21/03/2019", "2/4/2019",
              "19/09/2019", "30/09/2019", "1/10/2019", "16/10/2019", 
              "14/01/2018", "2/03/2018", "30/03/2018", "3/04/2018", 
              "4/04/2018", "17/04/2018", "6/05/2018", "23/05/2018", 
              "28/05/2018", "29/10/2018"), evento = c("lluvia"))

# Eventos total

eventos <- read_excel("./Eventos.xlsx")
eventos <- transform(eventos, fecha = lubridate::dmy(fecha))

eventos <- bind_rows(eventos, fest)

str(eventos)


# Analisis exploratorio ---------------------------------------------------
summary(datos.1)
summary(datos.1$Est_serv)


# Visualizacion -----------------------------------------------------------
# parametros visualizacion
x <- datos.1 %>%
  dplyr::filter(año == 2019
                ,mes == "sept"
                ,dia == 1
                )
# Mapa
mapa <- leaflet() %>%
  addTiles() %>%  # Añade por defecto los Tiles de  OpenStreetMap
  addMarkers(lng=x$Longitud, lat=x$Latitud 
             ,clusterOptions = markerClusterOptions()
  )
mapa


# Preguntas ---------------------------------------------------------------
# ¿Cuándo hay mayor incidencia de servicios tipo P? (Pendiente)
# ¿A qué se puede deber? ¿Qué pasó en la ciudad en esos momentos?

# Total por mes
ggplot(data = datos.1 %>%
         dplyr::filter(Est_serv == "P")) +
  geom_bar(mapping = aes(x = mes)) + theme_bw()

# Total mes por año
ggplot(data = datos.1 %>%
         dplyr::filter(Est_serv == "P")) +
  geom_bar(mapping = aes(x = mes)) + 
  facet_grid(~año) + theme_bw()

# Total dia_sem por año
ggplot(data = datos.1 %>%
         dplyr::filter(Est_serv == "P")) +
  geom_bar(mapping = aes(x = dia_sem)) + 
  facet_grid(~año) + theme_bw()

# Total hora por año
ggplot(data = datos.1 %>%
         dplyr::filter(Est_serv == "P")) +
  geom_bar(mapping = aes(x = hora)) + 
  facet_grid(~año) + theme_bw()

# ¿Qué días de la semana hay más demanda?
# Total por dia_sem
ggplot(data = datos.1 %>%
         dplyr::filter(Est_serv %in% c("A", "C", "D", "F",
                                       "I", "M", "N", "P", "S"))) +
  geom_bar(mapping = aes(x = dia_sem)) + theme_bw()

# Total por dia_sem por año
ggplot(data = datos.1 %>%
         dplyr::filter(Est_serv %in% c("A", "C", "D", "F",
                                       "I", "M", "N", "P", "S"))) +
  geom_bar(mapping = aes(x = dia_sem)) + 
  facet_grid(~año) + theme_bw()



# ¿En qué horas de los días (entre semana) hay más y menos demanda?
# Total por hora
ggplot(data = datos.1 %>%
         dplyr::filter(Est_serv %in% c("A", "C", "D", "F",
                                       "I", "M", "N", "P", "S"),
                       dia_sem %in% c("lunes","martes","miércoles","jueves","viernes"))) +
  geom_bar(mapping = aes(x = hora)) + theme_bw()

# Total hora por dia_sem
ggplot(data = datos.1 %>%
         dplyr::filter(Est_serv %in% c("A", "C", "D", "F",
                                       "I", "M", "N", "P", "S"),
                       dia_sem %in% c("lunes","martes","miércoles","jueves","viernes"))) +
  geom_bar(mapping = aes(x = hora)) + 
  facet_grid(~dia_sem) + theme_bw()



# ¿La demanda cambia cuando el día es festivo?
datos.2 <- left_join(datos.1, eventos, by = "fecha")

dia_norm <- datos.1[,c(4,10)]
  
# Promedio dias normales 
dia_norm <- count(datos.2 %>% filter(is.na(evento)), c("dia_sem","fecha"))
dia_norm <- plyr::ddply(dia_norm,~dia_sem,summarise,mean=mean(freq))


# Promedio dias festivos 
dia_fest <- count(datos.2 %>% filter(evento == "festivo"), c("dia_sem","fecha"))
dia_fest <- plyr::ddply(dia_fest,~dia_sem,summarise,mean=mean(freq))
dia_fest <- bind_rows(dia_fest, data.frame(dia_sem = "domingo", mean =0))

comparacion <- left_join(dia_norm, dia_fest, by = "dia_sem")

comparacion <- transform(comparacion, 
                         dia_sem = factor(dia_sem, labels = c("lunes","martes","miércoles", 
                                                              "jueves","viernes", 
                                                              "sábado","domingo"), levels = c(1,2,3,4,5,6,7)))

# Total por dia_sem
ggplot(data = comparacion) +
geom_point(mapping = aes(x = dia_sem, y = mean.x, colour = "Dia normal")) +
geom_point(mapping = aes(x = dia_sem, y = mean.y, colour = "Dia festivo")) +
  labs(x = "Dias",
       y = "Promedio",
       color = "Legend") + 
  theme_bw()


# ¿La demanda en la ciudad cambia en épocas especiales? (Semana Santa,
# vacaciones, Feria de Cali)



# Algunos conductores reciben más servicios que otros. ¿Cómo decidir dónde
# ubicar pistas?
 

 




