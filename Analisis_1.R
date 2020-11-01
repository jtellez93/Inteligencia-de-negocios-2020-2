# Librerias necesarias ----------------------------------------------------
library(readxl)
library(fs)
library(dplyr)
library(purrr)

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
str(datos)
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


# identificar fechas por jerarquia

datos.1 <- datos.dep %>% 
  mutate(fecha = lubridate::date(Fecha_Hora),
        mes = lubridate::month(Fecha_Hora), 
         año = lubridate::year(Fecha_Hora),
         dia = lubridate::day(Fecha_Hora), 
         hora = lubridate::hour(Fecha_Hora),
         dia_sem = lubridate::wday(Fecha_Hora)) %>%
  group_by(dia)

summary(datos.1)


# Informacion complementaria ----------------------------------------------

# Festivos
fest <- read.csv2("./Festivos.csv", header = T)
str(fest)

fest <- fest %>%
  transform(fest, Fecha = lubridate::dmy(Fecha)) %>% 
  select(Fecha)
  
# Paros
paros <- data.frame(Fecha = lubridate::dmy("22-09-2018","10-10-2018","15-11-2018","28-11-2018"),
                    Evento = c("Dia sin carro","Paro Nal estudiantes","Marcha Nal centrales obreras",
                               "Paro Nal centrales obreras"))  
str(paros)

# Influencia del clima




# Analisis exploratorio ---------------------------------------------------
summary(datos)
summary(datos$Est_serv)


# Visualizacion





