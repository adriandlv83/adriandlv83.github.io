# Cargamos las librerías que vamos a utilizar

library(tidyverse) # Librería para análisis, visualización, transformación.
library(janitor) # Librería para limpieza y organización de datos
library(skimr)   # Librería de análisis exploratorio y estadístico
library(here) # Librería para establecer rutas relativas al proyecto

# Inicializamos variables de entrada, salida, y uso de ejemplo de datos
########################################################################
set.seed(123)
raw_dir <- here("data","raw","cyclistic")
out_dir <- here("data", "processed", "cyclistic")
datasample <-  FALSE
########################################################################

# Creación de función para unificar archivos de directorios

uni_csv <- function(dir, pattern, dfname = NULL, sample = FALSE, nsample = NULL )  {
  
  # Validación de parámetros
  
  if (!dir.exists(dir)) {
    rlang::abort("El directorio no existe")
  }
  
  if (!is.null(dfname)) {
    if (!is.character(dfname) || length(dfname) != 1) {
      rlang::abort("dfname debe ser argumento cadena o NULL")
    }
  }
  
  if (!isTRUE(sample) && !identical(sample, FALSE)) {
    rlang::abort("datasample debe ser TRUE or FALSE")
  }
  
  if (isTRUE(sample)) {
    if(is.null(nsample) || !is.numeric(nsample) || length(nsample) != 1 || is.na(nsample) || nsample <= 0) {
      rlang::abort("datasample debe ser un número mayor a cero")
    }
  }
  
  # Listar archivos y comprobar existencia
  
  files <- list.files(path = dir, pattern = pattern, full.names = T)
  
  if (length(files) == 0) {
    rlang::abort("No se encontraron archivos")
  }
  
  message("Archivos encontrados:", length(files))
  
  
  # Funcion para lectura de archivos
  
  read_file_csv <- function(path) {
    message("Leyendo:", basename(path))

    df <- read_csv(path, show_col_types = FALSE, na = c("", " ")) |> 
      mutate(
        source_data = basename(path),
        started_at = ymd_hms(started_at, tz = "UTC"),
        ended_at = ymd_hms(ended_at, tz = "UTC")
      )
  }
  
  df_out <- map(files, read_file_csv) |> 
    bind_rows()
  
  if (isTRUE(sample)) {
    df_out <- df_out |> 
      slice_sample(n = min(nsample, nrow(df_out)))
  }
  
  return(df_out)
  
}

# Ejecutamos la función pasándole los argumentos de los parámetros (path,, pattern, dfname)
# parámetros (datasample y nsample solo si queremos generar un data frame del universo de los datos)

df <- uni_csv(raw_dir,"\\.csv$", "df_sample_trips", sample = datasample, nsample = 50000)


# Variables de análisis y eliminamos duraciones < 1 minuto

df_clean <- df |> 
  mutate(
    ride_length = as.numeric(round(difftime(ended_at, started_at, units = "mins"), 2)),
    month = month(started_at, abbr = FALSE),
    day_of_week = wday(started_at, week_start = 1),
    hour15min = (round((hour(started_at) * 60 + minute(started_at)) / 15) * 15) / 60,
    route = paste(start_station_id, end_station_id, sep = "-")
  ) |> 
  filter(
    !is.na(end_lat),
    ride_length >= 1
  )


# Total de viajes por tipo de usuario

summary_user <- df_clean |> 
  count(member_casual, name = "tot_trips")


# Resumen de viajes por tipo de usuario y bicicleta

summary_user_bike <- df_clean |> 
  count(member_casual, rideable_type, name = "tot_trips") |> 
  group_by(member_casual) |> 
  mutate(
    percentage = round(tot_trips / sum(tot_trips) * 100, 1)
  ) |> 
  ungroup()


# Resumen Duración promedio y mediana y percentil 90por tipo de usuario

summary_duration <- df_clean |> 
  group_by(member_casual, rideable_type) |> 
  summarise(
    avg_ride_length = round(mean(ride_length, na.rm = TRUE), 2),
    med_ride_length = round(median(ride_length, na.rm = TRUE), 2),
    p90_ride_length = round(quantile(ride_length, 0.90, na.rm = TRUE), 2),
    p99_ride_length = round(quantile(ride_length, 0.999, na.rm = TRUE), 2),
    .groups = "drop"
  )


# Resumen de viajes por estacionalidad

summary_season <- df_clean |> 
  count(month, member_casual, name = "tot_trips")


# Resumen Distribución de viajes por día de la semana

summary_week_day <- df_clean |>
  count(member_casual, day_of_week, name = "tot_trips") |> 
    mutate(
    day_name = case_when(
      day_of_week == 1 ~ "Lun",
      day_of_week == 2 ~ "Mar",
      day_of_week == 3 ~ "Mie",
      day_of_week == 4 ~ "Jue",
      day_of_week == 5 ~ "Vie",
      day_of_week == 6 ~ "Sab",
      day_of_week == 7 ~ "Dom",
    )
  )
  
# Distribución de viajes por hora del día

summary_hour_trips <- df_clean |> 
  count(member_casual, hour15min, name = "tot_trips") |>
  group_by(member_casual) |> 
  mutate(
    percentage = round(tot_trips / sum(tot_trips) * 100, 2)
  ) |> 
  ungroup()


# Análisis de Rutas.

# eliminación de valores "NA" en variables de Origen- Destino

df_clean_routs <- df_clean |> 
  mutate(round_trip = if_else(start_station_id == end_station_id, "round_trip", "one_way_trip")) |> 
  filter(
    !is.na(start_station_id),
    !is.na(end_station_id)
  )

# Resumen de Viajes redondos x tipo de usuario y bicicleta

summary_round_trip <- df_clean_routs |> 
  group_by(member_casual, round_trip, rideable_type) |> 
  summarise(
    avg_ride_length = round(mean(ride_length), 2),
    med_ride_length = round(median(ride_length), 2),
    p99 = round(quantile(ride_length, 0.99)),
    tot_trips = n(),
    .groups = "drop_last"
  )

# Resumen Top15 de rutas por tipo de usuario.

summary_top_routes <- df_clean_routs |> 
  count(member_casual, route, name = "tot_trips", sort = TRUE) |> 
  group_by(member_casual) |> 
  slice_head(n = 15) |> 
  arrange(member_casual, desc(tot_trips)) |> 
  ungroup()


# Resumen de rutas unicas 

summary_routes_uni <- df_clean_routs |> 
  count(member_casual, day_of_week, route,
        name = "tot_trips", sort = TRUE) |> 
  group_by(day_of_week, member_casual) |> 
  summarise(
    route_uni = n_distinct(route),
    tot_trips = sum(tot_trips),
    .groups = "drop"
  )




# Escrituras de resúmenes a csv

if (dir.exists(here("data", "processed", "cyclistic"))) {
  
} else {
  
  dir.create(here("data", "processed", "cyclistic"), recursive = TRUE, showWarnings = FALSE)
}

write_csv(summary_duration, here("data", "processed", "cyclistic", "summary_duration.csv"))
write_csv(summary_hour_trips, here("data", "processed", "cyclistic", "summary_hour_trips.csv"))
write_csv(summary_user, here("data", "processed", "cyclistic", "summary_user.csv"))
write_csv(summary_user_bike, here("data", "processed", "cyclistic", "summary_user_bike.csv"))
write_csv(summary_week_day, here("data", "processed", "cyclistic", "summary_week_day.csv"))
write_csv(summary_season, here("data", "processed", "cyclistic", "summary_season.csv"))
write_csv(summary_round_trip, here("data", "processed", "cyclistic", "summary_round_trip.csv"))
write_csv(summary_top_routes, here("data", "processed", "cyclistic", "summary_top_routes.csv"))
write_csv(summary_routes_uni, here("data", "processed", "cyclistic", "summary_routes_uni.csv"))

# Creación de tibble con información relevante del procesamiento

metadata <- tibble(
  type_dataset = if_else(datasample, "Sample_data", "Full_data"),
  rows_loaded = nrow(df),
  rows_deleted = nrow(df) - nrow(df_clean),
  rows_clean = nrow(df_clean),
  rows_deleted_r = nrow(df_clean) - nrow(df_clean_routs),
  rows_clean_r = nrow(df_clean_routs),
  date_process = as.character(Sys.Date())
)

write_csv(metadata, here("data", "processed", "cyclistic", "metadata.csv"))

print(metadata)
