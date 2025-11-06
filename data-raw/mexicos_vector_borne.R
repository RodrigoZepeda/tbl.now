dengue_files <- list.files("~/Downloads/dengue/", full.names = T, pattern = "*.csv")
dengue <- read_csv(dengue_files, id = "file")
dengue <- dengue |>
  select(ID_REGISTRO, SEXO, ENTIDAD_UM_NOTIF, FECHA_ACTUALIZACION, FECHA_SIGN_SINTOMAS) |>
  mutate(update = if_else(str_detect(FECHA_ACTUALIZACION, "-"), ymd(FECHA_ACTUALIZACION), dmy(FECHA_ACTUALIZACION))) |>
  mutate(symptoms = if_else(str_detect(FECHA_SIGN_SINTOMAS, "-"), ymd(FECHA_SIGN_SINTOMAS), dmy(FECHA_SIGN_SINTOMAS)))

dengue <- dengue |>
  group_by(ID_REGISTRO) |>
  mutate(MIN_REGISTRO = min(update)) |>
  ungroup() |>
  rename(CLAVE_ENTIDAD = ENTIDAD_UM_NOTIF)

dengue <- dengue |>
  mutate(CLAVE_ENTIDAD = if_else(nchar(CLAVE_ENTIDAD) == 1, paste0("0", CLAVE_ENTIDAD), CLAVE_ENTIDAD)) |>
  mutate(ENTIDAD_FEDERATIVA = case_when(
    CLAVE_ENTIDAD == "01" ~ "AGUASCALIENTES",
    CLAVE_ENTIDAD == "02" ~ "BAJA CALIFORNIA",
    CLAVE_ENTIDAD == "03" ~ "BAJA CALIFORNIA SUR",
    CLAVE_ENTIDAD == "04" ~ "CAMPECHE",
    CLAVE_ENTIDAD == "05" ~ "COAHUILA DE ZARAGOZA",
    CLAVE_ENTIDAD == "06" ~ "COLIMA",
    CLAVE_ENTIDAD == "07" ~ "CHIAPAS",
    CLAVE_ENTIDAD == "08" ~ "CHIHUAHUA",
    CLAVE_ENTIDAD == "09" ~ "CIUDAD DE MÉXICO",
    CLAVE_ENTIDAD == "10" ~ "DURANGO",
    CLAVE_ENTIDAD == "11" ~ "GUANAJUATO",
    CLAVE_ENTIDAD == "12" ~ "GUERRERO",
    CLAVE_ENTIDAD == "13" ~ "HIDALGO",
    CLAVE_ENTIDAD == "14" ~ "JALISCO",
    CLAVE_ENTIDAD == "15" ~ "MÉXICO",
    CLAVE_ENTIDAD == "16" ~ "MICHOACÁN DE OCAMPO",
    CLAVE_ENTIDAD == "17" ~ "MORELOS",
    CLAVE_ENTIDAD == "18" ~ "NAYARIT",
    CLAVE_ENTIDAD == "19" ~ "NUEVO LEÓN",
    CLAVE_ENTIDAD == "20" ~ "OAXACA",
    CLAVE_ENTIDAD == "21" ~ "PUEBLA",
    CLAVE_ENTIDAD == "22" ~ "QUERÉTARO",
    CLAVE_ENTIDAD == "23" ~ "QUINTANA ROO",
    CLAVE_ENTIDAD == "24" ~ "SAN LUIS POTOSÍ",
    CLAVE_ENTIDAD == "25" ~ "SINALOA",
    CLAVE_ENTIDAD == "26" ~ "SONORA",
    CLAVE_ENTIDAD == "27" ~ "TABASCO",
    CLAVE_ENTIDAD == "28" ~ "TAMAULIPAS",
    CLAVE_ENTIDAD == "29" ~ "TLAXCALA",
    CLAVE_ENTIDAD == "30" ~ "VERACRUZ",
    CLAVE_ENTIDAD == "31" ~ "YUCATÁN",
    CLAVE_ENTIDAD == "32" ~ "ZACATECAS",
    CLAVE_ENTIDAD == "33" ~ "UNITED STATES OF AMERICA",
    CLAVE_ENTIDAD == "34" ~ "OTHER COUNTRIES IN LATINAMERICA",
    CLAVE_ENTIDAD == "35" ~ "OTHER COUNTRIES",
    CLAVE_ENTIDAD == "97" ~ "DOES NOT APPLY",
    CLAVE_ENTIDAD == "98" ~ "UNKNOWN",
    CLAVE_ENTIDAD == "99" ~ "NOT SPECIFIED",
    TRUE ~ NA_character_
  ))

etv_dat <- dengue |>
  count(SEXO, update, symptoms, ENTIDAD_FEDERATIVA) |>
  filter(ENTIDAD_FEDERATIVA %in% c("GUERRERO", "VERACRUZ", "TABASCO")) |>
  rename(state = ENTIDAD_FEDERATIVA) |>
  rename(sex = SEXO) |>
  mutate(sex = if_else(sex == 1, "MALE", "FEMALE"))

save(etv_dat, file = "data/etvdat.rda")
