library(tidyverse)

covid <- read_csv("~/Downloads/covid19_sisver_cdmx_completa.csv")
covid_summary <- covid |>
  filter(resultado_definitivo == "SARS-CoV-2") |>
  # mutate(mun_residencia = if_else(entidad_residencia != "CIUDAD DE MEXICO", "OUT OF STATE", mun_residencia)) |>
  # mutate(mun_residencia = if_else(mun_residencia %in%
  #                                   c("ENSENADA", "JESUS MARIA", "ATIZAPAN DE ZARAGOZA", "IGUALA DE LA INDEPENDENCIA"), "OUT OF STATE", mun_residencia)) |>
  # mutate(mun_residencia = replace_na(mun_residencia, "UNKNOWN")) |>
  filter(fecha_registro <= ymd("2023/01/01")) |>
  group_by(fecha_registro, fecha_inicio_sintomas, sexo) |>
  tally() |>
  ungroup() |>
  mutate(fecha_registro = as.Date(fecha_registro)) |>
  # mutate(fecha_ingreso = as.Date(fecha_ingreso)) |>
  mutate(fecha_inicio_sintomas = as.Date(fecha_inicio_sintomas)) |>
  rename(date_of_registry = fecha_registro) |>
  rename(date_of_symptom_onset = fecha_inicio_sintomas) |>
  # rename(date_of_admission = fecha_ingreso) |>
  rename(sex = sexo) |>
  # rename(neighbourhood = mun_residencia) |>
  mutate(sex = case_when(
    sex == "MASCULINO" ~ "MALE",
    sex == "FEMENINO" ~ "FEMALE",
    TRUE ~ sex
  ))

covidat <- covid_summary
save(covidat, file = "data/covidat.rda")
#load("data/covidat.rda")
#covidat# <- covidat |> filter(year(date_of_symptom_onset) > 2020)
