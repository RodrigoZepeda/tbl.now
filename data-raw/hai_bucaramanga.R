# hai_bucaramanga -------------------------------------------------------------
#
# Healthcare-associated infections (IAAS -- "Infecciones Asociadas a la Atencion
# en Salud") notified in the municipality of Bucaramanga, Santander, Colombia.
#
# Source: Colombian open-data portal, dataset 48 of the Secretaria de Salud y
#   Ambiente de Bucaramanga.
#   https://www.datos.gov.co/Salud-y-Protecci-n-Social/48-Infecciones-asociadas-a-la-atenci-n-en-salud-IA/w4zx-wbff/about_data
#
# The source file is a Spanish-language line list with 1,423 records and 36
# columns. This script translates it to English, trims it to the columns that
# matter for a nowcasting/delay analysis, and repairs the encoding quirks
# documented below. Run with:  source("data-raw/hai_bucaramanga.R")
#
# KNOWN DEFECTS IN THE SOURCE (deliberately preserved, not silently repaired):
#
#   * `1900-01-01` is an undocumented missing-date sentinel. It affects 22.3% of
#     `fecha examen`, 83.9% of `fecha recepcion` and 41.0% of `fecha
#     expedicion`. It is converted to `NA` here, because leaving it in produces
#     delays of roughly -45,000 days.
#   * `fecha recepcion` is only populated for 229 records (16.1%) and stops
#     at 2019-12-01, so the three-date chain exists only for the early years.
#   * 88 records (10.7% of otherwise-valid pairs) have `report_date` BEFORE
#     `specimen_date`, by as much as 331 days. These are kept as recorded --
#     they are a real property of this source and users must decide how to
#     handle them.
#   * For the 314 clinically-confirmed cases the literal string "CONFIRMADO POR
#     CLINICA" was written into `muestra`, `nombre prueba` AND `microorganismo`.
#     Those are set to `NA`; the information survives in `case_type`.
#   * "NO DISPONIBLE" (not available) is likewise mapped to `NA`.
#   * `tipo uci` contains the typo "NEONATLA" (2 records) for "NEONATAL".
#   * 100 records are EXACT duplicates: byte-identical across all 36 source
#     columns, including the `Orden` autonumber that the data dictionary
#     describes as unique. All 1,423 rows are shipped as-is (fidelity to the
#     source); `dplyr::distinct()` drops the 100 second copies.
#
# DROPPED COLUMNS: `barrio vereda` (dropped at the maintainer's request), and --
#   per the "aggressive trim" -- `control` (a single real level), `semana` /
#   `ano` (derivable from the dates, and `semana` is 0 for 49 records),
#   `Curso de vida` (a coarsening of `Grupo Etario`), `tipo seguridad social`,
#   `estrato` (53% are the -89 sentinel), the four geography columns, and all
#   twelve `grupo *` special-population flags (four are constant).

library(dplyr)

source_csv <- file.path(
  "data-raw",
  paste0(
    "48._Infecciones_asociadas_a_la_atención_en_salud_IAAS_en_el_",
    "Municipio_de_Bucaramanga_20260817.csv"
  )
)

# `encoding = "UTF-8"` (not `fileEncoding`) -- the latter aborts the read at the
# first "n-tilde" on a UTF-8 platform.
raw <- utils::read.csv(
  source_csv,
  colClasses = "character", check.names = FALSE, encoding = "UTF-8"
)
stopifnot(nrow(raw) == 1423L)

# -- helpers -------------------------------------------------------------------

# Strip Spanish diacritics so every lookup key below can stay pure ASCII.
# Written with \u escapes on purpose: comparing a UTF-8-marked string from
# read.csv() against an accented literal in this file is encoding-dependent and
# fails on some platforms.
fold_accents <- function(x) {
  chartr(
    paste0(
      "\u00c1\u00c9\u00cd\u00d3\u00da\u00dc\u00d1",
      "\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1"
    ),
    "AEIOUUNaeiouun",
    x
  )
}

# Trim, collapse internal whitespace, fold accents, and map the source's several
# spellings of "missing" onto NA.
clean_chr <- function(x) {
  x <- fold_accents(trimws(gsub("[[:space:]]+", " ", x)))
  x[x %in% c("", "NA", "SIN INFORMACION",
             "NO DISPONIBLE", "CONFIRMADO POR CLINICA")] <- NA_character_
  x
}

# Parse "YYYY-MM-DDT00:00:00.000" and drop the 1900-01-01 sentinel.
parse_date <- function(x) {
  out <- as.Date(substr(x, 1L, 10L))
  out[!is.na(out) & out <= as.Date("1901-01-01")] <- NA
  out
}

# Recode via an explicit lookup, erroring on any level not in the map. This is
# deliberately strict: a silent NA here would be a translation bug.
recode_strict <- function(x, map, what) {
  unknown <- setdiff(stats::na.omit(unique(x)), names(map))
  if (length(unknown)) {
    stop("Untranslated ", what, " level(s): ", paste(unknown, collapse = ", "))
  }
  unname(ifelse(is.na(x), NA_character_, map[x]))
}

# -- translation tables --------------------------------------------------------

map_sex <- c(MASCULINO = "Male", FEMENINO = "Female")

map_age <- c(
  "MENOR DE 1" = "<1", "1 A 4" = "1-4", "5 A 9" = "5-9", "10 A 14" = "10-14",
  "15 A 19" = "15-19", "20 A 29" = "20-29", "30 A 39" = "30-39",
  "40 A 49" = "40-49", "50 A 59" = "50-59", "60 A 69" = "60-69",
  "70 Y MAS" = "70+"
)
age_levels <- unname(map_age)

map_specimen <- c(
  "SANGRE TOTAL" = "Whole blood",
  "ORINA" = "Urine",
  "SECRECIONES" = "Secretions",
  "OTROS LIQUIDOS ESTERILES" = "Other sterile fluids",
  "ESPUTO" = "Sputum",
  "LAVADO BRONCOALVEOLAR" = "Bronchoalveolar lavage"
)

map_test <- c(
  "HEMOCULTIVO" = "Blood culture",
  "UROCULTIVO" = "Urine culture",
  "CULTIVO DE SECRESIONES RESPIRATORIAS" = "Respiratory secretion culture",
  "CULTIVO DEL PARENQUIMA PULMONAR" = "Lung parenchyma culture",
  "LAVADO BRONCOALVEOLAR" = "Bronchoalveolar lavage",
  "CULTIVO" = "Culture",
  "CULTIVO DE LIQUIDO PLEURAL" = "Pleural fluid culture"
)

map_case_type <- c(
  "CONFIRMADO POR LABORATORIO" = "Laboratory-confirmed",
  "CONFIRMADO POR CLINICA" = "Clinically-confirmed"
)

map_condition <- c(VIVO = "Alive", MUERTO = "Dead")

map_icu <- c(
  ADULTO = "Adult", PEDIATRICA = "Paediatric",
  NEONATAL = "Neonatal", NEONATLA = "Neonatal"  # source typo
)

# Every one of the 91 observed `microorganismo` values, mapped explicitly to
# conventional binomial capitalisation. Spanish connectives are translated,
# obvious source typos ("STAPHILOCOCCUS", "STENOTROPHOMONA") are corrected, and
# non-specific entries become "<Genus> spp.".
map_microorganism <- c(
  "KLEBSIELLA PNEUMONIAE" = "Klebsiella pneumoniae",
  "PSEUDOMONAS AERUGINOSA" = "Pseudomonas aeruginosa",
  "ESCHERICHIA COLI" = "Escherichia coli",
  "STAPHYLOCOCCUS EPIDERMIDIS" = "Staphylococcus epidermidis",
  "SERRATIA MARCESCENS" = "Serratia marcescens",
  "CANDIDA ALBICANS" = "Candida albicans",
  "ENTEROCOCCUS FAECALIS" = "Enterococcus faecalis",
  "ENTEROBACTER CLOACAE" = "Enterobacter cloacae",
  "ACINETOBACTER BAUMANNII" = "Acinetobacter baumannii",
  "CANDIDA TROPICALIS" = "Candida tropicalis",
  "ENTEROCOCCUS FAECIUM" = "Enterococcus faecium",
  "PROTEUS MIRABILIS" = "Proteus mirabilis",
  "STAPHYLOCOCCUS HAEMOLYTICUS" = "Staphylococcus haemolyticus",
  "STAPHYLOCOCCUS AUREUS" = "Staphylococcus aureus",
  "STAPHYLOCOCCUS HOMINIS" = "Staphylococcus hominis",
  "CANDIDA PARAPSILOSIS" = "Candida parapsilosis",
  "STENOTROPHOMONAS MALTOPHILIA" = "Stenotrophomonas maltophilia",
  "KLEBSIELLA OXYTOCA" = "Klebsiella oxytoca",
  "KLEBSIELLA AEROGENES" = "Klebsiella aerogenes",
  "MORGANELLA MORGANII" = "Morganella morganii",
  "KLEBSIELLA PNEUMONIAE SSP PNEU" = "Klebsiella pneumoniae subsp. pneumoniae",
  "ENTEROBACTER AEROGENES" = "Enterobacter aerogenes",
  "STAPHYLOCOCCUS SAPROPHYTICUS" = "Staphylococcus saprophyticus",
  "CANDIDA GLABRATA" = "Candida glabrata",
  "PSEUDOMONAS PUTIDA" = "Pseudomonas putida",
  "STAPHYLOCOCCUS COAGULASA NEGATIVO" = "Staphylococcus, coagulase-negative",
  "CITROBACTER FREUNDII" = "Citrobacter freundii",
  "ACINETOBACTER CALCOACETICUS-BAUMANNII COMPLEX" =
    "Acinetobacter calcoaceticus-baumannii complex",
  "BURKHOLDERIA CEPACIA" = "Burkholderia cepacia",
  "CANDIDA GUILLIERMONDII" = "Candida guilliermondii",
  "KLEBSIELLA OZAENAE" = "Klebsiella ozaenae",
  "PROVIDENCIA RETTGERI" = "Providencia rettgeri",
  "PSEUDOMONAS FLUORESCENS" = "Pseudomonas fluorescens",
  "STREPTOCOCCUS PNEUMONIAE" = "Streptococcus pneumoniae",
  "TRICHOSPORON ASAHII" = "Trichosporon asahii",
  "ACHROMOBACTER" = "Achromobacter spp.",
  "AEROMONAS VERONII SOBRIA" = "Aeromonas veronii bv. sobria",
  "BURKHOLDERIA CEPACIA COMPLEX" = "Burkholderia cepacia complex",
  "CANDIDA" = "Candida spp.",
  "CANDIDA FAMATA" = "Candida famata",
  "CANDIDA HAEMULONII" = "Candida haemulonii",
  "CANDIDA KRUSEI" = "Candida krusei",
  "CITROBACTER FARMERI" = "Citrobacter farmeri",
  "CORYNEBACTERIUM JEIKEIUM" = "Corynebacterium jeikeium",
  "ENTEROBACTER CLOACAE DISSOLVENS" = "Enterobacter cloacae subsp. dissolvens",
  "KLEBSIELLA PNEUMONIAE PNEUMONIAE" = "Klebsiella pneumoniae subsp. pneumoniae",
  "KLUYVERA ASCORBATA" = "Kluyvera ascorbata",
  "MICROCOCCUS LUTEUS" = "Micrococcus luteus",
  "STAPHYLOCOCCUS EPIDERMIDIS ALBUS" = "Staphylococcus epidermidis (albus)",
  "STAPHYLOCOCCUS SCHLEIFERI" = "Staphylococcus schleiferi",
  "STREPTOCOCCUS DYSGALACTIAE" = "Streptococcus dysgalactiae",
  "STREPTOCOCCUS GALLOLYTICUS GALLOLYTICUS" =
    "Streptococcus gallolyticus subsp. gallolyticus",
  "STREPTOCOCCUS INTERMEDIUS" = "Streptococcus intermedius",
  "ACINETOBACTER HAEMOLYTICUS" = "Acinetobacter haemolyticus",
  "ACINETOBACTER LWOFFII" = "Acinetobacter lwoffii",
  "AEROCOCCUS VIRIDANS" = "Aerococcus viridans",
  "ALCALIGENES FAECALIS" = "Alcaligenes faecalis",
  "ALLOIOCOCCUS OTITIS" = "Alloiococcus otitis",
  "CANDIDA LUSITANIAE" = "Candida lusitaniae",
  "CANDIDA SPP" = "Candida spp.",
  "CANDIDA TROPICALIS (SUCROSE NEGATIVE)" =
    "Candida tropicalis (sucrose-negative)",
  "CITROBACTER YOUNGAE" = "Citrobacter youngae",
  "ELASTASE-PRODUCING STRAIN STAPHYLOCOCCUS EPIDERMIDIS" =
    "Staphylococcus epidermidis (elastase-producing strain)",
  "ENTEROBACTER ASBURIAE" = "Enterobacter asburiae",
  "ESCHERICHIA COLI 0157:H7" = "Escherichia coli O157:H7",
  "HAEMOPHILUS HAEMOLYTICUS" = "Haemophilus haemolyticus",
  "HAFNIA ALVEI" = "Hafnia alvei",
  "KLEBSIELLA PNEUMONIAE/OXYTOCA" = "Klebsiella pneumoniae/oxytoca",
  "KLEBSIELLA SPP" = "Klebsiella spp.",
  "MICROCOCCUS Y ESPECIES RELACIONADAS" = "Micrococcus and related species",
  "MULTIDRUG RESISTANT KLEBSIELLA PNEUMONIAE" =
    "Klebsiella pneumoniae (multidrug-resistant)",
  "MULTIDRUG RESISTANT PSEUDOMONAS AERUGINOSA" =
    "Pseudomonas aeruginosa (multidrug-resistant)",
  "PANTOEA" = "Pantoea spp.",
  "PROVIDENCIA STUARTII" = "Providencia stuartii",
  "PSEUDOMONAS" = "Pseudomonas spp.",
  "PSEUDOMONAS AERUGINOSA Y STENOTROPHOMONA MALTOPHILIA" =
    "Pseudomonas aeruginosa and Stenotrophomonas maltophilia",
  "PSEUDOMONAS MALTOPHILIA" = "Pseudomonas maltophilia",
  "PSEUDOMONAS ORYZIHABITANS" = "Pseudomonas oryzihabitans",
  "PSEUDOMONAS TESTOSTERONI" = "Pseudomonas testosteroni",
  "RAOULTELLA (K.) ORNITHINOLYTICA" = "Raoultella ornithinolytica",
  "RAOULTELLA ORNITHINOLYTICA" = "Raoultella ornithinolytica",
  "SERRATIA" = "Serratia spp.",
  "SPHINGOMONAS PAUCIMOBILIS" = "Sphingomonas paucimobilis",
  "STAPHILOCOCCUS EPIDERMIDIS" = "Staphylococcus epidermidis",
  "STAPHYLOCOCCUS CAPITIS" = "Staphylococcus capitis",
  "STAPHYLOCOCCUS CAPRAE" = "Staphylococcus caprae",
  "STAPHYLOCOCCUS COHNII SUBSPECIES UREALYT" =
    "Staphylococcus cohnii subsp. urealyticus",
  "STREPTOCOCCUS AGALACTIAE" = "Streptococcus agalactiae",
  "STREPTOCOCCUS, GROUP C" = "Streptococcus, group C"
)

# -- build ---------------------------------------------------------------------

hai_bucaramanga <- tibble::tibble(
  id            = as.integer(raw[["Orden"]]),
  specimen_date = parse_date(raw[["fecha examen"]]),
  received_date = parse_date(raw[["fecha recepcion"]]),
  report_date   = parse_date(raw[["fecha expedicion"]]),
  specimen      = clean_chr(raw[["muestra"]]),
  test          = clean_chr(raw[["nombre prueba"]]),
  microorganism = clean_chr(raw[["microorganismo"]]),
  sex           = clean_chr(raw[["sexo"]]),
  age_group     = clean_chr(raw[["Grupo Etario"]]),
  case_type     = trimws(raw[["tipo caso"]]),
  final_condition = clean_chr(raw[["condicion final"]]),
  icu_type      = clean_chr(raw[["tipo uci"]]),
  institution   = as.integer(raw[["institucion"]])
) |>
  mutate(
    specimen        = factor(recode_strict(specimen, map_specimen, "specimen")),
    test            = factor(recode_strict(test, map_test, "test")),
    microorganism   = recode_strict(microorganism, map_microorganism,
                                    "microorganism"),
    sex             = factor(recode_strict(sex, map_sex, "sex")),
    age_group       = factor(recode_strict(age_group, map_age, "age_group"),
                             levels = age_levels, ordered = TRUE),
    case_type       = factor(recode_strict(case_type, map_case_type,
                                           "case_type")),
    final_condition = factor(recode_strict(final_condition, map_condition,
                                           "final_condition")),
    icu_type        = factor(recode_strict(icu_type, map_icu, "icu_type"))
  )

# -- sanity checks -------------------------------------------------------------

stopifnot(
  nrow(hai_bucaramanga) == 1423L,
  !anyNA(hai_bucaramanga$id),
  # 100 exact duplicates are expected and deliberately retained (see above).
  sum(duplicated(hai_bucaramanga)) == 100L,
  sum(duplicated(hai_bucaramanga$id)) == 100L,
  sum(!is.na(hai_bucaramanga$specimen_date)) == 1105L,
  sum(!is.na(hai_bucaramanga$received_date)) == 229L,
  sum(!is.na(hai_bucaramanga$report_date)) == 840L,
  # The 314 clinically-confirmed records must have lost their fake specimen /
  # test / organism values, and only those.
  sum(is.na(hai_bucaramanga$microorganism)) == 320L,
  nlevels(hai_bucaramanga$icu_type) == 3L
)

delay <- as.numeric(hai_bucaramanga$report_date - hai_bucaramanga$specimen_date)
stopifnot(sum(!is.na(delay)) == 826L, sum(delay < 0, na.rm = TRUE) == 88L)

message(
  "hai_bucaramanga: ", nrow(hai_bucaramanga), " rows x ",
  ncol(hai_bucaramanga), " cols | ",
  sum(!is.na(delay) & delay >= 0), " rows usable for a specimen -> report ",
  "nowcast | median delay ", stats::median(delay[!is.na(delay) & delay >= 0]),
  " days"
)

hai_bucaramanga <- hai_bucaramanga |>
  filter(is.na(specimen_date) | specimen_date >= ymd("2020/01/01"))

usethis::use_data(hai_bucaramanga, overwrite = TRUE, compress = "xz")
