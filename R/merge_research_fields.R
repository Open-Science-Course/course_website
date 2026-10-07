# Merge habitat / organism / field from research_fields.csv into student_stats.xlsx
# by name + year, then strip names from both files.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
})

norm_name <- function(x) {
  x <- iconv(as.character(x), to = "ASCII//TRANSLIT")
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9 ]", " ", x, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}

last_token <- function(x) {
  toks <- unique(strsplit(norm_name(x), " ", fixed = TRUE)[[1]])
  if (!length(toks)) return(NA_character_)
  toks[[length(toks)]]
}

manual_map <- tibble::tribble(
  ~year, ~fields_name, ~student_name,
  2022, "April McKay", "April Irene Riderbo Mckay",
  2022, "Jaume A. Badia-Boher", "Jaume-Adrià Badia Boher",
  2022, "Morgane Kerdoncuff", "Morgane Alizee Kerdoncuf",
  2022, "Stanislau Krasouski", "Stanislav Krasovsky",
  2022, "Samariddin Barotov", "Samariddin Bartov",
  2022, "Jonathan von Oppen", "Jonathan Von Oppen",
  2023, "Mika H. Kirkhus", "Mika Kirkhus",
  2023, "Nadine Arzt", "Nadine Michaela Arzt",
  2023, "Hedda Ørbæk", "Hedda Barfod Ørbæk",
  2023, "Sørine Gerlich", "Hannah Sørine Gerlich",
  2023, "Lucie Lelotte", "Lelotte Lucie",
  2023, "Beatrice Trascau", "Beatrice Maria Trascau",
  2023, "Unknown (2023 fishing-cat slide)", "Kitipat Phosri",
  2024, "Freya Coursey", "Freya Josephine Coursey",
  2024, "Peter Farsund", "Peter Groth Farsund",
  2024, "Saria Sato-Bajracharya", "Saria Sato Bajracharya",
  2024, "Merle Scheiner", "Merle Alice Scheiner",
  2024, "Mari A. Fjelldal", "Mari Fjelldal",
  2024, "Unknown (2024 rodent infection slide)", "Ana Martinez-Checa Guiote",
  2025, "Gard W. Gravdal", "Gard Westrum Gravdal",
  2025, "Adele Mant", "Celestine Adelmant",
  2025, "Cecilie Iden Nilse", "Cecilie Iden Nilsen",
  2025, "Unknown (2025 soil microbiome slide)", "Eirik Ottesen Hovland"
)

classify_research <- function(text) {
  t <- tolower(paste(as.character(text), collapse = " "))
  if (!nzchar(trimws(t)) || t %in% c("na", "nil", "-", "n/a")) {
    return(c(habitat = "NA", organism = "NA", field = "NA"))
  }

  habitat <- "NA"
  if (grepl("marine|aquatic|freshwater|lake|pond|wetland|salmon|fish|hydropower|ocean|\\bsea\\b", t)) {
    habitat <- "Aquatic"
  } else if (grepl("terrestrial|forest|tundra|alpine|arctic|heath|soil|plant|bat|bird|bee|graz|vegetation|shrub|meadow|wood|deer|salamander|frog", t)) {
    habitat <- "Terrestrial"
  }

  organism <- "NA"
  if (grepl("bat|bird|bee|insect|arthropod|mammal|animal|wildlife|predator|prey|rodent|cattle|deer|salamander|frog|fish|salmon|raptor|earthworm|pollinat|invertebrate", t)) {
    organism <- "Animals"
  } else if (grepl("fungi|fungal|mycorrhiz|lichen|mushroom", t)) {
    organism <- "Fungi"
  } else if (grepl("microb|bacteria|bacterial", t)) {
    organism <- "Microbes"
  } else if (grepl("plant|vegetation|shrub|tree|forest|heath|botan|cloudberry|forb|grass|phenolog", t)) {
    organism <- "Plants"
  }

  field <- "NA"
  if (grepl("genom|genetic|\\bgene\\b", t)) {
    field <- "Genetics"
  } else if (grepl("behavio", t)) {
    field <- "Behavioural ecology"
  } else if (grepl("population|demograph|abundance|survival", t)) {
    field <- "Population ecology"
  } else if (grepl("community|species interaction|species diversity", t)) {
    field <- "Community ecology"
  } else if (grepl("ecosystem|carbon|nutrient|flux|decomposition", t)) {
    field <- "Ecosystem ecology"
  } else if (grepl("conserv|invasive|restoration|management|conflict|nature.based|protected area|wind turbine|land.?use", t)) {
    field <- "Conservation ecology"
  } else if (grepl("remote sensing|database|monitor|modelling|modeling|statistical|machine learning", t)) {
    field <- "Biodiversity informatics"
  }

  c(habitat = habitat, organism = organism, field = field)
}

students <- read_excel("data/student_stats.xlsx")
students <- students[, !grepl("^\\.\\.\\.", names(students), perl = TRUE)]
students <- students |>
  mutate(
    Course = as.integer(Course),
    Name = as.character(Name),
    Research = as.character(Research),
    s_key = norm_name(Name),
    s_last = vapply(Name, last_token, character(1))
  )

fields <- read.csv(
  "data/research_fields.csv",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA"),
  check.names = FALSE
) |>
  mutate(
    year = as.integer(year),
    name = as.character(name),
    f_key = norm_name(name),
    f_last = vapply(name, last_token, character(1)),
    habitat = ifelse(is.na(habitat), "NA", as.character(habitat)),
    organism = ifelse(is.na(organism), "NA", as.character(organism)),
    field = ifelse(is.na(field), "NA", as.character(field))
  )

# Manual
by_manual <- manual_map |>
  left_join(
    fields |> select(year, name, habitat, organism, field),
    by = c("year", "fields_name" = "name")
  ) |>
  transmute(
    Course = year,
    Name = student_name,
    habitat, organism, field,
    source = "manual"
  )

# Exact normalized key
by_exact <- students |>
  anti_join(by_manual, by = c("Course", "Name")) |>
  inner_join(
    fields |> select(year, f_key, habitat, organism, field),
    by = c("Course" = "year", "s_key" = "f_key")
  ) |>
  transmute(Course, Name, habitat, organism, field, source = "exact")

matched <- bind_rows(by_manual, by_exact)

# Unique last name within year
remaining <- students |> anti_join(matched, by = c("Course", "Name"))
fields_left <- fields |>
  filter(!name %in% manual_map$fields_name) |>
  filter(!f_key %in% students$s_key[students$Name %in% by_exact$Name])

uniq_s <- remaining |> count(Course, s_last) |> filter(n == 1)
uniq_f <- fields_left |> count(year, f_last) |> filter(n == 1)
by_last <- remaining |>
  inner_join(uniq_s |> select(Course, s_last), by = c("Course", "s_last")) |>
  inner_join(
    uniq_f |>
      select(year, f_last) |>
      inner_join(
        fields_left |> select(year, f_last, habitat, organism, field),
        by = c("year", "f_last")
      ),
    by = c("Course" = "year", "s_last" = "f_last")
  ) |>
  transmute(Course, Name, habitat, organism, field, source = "lastname")

matched <- bind_rows(matched, by_last) |>
  distinct(Course, Name, .keep_all = TRUE)

out <- students |>
  left_join(matched, by = c("Course", "Name"))

Habitat <- character(nrow(out))
Organism <- character(nrow(out))
Field <- character(nrow(out))
source <- character(nrow(out))

for (i in seq_len(nrow(out))) {
  if (!is.na(out$source[[i]])) {
    Habitat[[i]] <- out$habitat[[i]]
    Organism[[i]] <- out$organism[[i]]
    Field[[i]] <- out$field[[i]]
    source[[i]] <- out$source[[i]]
  } else {
    cls <- classify_research(out$Research[[i]])
    Habitat[[i]] <- cls[["habitat"]]
    Organism[[i]] <- cls[["organism"]]
    Field[[i]] <- cls[["field"]]
    source[[i]] <- if (all(cls == "NA")) "na" else "research_text"
  }
}

out$Habitat <- Habitat
out$Organism <- Organism
out$Field <- Field
out$source <- source

cat("Match sources by year:\n")
print(table(out$source, out$Course))

cat("\nStill fully NA:\n")
print(
  out |>
    filter(Habitat == "NA", Organism == "NA", Field == "NA") |>
    select(Course, Name, Research),
  n = 50
)

out_students <- out |>
  transmute(
    Course,
    Country,
    Degree,
    Research,
    Habitat,
    Organism,
    Field
  )

if (!requireNamespace("writexl", quietly = TRUE)) {
  install.packages("writexl", repos = "https://cloud.r-project.org")
}
writexl::write_xlsx(out_students, "data/student_stats.xlsx")
cat("\nWrote data/student_stats.xlsx (", nrow(out_students), " rows) without names.\n")

out_fields <- fields |>
  transmute(year, topic, habitat, organism, field)
write.csv(out_fields, "data/research_fields.csv", row.names = FALSE, na = "NA", quote = TRUE)
cat("Wrote data/research_fields.csv without names.\n")
