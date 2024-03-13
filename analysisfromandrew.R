pacman::p_load(tidyverse, plotly, lubridate, rprojroot, stringr, sf, tidyr, dplyr, ggthemes, viridis)

#reading in the total summary dataset
arthropod <- read_csv("Galiano_terrestrial_arthropods_review_summary_2023-10-21.csv")
#splitting into data that has data for the first observed date (musueum specimens most likely)
first.observed <- arthropod %>%
  drop_na(First.Observed) %>%
  select(Taxon, First.Observed)%>%
  mutate(full_date = First.Observed)
#reformatting date
first.observed$First.Observed <- format(first.observed$First.Observed, "%Y/%m")

#splitting into data has data for the collected reported date (inat observations?)
collected.reported <- arthropod %>%
  drop_na(Collected.Reported..y.m.d.) %>%
  select(Taxon, Collected.Reported..y.m.d.) %>%
  rename(First.Observed = Collected.Reported..y.m.d.) %>%
  mutate(First.Observed = as.Date(First.Observed))%>%
  mutate(full_date = First.Observed)
#reformatting date
collected.reported$First.Observed <- format(collected.reported$First.Observed, "%Y/%m")

#glueing these  datasets together again for the full analysis
combined <- rbind(first.observed, collected.reported) %>% 
  group_by(Taxon) %>% filter(First.Observed == min(First.Observed)) %>%
  ungroup() %>%
  mutate(date_final = format(lubridate::ymd(full_date), '%Y-%m-%d'),
         year = lubridate::year(date_final), 
         month = lubridate::month(date_final))


#plotting the freqency of first observed data
#combined_df <- as.data.frame(table(combined$First.Observed))
#combined_df %>%
  #plot_ly(y = ~Freq, x=~Var1, type="scatter")

split_biodiv_year <- function(df, x, y) {
 split <- df %>% filter(between(year,x,y))
 cum.obs <- nrow(split) # cumulative no. observations
 cum.spp <- length(unique(split$Taxon)) # cumulative no. species
 biodiv_calcs <- data.frame(cum.obs, cum.spp) 
 biodiv_calcs
}

y.1925.1935 <- split_biodiv(combined, 1925,1935) %>% mutate(range= "1925-1935")
y.1925.1945 <- split_biodiv(combined, 1925,1945) %>% mutate(range= "1925-1945")
y.1925.1955 <- split_biodiv(combined, 1925,1955) %>% mutate(range= "1925-1955")
y.1925.1965 <- split_biodiv(combined, 1925,1965) %>% mutate(range= "1925-1965")
y.1925.1975 <- split_biodiv(combined, 1925,1975) %>% mutate(range= "1925-1975")
y.1925.1985 <- split_biodiv(combined, 1925,1985) %>% mutate(range= "1925-1985")
y.1925.1995 <- split_biodiv(combined, 1925,1995) %>% mutate(range= "1925-1995")
y.1925.2005 <- split_biodiv(combined, 1925,2005) %>% mutate(range= "1925-2005")
y.1925.2015 <- split_biodiv(combined, 1925,2015) %>% mutate(range= "1925-2015")
y.1925.2025 <- split_biodiv(combined, 1925,2025) %>% mutate(range= "1925-2025")


summary_year <- rbind(y.1925.1935, y.1925.1945, y.1925.1955, y.1925.1965, 
                 y.1925.1975, y.1925.1985, y.1925.1995, y.1925.2005, 
                 y.1925.2025)


summary %>%
  plot_ly(y = ~cum.spp, x=~range, type="scatter", mode="line")

split_biodiv_month <- function(df, x) {
  split <- df %>% filter(month %in% x)
  cum.obs <- nrow(split) # cumulative no. observations
  cum.spp <- length(unique(split$Taxon)) # cumulative no. species
  biodiv_calcs <- data.frame(cum.obs, cum.spp) 
  biodiv_calcs
}

m.1 <- split_biodiv_month(combined, 1) %>% mutate(month= "1")
m.2 <- split_biodiv_month(combined, 2) %>% mutate(month= "2")
m.3 <- split_biodiv_month(combined, 3) %>% mutate(month= "3")
m.4 <- split_biodiv_month(combined, 4) %>% mutate(month= "4")
m.5 <- split_biodiv_month(combined, 5) %>% mutate(month= "5")
m.6 <- split_biodiv_month(combined, 6) %>% mutate(month= "6")
m.7 <- split_biodiv_month(combined, 7) %>% mutate(month= "7")
m.8 <- split_biodiv_month(combined, 8) %>% mutate(month= "8")
m.9 <- split_biodiv_month(combined, 9) %>% mutate(month= "9")
m.10<- split_biodiv_month(combined, 10) %>% mutate(month= "10")
m.11<- split_biodiv_month(combined, 11) %>% mutate(month= "11")
m.12<- split_biodiv_month(combined, 12) %>% mutate(month= "12")

summary_month <- rbind(m.1, m.2, m.3, m.4, m.5, m.6, m.7, 
                       m.8, m.9, m.10, m.11, m.12)

#focus on one figure after the next, have a separate script for each of these figs
#context for interpretting each figure 
#start with the years going by, then proceed to the 2023 data


summary_2023 <- combined %>%
  filter(year == 2023) %>%
  group_by(month) %>%
  summarise(spp = length(unique(Taxon)))


summary_2023 %>%
  plot_ly(y = ~spp, x=~as.numeric(month), type="scatter")

arthropod %>%
  group_by(Taxon, month, year) %>%
  summarise(count=n()) %>%
  plot_ly(x = ~as.factor(month), y=~count, stroke=~as.factor(year), type="scattercarpet")




#culturally significant intersection 

#regenerate 
summary <- read_csv("Galiano_marine_animals_resynthesized_summary_2023-11-07.csv")
animal <- read_csv("Galiano_marine_animal_records_consolidated-2023-11-07.csv")
hulq <- read_csv("reintegrated-hulq.csv") %>%
  rename(scientificName = `iNaturalist taxon name`)%>%
  select(scientificName, `Food Value`, `Medicinal Value`, `Spiritual Value`, `Material Value`, 
         `Trade Value`, `Indicator Value`, `Hulquminum Name`, `Hulquminum Authority`)
hulq.names <- unique(hulq$scientificName) 
cultural <- animals %>% filter(scientificName %in% hulq.names)
algae <- read_csv("Galiano_marine_algae_records_consolidated_2023-04-22.csv")






#vector w/unique list of names, filter catalogue based on values in that vector


length(unique(summary$scientificName))
# Create grid

# Create CRS object

EPSG.4326 <- st_crs(4326)

animal.points <- animal %>% drop_na(decimalLatitude)
algae.points <- algae %>% drop_na(decimalLatitude)

# Convert records to sf points

animal.points <- st_as_sf(animal.points, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)
algae.points <- st_as_sf(algae.points, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)


# 
# Transform to NAD84 UTM Zone 10

# Create CRS object

EPSG.32610 <- st_crs(32610)

# Reproject to NAD83 UTM Zone 10

# Point data (for gridded catalogs, with each record assigned cell_id)

# Point data (for gridded catalogs, with each record assigned cell_id)

animal.points <- st_transform(animal.points, crs = st_crs(EPSG.32610))
algae.points <- st_transform(algae.points, crs = st_crs(EPSG.32610))


# Combine points to create grid

points <- rbind(animal.points, algae.points)

# Set cell size

cs <- c(1000, 1000)

# Create grid

area.grid = st_make_grid(points, what = "polygons", square = TRUE, cellsize = cs)

sf::st_length(area.grid)
st_crs(area.grid)$proj4string

# Add grid ID

grid = st_sf(area.grid) %>%
  mutate(cell_id = 1:length(lengths(area.grid)))

#annelida        <- list(summary = summary %>% filter(phylum == 'Annelida'), records = cultural %>% filter(phylum == 'Annelida'))
#brachiopoda     <- list(summary = summary %>% filter(phylum == 'Brachiopoda'), records = cultural %>% filter(phylum == 'Brachiopoda'))
#bryozoa         <- list(summary = summary %>% filter(phylum == 'Bryozoa'), records = cultural %>% filter(phylum == 'Bryozoa'))
#chaetognatha    <- list(summary = summary %>% filter(phylum == 'Chaetognatha'), records = cultural %>% filter(phylum == 'Chaetognatha'))
#cnidaria        <- list(summary = summary %>% filter(phylum == 'Cnidaria'), records = cultural %>% filter(phylum == 'Cnidaria'))
crustacea       <- list(summary = summary %>% filter(subphylum == 'Crustacea'), records = cultural %>% filter(subphylum == 'Crustacea'))
#ctenophora      <- list(summary = summary %>% filter(phylum == 'Ctenophora'), records = cultural %>% filter(phylum == 'Ctenophora'))
echinodermata   <- list(summary = summary %>% filter(phylum == 'Echinodermata'), records = cultural %>% filter(phylum == 'Echinodermata'))
#entoprocta      <- list(summary = summary %>% filter(phylum == 'Entoprocta'), records = cultural %>% filter(phylum == 'Entoprocta'))
mammalia        <- list(summary = summary %>% filter(class == 'Mammalia'), records = cultural %>% filter(class == 'Mammalia'))
mollusca        <- list(summary = summary %>% filter(phylum == 'Mollusca'), records = cultural %>% filter(phylum == 'Mollusca'))
#nemertea        <- list(summary = summary %>% filter(phylum == 'Nemertea'), records = cultural %>% filter(phylum == 'Nemertea'))
#phoronida       <- list(summary = summary %>% filter(phylum == 'Phoronida'), records = cultural %>% filter(phylum == 'Phoronida'))
#platyhelminthes <- list(summary = summary %>% filter(phylum == 'Platyhelminthes'), records = cultural %>% filter(phylum == 'Platyhelminthes'))
#porifera        <- list(summary = summary %>% filter(phylum == 'Porifera'), records = cultural %>% filter(phylum == 'Porifera'))
#sipuncula       <- list(summary = summary %>% filter(phylum == 'Sipuncula'), records = cultural %>% filter(phylum == 'Sipuncula'))
#tunicata        <- list(summary = summary %>% filter(subphylum == 'Tunicata'), records = cultural %>% filter(subphylum == 'Tunicata'))
fishes          <- list(summary = summary %>% filter(class == 'Actinopterygii'| class == 'Elasmobranchii'), records = cultural %>% filter(class == 'Actinopterygii'| class == 'Elasmobranchii'))


allTaxaRecords <- list(crustacea = crustacea, echinodermata = echinodermata,
                        mammalia = mammalia, mollusca = mollusca, fishes = fishes)

#allTaxaRecords <- list(bryozoa = bryozoa)

# Process one status (i.e. new, reported, confirmed) for one taxon
#   recs - The upstream records for the taxon, including elements `summary` and `records`
#   taxon - The taxon name
#   grid - The grid to which the records are to be binned
#   status - The status to be processed (new, reported, confirmed)
# Returns a structure containing all gridded data, and various other preparatory fields - unique taxa, counts, etc.
process_one_status <- function (recs, taxon, grid, status) {
  togo <- list()
  
  togo$summary <- recs$summary 
  if (status == "all") {
    togo$summary <- recs$summary 
  } else {
    togo$summary <- recs$summary %>% filter(str_detect(reportingStatus, status))
  }
  
  togo$taxa <- unique(togo$summary$scientificName)
  
  togo$taxa.records <- recs$records %>% filter(scientificName %in% togo$taxa)
  togo$taxa.records <- togo$taxa.records %>% drop_na(decimalLatitude)
  
  # Prepare gridded choropleths of historic, confirmed, new records
  
  # New records
  
  # Convert records to sf points
  
  togo$taxa.points <- st_as_sf(togo$taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)
  
  # # Reproject to NAD83 UTM Zone 10
  
  # Generate gridded dataframes (each record assigned cell_id)
  
  togo$taxa.points <- st_transform(togo$taxa.points, crs = st_crs(EPSG.32610))
  
  togo$records.gridded <- togo$taxa.points %>% st_join(grid)
  
  togo$records.gridded <- as.data.frame(togo$records.gridded)
  
  togo$records.gridded$geometry <- NULL
  
  # Summarized points (for choropleths)
  
  togo$taxa.points.sum <- st_transform(togo$taxa.points, crs = st_crs(EPSG.32610)) %>%
    group_by(scientificName) %>%
    summarize()
  
  # Generate species richness choropleths
  # Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
  # and https://luisdva.github.io/rstats/richness/
  
  togo$grid <- grid %>%
    st_join(togo$taxa.points.sum) %>%
    mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
    group_by(cell_id) %>%
    summarize(richness = sum(overlap))
  
  # Remove grid cell with zero records
  
  togo$grid = filter(togo$grid, richness > 0)
  
  togo$grid$status <- status
  togo
}

# Write an SHP file, ensuring to delete anything at the target path first
write_grid <- function (grid, path) {
  unlink(path, recursive=TRUE)
  st_write(grid, path, driver = "ESRI Shapefile")
}

# Process one taxon's worth of records, calling process_one_status for each status, writing SHP and CSV files for each resulting status
process_one_taxon <- function (recs, taxon, grid) {
  print(str_glue("Processing {taxon}"))
  # Subset historic, confirmed and new records
  togo <- list(new = process_one_status(recs, taxon, grid, "new"),
               confirmed = process_one_status(recs, taxon, grid, "confirmed"), 
               reported = process_one_status(recs, taxon, grid, "reported"),
               all = process_one_status(recs, taxon, grid, "all"))
  
  #plot(togo$reported$grid)
  
  # Write choropleths
  
write_grid(togo$all$grid, str_glue("analyze/Animalia/marine_animals/combined_analysis/outputs/vectors/{taxon}_cultural_all_grid"))
  
  # Write gridded dataframes
write.csv(togo$all$records.gridded, str_glue("analyze/Animalia/marine_animals/combined_analysis/outputs/tabular_data/{taxon}_cultural_all_records_gridded.csv"), row.names = FALSE)
  
  # Write summary
  
  write.csv(recs$summary, str_glue("analyze/Animalia/marine_animals/combined_analysis/outputs/tabular_data/{taxon}_summary.csv"), row.names = FALSE)  
  
  togo
}

allTaxaGridded = list()

for (taxon in names(allTaxaRecords)) {
  taxonGridded <- process_one_taxon(allTaxaRecords[[taxon]], taxon, grid)
  allTaxaGridded[[taxon]] <- taxonGridded
}






