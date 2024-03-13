# Load libraries
pacman::p_load(sf, tidyverse, sf, viridis, patchwork)

# Source dependencies

source("scripts/utils.R")

# Analysis of historical collection activities

inat_tidy <- read_csv("intersection_inat_hunterstonmap_assoc_polygons.csv") %>%
  filter(quality_grade == "research") #lose ~1000 rows of data
hunterston2023 <- inat_tidy %>%
  drop_na(Id_2)%>% #70% data loss 
  mutate(year=2023) %>%
  rename(type = Type, Id = Id_2, scientificName = scientific_name) %>%
  select(scientificName, Id, latitude, longitude, year, type,iconic_taxon_name) %>%
  mutate(fullname=scientificName) %>%
  separate(scientificName, c("genus", "species"))


hunterston2010 <- read_csv("hunterston2010_data+updatedpolygons.csv") %>%
  mutate(year=2010) %>%
  rename(scientificName = Scientific, 
         type = Type, 
         latitude = Latitude, 
         longitude = Longitude) %>%
  select(scientificName, Id, latitude, longitude, year, type)%>%
  mutate(fullname=scientificName) %>%
  separate(scientificName, c("genus", "species"))

hunterston2010_names <- unique(hunterston2010$fullname)
hunterston2023_names <- unique(hunterston2023$fullname)
hunterston2023_coords <- hunterston2023 %>% select(Id, latitude, longitude)
confirmed_obs <- hunterston2023 %>% filter(scientificName %in% hunterston2010_names) #261 confirmed observations


left_join(hunterston2010_names, hunterston2023_names, by="fullname")
#reported
x <- anti_join(hunterston2010, hunterston2023, by="genus")
#new
y <- anti_join(hunterston2023, hunterston2010, by="genus")
#confirmed
z <- left_join(hunterston2023, hunterston2010, by="genus")
summary_confirmed <- z %>%
  summarise(species=length(unique(genus)), 
            group=length(unique(iconic_taxon_name))) #166 confirmed

summary_new <- y %>%
  summarise(species=length(unique(genus)), 
            group=length(unique(iconic_taxon_name))) #50 new species

summary_reported <- x %>% 
  group_by(iconic_taxon_name) %>%
  summarise(species=length(unique(genus))) %>%
  mutate(type="reported")


summary_confirmed <- z %>% 
  group_by(iconic_taxon_name) %>%
  summarise(species=length(unique(genus))) %>%
  mutate(type="confirmed")

summary_new <- y %>% 
  group_by(iconic_taxon_name) %>%
  summarise(species=length(unique(genus))) %>%
  mutate(type="new")
  
  
summary <- rbind(summary_confirmed, summary_new)



  
summary %>% ggplot(aes(x=reorder(iconic_taxon_name, species), y=species, fill=factor(type, levels=c("new", "confirmed"))))+
  geom_col()+
  scale_fill_manual(values=c("slateblue2", "slateblue4"), name="observation type")+
  xlab("Taxonomic group")+
  ylab("Number of unique taxa")+
  ggtitle("Observations from 2023 Bioblitz")+
  coord_flip()+
  theme_bw()

pl2 <- summary_new  %>%
  ggplot(aes(x=iconic_taxon_name, y=species))+
           geom_col()+
  geom_col(fill="slateblue4")+
  xlab("Taxonomic group")+
  ylab("Number of unique taxa")+
  ggtitle("New observations from 2023 Bioblitz")+
  coord_flip()+
  theme_bw()


pl1 + pl2

#anti_join? --> reported dataframe
#

new_obs <- hunterston2023 %>% filter(!scientificName %in% hunterston2010_names) #134 new observations
hulq <- read_csv("reintegrated-hulq.csv") %>%
  rename(scientificName = `iNaturalist taxon name`)%>%
  select(scientificName, `Food Value`, `Medicinal Value`, `Spiritual Value`, `Material Value`, 
         `Trade Value`, `Indicator Value`, `Hulquminum Name`, `Hulquminum Authority`)
hulq.names <- unique(hulq$scientificName) 

confirmed_cultr <- z %>% filter(fullname.x %in% hulq.names) # 2 confirmed
new_cult <- y %>% filter(fullname %in% hulq.names) # 2 new 

summary_2010 <- hunterston2010 %>%
  group_by(Id) %>%
  summarise(species=length(unique(scientificName)))

summary_2023 <- hunterston2023 %>%
  group_by(Id) %>%
  summarise(species=length(unique(scientificName)))

summary_new <- new_obs %>%
  group_by(Id) %>%
  summarise(species=length(unique(scientificName)))

#species list per community 
#geoJson list of species present

summary_confirmed <- confirmed_obs %>%
  group_by(Id) %>%
  summarise(species=length(unique(scientificName)))

# loading in polygons from shp file
grid <- st_read("new_hunterston_map.shp") %>%
  select(Id, geometry)
#merging shp file with biodiversity summary
summary_2010pn <- merge(grid, summary_2010, by="Id")
summary_2023pn <- merge(grid, summary_2023, by="Id")
summary_newpn <- merge(grid, summary_new, by="Id")
summary_confirmedpn <- merge(grid, summary_confirmed, by="Id")

#plotting

summary_2010pnplot <- summary_2010pn %>%
  ggplot()+
  geom_sf(aes(fill=species))+
  scale_fill_viridis(option="D")+
  ggtitle("2010")

summary_2023pnplot <- summary_2023pn %>%
  ggplot()+
  geom_sf(aes(fill=species))+
  scale_fill_viridis(option="D")+
  ggtitle("2023")

summary_newpnplot <- summary_newpn %>%
  ggplot()+
  geom_sf(aes(fill=species))+
  scale_fill_viridis(option="D")+
  ggtitle("new species")

summary_confirmedpnplot <- summary_confirmedpn %>%
  ggplot()+
  geom_sf(aes(fill=species))+
  scale_fill_viridis(option="D")+
  ggtitle("confirmed species")



print(summary_2010pnplot)
print(summary_2023pnplot)
print(summary_newpnplot)
print(summary_confirmedpnplot)







p1a <- confirmed_obs %>%
  filter(iconic_taxon_name %in% c("Reptilia", "Plantae", "Mollusca", 
                                  "Insecta", "Aves", "Animalia")) %>%
  count(iconic_taxon_name) %>% 
  ggplot(aes(x=iconic_taxon_name, y=n))+
  geom_col(fill="slateblue4")+
  xlab("Animal group")+
  ylab("Number of unique observations")+
  ggtitle("Confirmed observations from 2023 Bioblitz")+
  coord_flip()+
  theme_bw()

p2a <- new_obs %>%
  filter(iconic_taxon_name %in% c("Reptilia", "Plantae", "Mollusca", 
                                  "Insecta", "Aves", "Animalia")) %>%
    count(iconic_taxon_name) %>% 
    ggplot(aes(x=iconic_taxon_name, y=n))+
    geom_col(fill="slateblue4")+
    xlab("Animal group")+
    ylab("Number of unique observations")+
    ggtitle("New observations from 2023 Bioblitz")+
    coord_flip()+
    theme_bw()
p1a+p2a

p1b <- confirmed_obs %>%
  filter(!iconic_taxon_name %in% c("Reptilia", "Plantae", "Mollusca", 
                                  "Insecta", "Aves", "Animalia")) %>%
  count(iconic_taxon_name) %>% 
  ggplot(aes(x=iconic_taxon_name, y=n))+
  geom_col(fill="slateblue2")+
  xlab("Animal group")+
  ylab("Number of unique observations")+
  ggtitle("Confirmed observations from 2023 Bioblitz")+
  coord_flip()+
  theme_bw()

p2b <- new_obs %>%
  filter(!iconic_taxon_name %in% c("Reptilia", "Plantae", "Mollusca", 
                                  "Insecta", "Aves", "Animalia")) %>%
  count(iconic_taxon_name) %>% 
  ggplot(aes(x=iconic_taxon_name, y=n))+
  geom_col(fill="slateblue2")+
  xlab("Animal group")+
  ylab("Number of unique observations")+
  ggtitle("New observations from 2023 Bioblitz")+
  coord_flip()+
  theme_bw()

(p1a+p2a) / (p1b+p2b)

