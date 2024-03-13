pacman::p_load(tidyverse, sf, viridis)
hunterston<- read_csv("hunterston2010bioblitz.csv") %>%
  rename(Type = Zone_Description)
#looking at the data structure
glimpse(hunterston)
#how many unique species and abundance by species
#355 species, 
count(hunterston, Scientific)
#calculating unique species by ecosystem type 
unique_species <-hunterston %>%
  group_by(Type) %>%
  summarise(unique_species = n_distinct(Scientific)) 

#why are these the zones that were binned? is there documentation of this anywhere?
unique_species%>%
  ggplot(aes(x=reorder(Zone_Description, -unique_species), y=unique_species))+
  geom_col()+
  coord_flip()

#reading in the shp file for ecotypes across hunterston
nc <- st_read("complete_hunterston_map.shp") 
#joining the shape file with the unique species by ecosystem type
joined <- left_join(nc, unique_species, by= "Type")
#plotting the join occurence data and the spatial data
joined %>%
ggplot()+
  geom_sf(aes(fill = unique_species))+
  scale_fill_viridis()

