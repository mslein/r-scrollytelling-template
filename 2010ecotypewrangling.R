pacman::p_load(tidyverse)

ec01<-read_csv("HF_2010_taxa_EC_01.csv") %>% mutate(ecotype = 1)
ec02<-read_csv("HF_2010_taxa_EC_02.csv") %>% mutate(ecotype = 2)
ec03<-read_csv("HF_2010_taxa_EC_03.csv") %>% mutate(ecotype = 3)
ec04<-read_csv("HF_2010_taxa_EC_04.csv") %>% mutate(ecotype = 4)
ec05<-read_csv("HF_2010_taxa_EC_05.csv") %>% mutate(ecotype = 5)
ec06<-read_csv("HF_2010_taxa_EC_06.csv") %>% mutate(ecotype = 6)
ec07<-read_csv("HF_2010_taxa_EC_07.csv") %>% mutate(ecotype = 7)
ec08<-read_csv("HF_2010_taxa_EC_08.csv") %>% mutate(ecotype = 8)
ec09<-read_csv("HF_2010_taxa_EC_09.csv") %>% mutate(ecotype = 9)
ec10<-read_csv("HF_2010_taxa_EC_10.csv") %>% mutate(ecotype = 10)
ec11<-read_csv("HF_2010_taxa_EC_11.csv") %>% mutate(ecotype = 11)
ec12<-read_csv("HF_2010_taxa_EC_12.csv") %>% mutate(ecotype = 12)

full_data <- rbind(ec01, ec02, ec03, ec04, ec05, ec06, ec07, ec08, ec09, ec10, ec11, ec12)

count(full_data, Group)

marine <- full_data %>%
  filter(Group %in% c("Algae", "Cnidarians", "Fish", "Marine molluscs", "Echinoderms", "Crustaceans", "Ctenophores"))

botany <- full_data %>%
  filter(Group %in% c("Plants", "Mosses"))

ornithology <- full_data %>%
  filter(Group %in% c("Birds"))

mycology <- full_data %>%
  filter(Group %in% c("Fungi", "Lichens"))

entomology <- full_data %>%
  filter(Group %in% c("Arachnid", "Insects", "Springtails"))

herptology_et_al <- full_data %>%
  filter(Group %in% c("Amphibians", "Annelids", "Terrestrial molluscs", "Reptiles"))

mammals <- full_data %>%
  filter(Group %in% c("Mammals"))

write_csv(marine, "marine_zoology_HF2010.csv")
write_csv(botany, "botany_HF2010.csv")
write_csv(ornithology, "ornithology_HF2010.csv")
write_csv(mycology, "mycology_etal_HF2010.csv")
write_csv(entomology, "entomology_HF2010.csv")
write_csv(herptology_et_al, "herptology_etal_HF2010.csv")
write_csv(mammals, "mammals_HF2010.csv")







