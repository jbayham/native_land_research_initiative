

#If build/inputs does not exist, establish symbolic link to folder on RSTOR
if(!dir.exists("01_data/census_2020"))  system("ln -s /RSTOR/bayham/projects/native_land_research_initiative/census_2020 01_data/census_2020")





