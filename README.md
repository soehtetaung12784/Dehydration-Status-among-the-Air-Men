# Dehydration Status among the Air Men in Myanmar

An adequate hydration status is critical to ensure efficiency during mental and physical activities. Our goal was to assess the hydration status of aeronautical military men and to determine the association of hydration status with body composition and anxiety. A total of 72 men were evaluated through a validated hydration questionnaire, anthropometric and
biochemical parameters, and an anxiety questionnaire. Based on these methods, the criteria of hydration were established.



## Authors

- [@soehtetaung12784](https://github.com/soehtetaung12784)


## Badges

Add badges from somewhere like: [shields.io](https://shields.io/)

[![MIT License](https://img.shields.io/badge/License-MIT-green.svg)](https://choosealicense.com/licenses/mit/)
[![GPLv3 License](https://img.shields.io/badge/License-GPL%20v3-yellow.svg)](https://opensource.org/licenses/)
[![AGPL License](https://img.shields.io/badge/license-AGPL-blue.svg)](http://www.gnu.org/licenses/agpl-3.0)


## Deata Wrangling

# pipe the raw dataset through the function clean_names(), assign result as "airforce"  
airforce <- airforce %>% 
  janitor::clean_names()

airforce %>% count()

# rename the variabels
airforce <- rename(airforce, 
                   height = height_cm,
                   weight = weight_kg,
                   tbw    = tbw_total_body_water_kg ,
                   icw    = icw_intracellular_water_kg,
                   ecw    = ecw_extracellular_water_kg,
                   protein = protein_kg,
                   minerals = minerals_kg,
                   bfm = bfm_body_fat_mass_kg,
                   slm = slm_soft_lean_mass_kg,
                   ffm = ffm_fat_free_mass_kg,
                   smm = smm_skeletal_muscle_mass_kg,
                   bmi = bmi_body_mass_index,
                   pbf = pbf_percent_body_fat,
                   bmr = bmr_basal_metabolic_rate,
                   whr = whr_waist_hip_ratio)

names(airforce)
str(airforce)


# transforming the variables
airforce$bmr_basal_metabolic_rate <- as.factor(airforce$group)  # change group into factor
airforce$role <- as.factor(airforce$role) # change role into factor

str(airforce)
view(airforce)




airforce <- airforce %>% mutate(  # create specific gravity
  specific_gravity = ifelse(sg < 1.02, "dehydration", "no dehydration"))

view(airforce)

airforce$osmo <- as.numeric(airforce$osmo)
class(airforce$osmo)

library(dplyr)

airforce <- airforce %>% mutate(osmolarity = case_when(
  osmo < 500 ~ "Normal",
  osmo >= 500 & osmo <= 799 ~ "Mild",
  osmo >= 800 ~ "Hypohydration"))
                    
view(airforce)



airforce$specific_gravity <- as.factor(airforce$specific_gravity)# change factor var
airforce$osmolarity   <- as.factor(airforce$osmolarity) # change factor var
str(airforce)


dr.nyan <- airforce # keep as master file




