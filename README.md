
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

### pipe the raw dataset through the function clean_names(), assign result as "airforce"  
airforce <- airforce %>% 
  janitor::clean_names()

airforce %>% count()

#### rename the variabels
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


### transforming the variables
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

## Statistical Methods 

### ANOVA 

pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics 
  gtsummary,    # summary statistics and tests
  rstatix,      # summary statistics and statistical tests
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
  flextable     # converting tables to pretty images
)

library(tidyverse)
library(ggpubr)
library(rstatix)
library(dplyr)

### summary statistics
str(airforce)

airforce %>%  # get summary statistics
  get_summary_stats(age, height, weight, bmi, whr, protein, pbf)

airforce %>% group_by(osmolarity) %>%
  get_summary_stats(pbf,  type = "common")


### Anova Test (One Way) 

### Equality of variances - homogeneity ##

### Boxplot
boxplot(age ~ osmolarity,
        data = airforce)
### Dotplot
library("lattice")

dotplot(age ~ osmolarity,
        data = airforce)

### Levene's test
library(car)

leveneTest(age ~ osmolarity,
           data = airforce) # P > 0.05 means variances are equal

### Normality Check 

res_aov <- aov(pdf ~ osmolarity, 
               data = airforce)
par(mfrow = c(1, 2)) # combine plots

### histogram
hist(res_aov$residuals)

### QQ-plot
library(car)
qqPlot(res_aov$residuals,
       id = FALSE # id = FALSE to remove point identification)

shapiro.test(res_aov$residuals) # normality: P-value of the Shapiro-Wilk test on the residuals > 0.05


### One way test 
oneway.test(protein ~ osmolarity,
            data = airforce,
            var.equal = TRUE) # assuming equal variances


## Correlation  

### data extraction
cor <- airforce %>% select(tbw, icw, ecw, osmolarity, osmo, sg, ph, a_score, group)

### remove unecessary variables
library(tidyverse)
dat <- cor %>%
  select(-a_score, -group, -osmolarity)

### display 5 first obs. of new dataset
head(dat, 5)
view(dat)

### Pearson correlation between 2 variables
cor(dat$tbw, dat$osmo,
    method = "pearson" )
    
### correlation for all variables
round(cor(dat),
      digits = 2 # rounded to 2 decimals)

### Visualization
### scatterplot
library(ggplot2)

ggplot(dat) +
  aes(x = tbw, y = osmo) +
  geom_point(colour = "#0c4c8a") +
  theme_minimal()

### multiple scatterplots
pairs(dat[, c("tbw", "osmo", "ph","sg")])

### improved correlation matrix
library(corrplot)

corrplot(cor(dat),
         method = "number",
         type = "upper" # show only upper side)

### Pearson correlation test
test <- cor.test(dat$tbw, dat$osmo)
test

### correlation tests for whole dataset
library(Hmisc)
res <- rcorr(as.matrix(dat)) # rcorr() accepts matrices only

### display p-values (rounded to 3 decimals)
round(res$P, 3)

### Combination of correlation coefficients and correlation tests
library(correlation)

correlation::correlation(dat, include_factors = TRUE, method = "auto")

library(GGally)
ggpairs(dat[, c("tbw", "ecw","icw", "sg","osmo")])

library(ggstatsplot)
ggcorrmat( data = dat[, c("tbw", "ecw","icw", "sg","osmo")],
  type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)


## Ordinal Logistic Regression 

require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  epikit,     # age_categories() function
  tidyverse   # data management and visualization
)

### Slice with columns name
dat <- airforce[, c('age', 'weight','bmi','whr','bfm', 'osmolarity')]

head(dat)


### build model
m <- polr(osmolarity ~ age + weight + bmi+ whr + bfm, data = dat, Hess=TRUE)
summary(m)


### calculate confidence interval for regression coefficient for 'hours'
confint(m, level=0.95)

### store table
(ctable <- coef(summary(m)))

### calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

### combined table
(ctable <- cbind(ctable, "p value" = p))

(ci <- confint(m)) # default method gives profiled CIs

confint.default(m) # CIs assuming normality

### odds ratios
exp(coef(m))
### OR and CI
exp(cbind(OR = coef(m), ci))

## Visualization 

### Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)

dat1 <- airforce[,c('age', 'weight','bmi','whr','bfm', 'osmolarity', 'role')]

### Plot
dat1 %>%
  ggplot( aes(x=osmolarity, y=age, fill=role)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)  ) +
  ggtitle("Distribution of osmolarity by age among air men ") +
  xlab("Osmolarity") 
  ![My Remote Image](https://drive.google.com/file/d/12eaPzTiv9O_nMBXNAD6TJyevVpMgSDdG/view?usp=share_link)
  
# End 
