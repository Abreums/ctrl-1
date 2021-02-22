# R tip ----

# Libraries ----
library(visdat) # Preliminary Visualisation of Data, CRAN v0.5.3      
library(naniar) # Data Structures, Summaries, and Visualisations for Missing Data, CRAN v0.6.0      
library(simputation) # Simple Imputation, CRAN v0.2.6 
library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.0   

# 1. Load data ----
air_quality_tbl <- airquality %>% as_tibble()

# 2. Missing data visualization ----
air_quality_tbl %>% vis_dat()
air_quality_tbl %>% vis_miss()

# 3. Miss upset
air_quality_tbl %>% gg_miss_upset()

# 4. ggplot missing points
air_quality_tbl %>% ggplot(aes(x = Solar.R, y = Ozone)) +
  geom_miss_point()
