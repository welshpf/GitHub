library(tidyverse)
df <- datasets::airmiles %>%
      tibble::enframe(name = "year", value = "miles")

# Create histogram
df %>% 
ggplot(aes(x = miles)) +
geom_histogram(bins = 30)
