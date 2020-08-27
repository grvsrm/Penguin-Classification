Tidymodels and Classification on Palmer Penguin Data
================
Gaurav Sharma
27/08/2020

# Reading the files

``` r
ttfile <- tidytuesdayR::tt_load('2020-07-28')
```

    ## 
    ##  Downloading file 1 of 2: `penguins.csv`
    ##  Downloading file 2 of 2: `penguins_raw.csv`

``` r
penguins <- ttfile$penguins
```

``` r
penguins %>% 
    drop_na() %>%
    pivot_longer(bill_length_mm:body_mass_g, names_to = 'spec', values_to = 'value') %>% 
    ggplot(aes(x= value, y = sex, fill = sex)) +
    geom_boxplot(color = "gray") +
    facet_wrap(~spec, scales = "free_y") +
    coord_flip()
```

![](index_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
