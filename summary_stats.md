summary-stats
================
Ngoc Duong
4/30/2020

Load curves.Rdata

``` r
load("./curves.RData")
```

``` r
by_country %>% group_by(country_region) %>%
  rename(`Country/Region` = country_region) %>% 
  summarise(`Median days` = median(t),
            `1st quartile` = quantile(t, probs = 0.25),
            `2nd quartile` = quantile(t, probs = 0.75),
            `Max days` = max(t)) %>% 
  knitr::kable()
```

| Country/Region                   | Median days | 1st quartile | 2nd quartile | Max days |
| :------------------------------- | ----------: | -----------: | -----------: | -------: |
| Afghanistan                      |        28.0 |        14.00 |        42.00 |       56 |
| Albania                          |        21.0 |        10.50 |        31.50 |       42 |
| Algeria                          |        27.5 |        13.75 |        41.25 |       55 |
| Andorra                          |        24.5 |        12.25 |        36.75 |       49 |
| Angola                           |        13.0 |         6.50 |        19.50 |       26 |
| Antigua and Barbuda              |        19.0 |         9.50 |        28.50 |       38 |
| Argentina                        |        24.0 |        12.00 |        36.00 |       48 |
| Armenia                          |        25.0 |        12.50 |        37.50 |       50 |
| Australia                        |        42.5 |        21.25 |        63.75 |       85 |
| Austria                          |        27.5 |        13.75 |        41.25 |       55 |
| Azerbaijan                       |        25.0 |        12.50 |        37.50 |       50 |
| Bahamas                          |        13.0 |         6.50 |        19.50 |       26 |
| Bahrain                          |        28.0 |        14.00 |        42.00 |       56 |
| Bangladesh                       |        21.5 |        10.75 |        32.25 |       43 |
| Barbados                         |        17.0 |         8.50 |        25.50 |       34 |
| Belarus                          |        26.0 |        13.00 |        39.00 |       52 |
| Belgium                          |        38.0 |        19.00 |        57.00 |       76 |
| Belize                           |        13.0 |         6.50 |        19.50 |       26 |
| Benin                            |        17.5 |         8.75 |        26.25 |       35 |
| Bhutan                           |        22.5 |        11.25 |        33.75 |       45 |
| Bolivia                          |        20.0 |        10.00 |        30.00 |       40 |
| Bosnia and Herzegovina           |        23.0 |        11.50 |        34.50 |       46 |
| Botswana                         |        10.5 |         5.25 |        15.75 |       21 |
| Brazil                           |        27.0 |        13.50 |        40.50 |       54 |
| Brunei                           |        21.0 |        10.50 |        31.50 |       42 |
| Bulgaria                         |        21.5 |        10.75 |        32.25 |       43 |
| Burkina Faso                     |        20.5 |        10.25 |        30.75 |       41 |
| Burma                            |        12.0 |         6.00 |        18.00 |       24 |
| Burundi                          |        10.0 |         5.00 |        15.00 |       20 |
| Cabo Verde                       |        13.0 |         6.50 |        19.50 |       26 |
| Cambodia                         |        42.0 |        21.00 |        63.00 |       84 |
| Cameroon                         |        13.0 |         6.50 |        19.50 |       26 |
| Canada                           |        42.5 |        21.25 |        63.75 |       85 |
| Central African Republic         |        18.0 |         9.00 |        27.00 |       36 |
| Chad                             |        13.0 |         6.50 |        19.50 |       26 |
| Chile                            |        24.0 |        12.00 |        36.00 |       48 |
| China                            |        44.5 |        22.25 |        66.75 |       89 |
| Colombia                         |        22.5 |        11.25 |        33.75 |       45 |
| Congo (Brazzaville)              |        18.0 |         9.00 |        27.00 |       36 |
| Congo (Kinshasa)                 |        20.0 |        10.00 |        30.00 |       40 |
| Costa Rica                       |        22.5 |        11.25 |        33.75 |       45 |
| Cote d’Ivoire                    |        20.0 |        10.00 |        30.00 |       40 |
| Croatia                          |        27.5 |        13.75 |        41.25 |       55 |
| Cuba                             |        19.5 |         9.75 |        29.25 |       39 |
| Cyprus                           |        21.0 |        10.50 |        31.50 |       42 |
| Czechia                          |        13.0 |         6.50 |        19.50 |       26 |
| Denmark                          |        26.5 |        13.25 |        39.75 |       53 |
| Diamond Princess                 |        13.0 |         6.50 |        19.50 |       26 |
| Djibouti                         |        16.5 |         8.25 |        24.75 |       33 |
| Dominica                         |        13.0 |         6.50 |        19.50 |       26 |
| Dominican Republic               |        25.0 |        12.50 |        37.50 |       50 |
| Ecuador                          |        25.0 |        12.50 |        37.50 |       50 |
| Egypt                            |        33.0 |        16.50 |        49.50 |       66 |
| El Salvador                      |        13.0 |         6.50 |        19.50 |       26 |
| Equatorial Guinea                |        18.0 |         9.00 |        27.00 |       36 |
| Eritrea                          |        13.0 |         6.50 |        19.50 |       26 |
| Estonia                          |        26.5 |        13.25 |        39.75 |       53 |
| Eswatini                         |        18.5 |         9.25 |        27.75 |       37 |
| Ethiopia                         |        19.0 |         9.50 |        28.50 |       38 |
| Fiji                             |        13.0 |         6.50 |        19.50 |       26 |
| Finland                          |        41.0 |        20.50 |        61.50 |       82 |
| France                           |        43.5 |        21.75 |        65.25 |       87 |
| Gabon                            |        18.5 |         9.25 |        27.75 |       37 |
| Gambia                           |        13.0 |         6.50 |        19.50 |       26 |
| Georgia                          |        27.0 |        13.50 |        40.50 |       54 |
| Germany                          |        42.0 |        21.00 |        63.00 |       84 |
| Ghana                            |        18.5 |         9.25 |        27.75 |       37 |
| Greece                           |        27.0 |        13.50 |        40.50 |       54 |
| Grenada                          |        13.0 |         6.50 |        19.50 |       26 |
| Guatemala                        |        18.5 |         9.25 |        27.75 |       37 |
| Guinea                           |        19.0 |         9.50 |        28.50 |       38 |
| Guinea-Bissau                    |        13.0 |         6.50 |        19.50 |       26 |
| Guyana                           |        19.5 |         9.75 |        29.25 |       39 |
| Haiti                            |        13.0 |         6.50 |        19.50 |       26 |
| Holy See                         |        22.5 |        11.25 |        33.75 |       45 |
| Honduras                         |        20.0 |        10.00 |        30.00 |       40 |
| Hungary                          |        23.5 |        11.75 |        35.25 |       47 |
| Iceland                          |        26.0 |        13.00 |        39.00 |       52 |
| India                            |        40.5 |        20.25 |        60.75 |       81 |
| Indonesia                        |        24.5 |        12.25 |        36.75 |       49 |
| Iran                             |        30.5 |        15.25 |        45.75 |       61 |
| Iraq                             |        28.0 |        14.00 |        42.00 |       56 |
| Ireland                          |        25.5 |        12.75 |        38.25 |       51 |
| Israel                           |        29.5 |        14.75 |        44.25 |       59 |
| Italy                            |        40.0 |        20.00 |        60.00 |       80 |
| Jamaica                          |        20.0 |        10.00 |        30.00 |       40 |
| Japan                            |        44.5 |        22.25 |        66.75 |       89 |
| Jordan                           |        24.0 |        12.00 |        36.00 |       48 |
| Kazakhstan                       |        19.0 |         9.50 |        28.50 |       38 |
| Kenya                            |        19.0 |         9.50 |        28.50 |       38 |
| Korea, South                     |        44.5 |        22.25 |        66.75 |       89 |
| Kosovo                           |        12.5 |         6.25 |        18.75 |       25 |
| Kuwait                           |        28.0 |        14.00 |        42.00 |       56 |
| Kyrgyzstan                       |        16.5 |         8.25 |        24.75 |       33 |
| Laos                             |        13.0 |         6.50 |        19.50 |       26 |
| Latvia                           |        24.5 |        12.25 |        36.75 |       49 |
| Lebanon                          |        29.5 |        14.75 |        44.25 |       59 |
| Liberia                          |        17.5 |         8.75 |        26.25 |       35 |
| Libya                            |        13.0 |         6.50 |        19.50 |       26 |
| Liechtenstein                    |        23.5 |        11.75 |        35.25 |       47 |
| Lithuania                        |        26.0 |        13.00 |        39.00 |       52 |
| Luxembourg                       |        25.5 |        12.75 |        38.25 |       51 |
| Madagascar                       |        13.0 |         6.50 |        19.50 |       26 |
| Malawi                           |         9.0 |         4.50 |        13.50 |       18 |
| Malaysia                         |        43.0 |        21.50 |        64.50 |       86 |
| Maldives                         |        21.5 |        10.75 |        32.25 |       43 |
| Mali                             |        13.0 |         6.50 |        19.50 |       26 |
| Malta                            |        22.0 |        11.00 |        33.00 |       44 |
| Martinique                       |         8.5 |         4.25 |        12.75 |       17 |
| Mauritania                       |        18.5 |         9.25 |        27.75 |       37 |
| Mauritius                        |        16.5 |         8.25 |        24.75 |       33 |
| Mexico                           |        26.0 |        13.00 |        39.00 |       52 |
| Moldova                          |        21.5 |        10.75 |        32.25 |       43 |
| Monaco                           |        25.5 |        12.75 |        38.25 |       51 |
| Mongolia                         |        20.5 |        10.25 |        30.75 |       41 |
| Montenegro                       |        17.0 |         8.50 |        25.50 |       34 |
| Morocco                          |        24.5 |        12.25 |        36.75 |       49 |
| Mozambique                       |        13.0 |         6.50 |        19.50 |       26 |
| MS Zaandam                       |        11.5 |         5.75 |        17.25 |       23 |
| Namibia                          |        18.5 |         9.25 |        27.75 |       37 |
| Nepal                            |        43.0 |        21.50 |        64.50 |       86 |
| Netherlands                      |        26.5 |        13.25 |        39.75 |       53 |
| New Zealand                      |        26.0 |        13.00 |        39.00 |       52 |
| Nicaragua                        |        13.0 |         6.50 |        19.50 |       26 |
| Niger                            |        13.0 |         6.50 |        19.50 |       26 |
| Nigeria                          |        26.0 |        13.00 |        39.00 |       52 |
| North Macedonia                  |        27.0 |        13.50 |        40.50 |       54 |
| Norway                           |        27.0 |        13.50 |        40.50 |       54 |
| Oman                             |        28.0 |        14.00 |        42.00 |       56 |
| Pakistan                         |        27.0 |        13.50 |        40.50 |       54 |
| Panama                           |        20.5 |        10.25 |        30.75 |       41 |
| Papua New Guinea                 |        13.0 |         6.50 |        19.50 |       26 |
| Paraguay                         |        21.5 |        10.75 |        32.25 |       43 |
| Peru                             |        22.5 |        11.25 |        33.75 |       45 |
| Philippines                      |        40.5 |        20.25 |        60.75 |       81 |
| Poland                           |        23.5 |        11.75 |        35.25 |       47 |
| Portugal                         |        24.5 |        12.25 |        36.75 |       49 |
| Qatar                            |        25.5 |        12.75 |        38.25 |       51 |
| Romania                          |        27.0 |        13.50 |        40.50 |       54 |
| Russia                           |        40.0 |        20.00 |        60.00 |       80 |
| Rwanda                           |        18.5 |         9.25 |        27.75 |       37 |
| Saint Kitts and Nevis            |        13.0 |         6.50 |        19.50 |       26 |
| Saint Lucia                      |        18.5 |         9.25 |        27.75 |       37 |
| Saint Vincent and the Grenadines |        18.5 |         9.25 |        27.75 |       37 |
| San Marino                       |        26.5 |        13.25 |        39.75 |       53 |
| Sao Tome and Principe            |         7.0 |         3.50 |        10.50 |       14 |
| Saudi Arabia                     |        24.5 |        12.25 |        36.75 |       49 |
| Senegal                          |        24.5 |        12.25 |        36.75 |       49 |
| Serbia                           |        22.5 |        11.25 |        33.75 |       45 |
| Seychelles                       |        18.5 |         9.25 |        27.75 |       37 |
| Sierra Leone                     |        10.0 |         5.00 |        15.00 |       20 |
| Singapore                        |        44.0 |        22.00 |        66.00 |       88 |
| Slovakia                         |        22.5 |        11.25 |        33.75 |       45 |
| Slovenia                         |        23.0 |        11.50 |        34.50 |       46 |
| Somalia                          |        17.5 |         8.75 |        26.25 |       35 |
| South Africa                     |        23.0 |        11.50 |        34.50 |       46 |
| South Sudan                      |         7.5 |         3.75 |        11.25 |       15 |
| Spain                            |        39.5 |        19.75 |        59.25 |       79 |
| Sri Lanka                        |        42.0 |        21.00 |        63.00 |       84 |
| Sudan                            |        19.0 |         9.50 |        28.50 |       38 |
| Suriname                         |        18.5 |         9.25 |        27.75 |       37 |
| Sweden                           |        40.0 |        20.00 |        60.00 |       80 |
| Switzerland                      |        27.5 |        13.75 |        41.25 |       55 |
| Syria                            |        13.0 |         6.50 |        19.50 |       26 |
| Taiwan\*                         |        44.5 |        22.25 |        66.75 |       89 |
| Tanzania                         |        17.5 |         8.75 |        26.25 |       35 |
| Thailand                         |        44.5 |        22.25 |        66.75 |       89 |
| Timor-Leste                      |        13.0 |         6.50 |        19.50 |       26 |
| Togo                             |        22.5 |        11.25 |        33.75 |       45 |
| Trinidad and Tobago              |        18.5 |         9.25 |        27.75 |       37 |
| Tunisia                          |        23.5 |        11.75 |        35.25 |       47 |
| Turkey                           |        20.0 |        10.00 |        30.00 |       40 |
| Uganda                           |        13.0 |         6.50 |        19.50 |       26 |
| Ukraine                          |        24.0 |        12.00 |        36.00 |       48 |
| United Arab Emirates             |        41.0 |        20.50 |        61.50 |       82 |
| United Kingdom                   |        40.0 |        20.00 |        60.00 |       80 |
| Uruguay                          |        18.5 |         9.25 |        27.75 |       37 |
| US                               |        28.0 |        14.00 |        42.00 |       56 |
| Uzbekistan                       |        18.0 |         9.00 |        27.00 |       36 |
| Venezuela                        |        18.5 |         9.25 |        27.75 |       37 |
| Vietnam                          |        44.0 |        22.00 |        66.00 |       88 |
| West Bank and Gaza               |        12.5 |         6.25 |        18.75 |       25 |
| Western Sahara                   |         7.5 |         3.75 |        11.25 |       15 |
| Yemen                            |         5.0 |         2.50 |         7.50 |       10 |
| Zambia                           |        16.5 |         8.25 |        24.75 |       33 |
| Zimbabwe                         |        13.0 |         6.50 |        19.50 |       26 |

### What did we learn from the fitted models?

``` r
by_country = by_country %>% rename(region = country_region)
summary_country_df = left_join(by_country, param_df1, by = "region")  
```

    ## Warning: Column `region` joining factor and character vector, coercing into
    ## character vector

**How many regions have passed the midpoint?** Characterized by max(t)
\> c

``` r
peak_data = summary_country_df %>% group_by(region) %>% 
  mutate(max_t = max(t)) %>% 
  dplyr::select(-t, -confirmed_cases, -fatalities) %>% 
  distinct(region, .keep_all=TRUE) 

past_peak_1wk = peak_data %>% filter(max_t > c + 7) 

past_peak_2wk = peak_data %>% filter(max_t > c + 14)

past_peak_1mn = peak_data %>% filter(max_t > c + 30)
past_peak_1mn
```

    ## # A tibble: 3 x 5
    ## # Groups:   region [3]
    ##   region           a     b     c max_t
    ##   <chr>        <dbl> <dbl> <dbl> <dbl>
    ## 1 Cambodia       100  0.5   52.7    84
    ## 2 China        80000  0.21  18.1    89
    ## 3 Korea, South 10000  0.19  42.4    89

Based on our models, there are three countries that have passed their
peak for at least 30 days. These countries include China and South
Korea, which were among the very first countries in the world to suffer
from the COVID-19 epidemic. The third country is Cambodia, which
recorded very low new confirmed cases and have been clear for COVID-19
for

**How many regions are approaching the end of virus spreading?**
Characterized by lowest b (bottom 10%?) among those whose max(t) \>
c

``` r
minb = peak_data %>% filter(max_t > c + 7) %>% ungroup() %>% top_n(-10, b)
minb
```

    ## # A tibble: 11 x 5
    ##    region               a     b          c max_t
    ##    <chr>            <dbl> <dbl>      <dbl> <dbl>
    ##  1 Australia        10000  0.1  70.7          85
    ##  2 Costa Rica        1000  0.1  33.4          45
    ##  3 Czechia          10000  0.09 14.2          26
    ##  4 Diamond Princess  1000  0.06  0.0000783    26
    ##  5 Lebanon           1000  0.09 43.9          59
    ##  6 Norway           10000  0.09 38.0          54
    ##  7 San Marino         500  0.09 34.1          53
    ##  8 Senegal            500  0.1  37.5          49
    ##  9 Taiwan*            500  0.1  66.2          89
    ## 10 Uganda             100  0.06 15.7          26
    ## 11 Vietnam            500  0.07 77.6          88

We picked countries that have the lowest growth rate “b” among those
that have passed their peak for at least 7 days. Countries in this list
include “Australia”, “Costa Rica”, Czechia“,”Lebanon“,”Norway“,”San
Marino“,”Senegal“,”Taiwan“,”Uganda“,”Vietnam“, and”Diamond Princess."

**Which regions have faster growth rate and which have more “flat
growth”** Characterized by larger b and smaller b among those whose
max(t) \< c. Implications for better allocation of resources and public
health interventions.

``` r
fast_growth_overall = peak_data %>% ungroup() %>% top_n(5, b)
fast_growth_overall
```

    ## # A tibble: 5 x 5
    ##   region                  a     b     c max_t
    ##   <chr>               <dbl> <dbl> <dbl> <dbl>
    ## 1 Cabo Verde            100  0.33 22.9     26
    ## 2 Cambodia              100  0.5  52.7     84
    ## 3 Djibouti             1000  0.35 28.1     33
    ## 4 New Zealand          1000  0.42 29.7     52
    ## 5 Trinidad and Tobago   100  0.32  9.73    37

``` r
slow_growth_overall =  peak_data %>% ungroup() %>% top_n(-5, b)
slow_growth_overall
```

    ## # A tibble: 9 x 5
    ##   region                    a     b     c max_t
    ##   <chr>                 <dbl> <dbl> <dbl> <dbl>
    ## 1 Brunei                  500  0.03  66.9    42
    ## 2 Dominica                100  0.03  74.9    26
    ## 3 Grenada                 100  0.03  82.1    26
    ## 4 MS Zaandam              100  0.04  74.3    23
    ## 5 Namibia                 100  0.04  72.5    37
    ## 6 Sao Tome and Principe   100  0.04  87.5    14
    ## 7 Seychelles              100  0.04  80.7    37
    ## 8 Suriname                100  0.04  83.5    37
    ## 9 Western Sahara          100  0.04  80.9    15

``` r
fast_growth = peak_data %>% filter(max_t < c) %>% ungroup()%>% top_n(5, b)
fast_growth 
```

    ## # A tibble: 6 x 5
    ##   region             a     b     c max_t
    ##   <chr>          <dbl> <dbl> <dbl> <dbl>
    ## 1 Bangladesh     10000  0.23  46.7    43
    ## 2 Sierra Leone 1000000  0.18  76.1    20
    ## 3 Somalia        50000  0.24  57.5    35
    ## 4 Sudan        1000000  0.18  89.8    38
    ## 5 Tanzania     1000000  0.2   76.8    35
    ## 6 Timor-Leste      100  0.23  30.9    26

``` r
slow_growth = peak_data %>% filter(max_t < c) %>% ungroup()%>% top_n(-5, b)
slow_growth 
```

    ## # A tibble: 9 x 5
    ##   region                    a     b     c max_t
    ##   <chr>                 <dbl> <dbl> <dbl> <dbl>
    ## 1 Brunei                  500  0.03  66.9    42
    ## 2 Dominica                100  0.03  74.9    26
    ## 3 Grenada                 100  0.03  82.1    26
    ## 4 MS Zaandam              100  0.04  74.3    23
    ## 5 Namibia                 100  0.04  72.5    37
    ## 6 Sao Tome and Principe   100  0.04  87.5    14
    ## 7 Seychelles              100  0.04  80.7    37
    ## 8 Suriname                100  0.04  83.5    37
    ## 9 Western Sahara          100  0.04  80.9    15
