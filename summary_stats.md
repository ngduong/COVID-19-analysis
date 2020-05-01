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
  summarise(`Mean days` = mean(t),
            `Median days` = median(t),
            `IQR` = IQR(t),
            `Max days` = max(t)) %>% 
  knitr::kable()
```

| Country/Region                   | Mean days | Median days |  IQR | Max days |
| :------------------------------- | --------: | ----------: | ---: | -------: |
| Afghanistan                      |      28.0 |        28.0 | 28.0 |       56 |
| Albania                          |      21.0 |        21.0 | 21.0 |       42 |
| Algeria                          |      27.5 |        27.5 | 27.5 |       55 |
| Andorra                          |      24.5 |        24.5 | 24.5 |       49 |
| Angola                           |      13.0 |        13.0 | 13.0 |       26 |
| Antigua and Barbuda              |      19.0 |        19.0 | 19.0 |       38 |
| Argentina                        |      24.0 |        24.0 | 24.0 |       48 |
| Armenia                          |      25.0 |        25.0 | 25.0 |       50 |
| Australia                        |      42.5 |        42.5 | 42.5 |       85 |
| Austria                          |      27.5 |        27.5 | 27.5 |       55 |
| Azerbaijan                       |      25.0 |        25.0 | 25.0 |       50 |
| Bahamas                          |      13.0 |        13.0 | 13.0 |       26 |
| Bahrain                          |      28.0 |        28.0 | 28.0 |       56 |
| Bangladesh                       |      21.5 |        21.5 | 21.5 |       43 |
| Barbados                         |      17.0 |        17.0 | 17.0 |       34 |
| Belarus                          |      26.0 |        26.0 | 26.0 |       52 |
| Belgium                          |      38.0 |        38.0 | 38.0 |       76 |
| Belize                           |      13.0 |        13.0 | 13.0 |       26 |
| Benin                            |      17.5 |        17.5 | 17.5 |       35 |
| Bhutan                           |      22.5 |        22.5 | 22.5 |       45 |
| Bolivia                          |      20.0 |        20.0 | 20.0 |       40 |
| Bosnia and Herzegovina           |      23.0 |        23.0 | 23.0 |       46 |
| Botswana                         |      10.5 |        10.5 | 10.5 |       21 |
| Brazil                           |      27.0 |        27.0 | 27.0 |       54 |
| Brunei                           |      21.0 |        21.0 | 21.0 |       42 |
| Bulgaria                         |      21.5 |        21.5 | 21.5 |       43 |
| Burkina Faso                     |      20.5 |        20.5 | 20.5 |       41 |
| Burma                            |      12.0 |        12.0 | 12.0 |       24 |
| Burundi                          |      10.0 |        10.0 | 10.0 |       20 |
| Cabo Verde                       |      13.0 |        13.0 | 13.0 |       26 |
| Cambodia                         |      42.0 |        42.0 | 42.0 |       84 |
| Cameroon                         |      13.0 |        13.0 | 13.0 |       26 |
| Canada                           |      42.5 |        42.5 | 42.5 |       85 |
| Central African Republic         |      18.0 |        18.0 | 18.0 |       36 |
| Chad                             |      13.0 |        13.0 | 13.0 |       26 |
| Chile                            |      24.0 |        24.0 | 24.0 |       48 |
| China                            |      44.5 |        44.5 | 44.5 |       89 |
| Colombia                         |      22.5 |        22.5 | 22.5 |       45 |
| Congo (Brazzaville)              |      18.0 |        18.0 | 18.0 |       36 |
| Congo (Kinshasa)                 |      20.0 |        20.0 | 20.0 |       40 |
| Costa Rica                       |      22.5 |        22.5 | 22.5 |       45 |
| Cote d’Ivoire                    |      20.0 |        20.0 | 20.0 |       40 |
| Croatia                          |      27.5 |        27.5 | 27.5 |       55 |
| Cuba                             |      19.5 |        19.5 | 19.5 |       39 |
| Cyprus                           |      21.0 |        21.0 | 21.0 |       42 |
| Czechia                          |      13.0 |        13.0 | 13.0 |       26 |
| Denmark                          |      26.5 |        26.5 | 26.5 |       53 |
| Diamond Princess                 |      13.0 |        13.0 | 13.0 |       26 |
| Djibouti                         |      16.5 |        16.5 | 16.5 |       33 |
| Dominica                         |      13.0 |        13.0 | 13.0 |       26 |
| Dominican Republic               |      25.0 |        25.0 | 25.0 |       50 |
| Ecuador                          |      25.0 |        25.0 | 25.0 |       50 |
| Egypt                            |      33.0 |        33.0 | 33.0 |       66 |
| El Salvador                      |      13.0 |        13.0 | 13.0 |       26 |
| Equatorial Guinea                |      18.0 |        18.0 | 18.0 |       36 |
| Eritrea                          |      13.0 |        13.0 | 13.0 |       26 |
| Estonia                          |      26.5 |        26.5 | 26.5 |       53 |
| Eswatini                         |      18.5 |        18.5 | 18.5 |       37 |
| Ethiopia                         |      19.0 |        19.0 | 19.0 |       38 |
| Fiji                             |      13.0 |        13.0 | 13.0 |       26 |
| Finland                          |      41.0 |        41.0 | 41.0 |       82 |
| France                           |      43.5 |        43.5 | 43.5 |       87 |
| Gabon                            |      18.5 |        18.5 | 18.5 |       37 |
| Gambia                           |      13.0 |        13.0 | 13.0 |       26 |
| Georgia                          |      27.0 |        27.0 | 27.0 |       54 |
| Germany                          |      42.0 |        42.0 | 42.0 |       84 |
| Ghana                            |      18.5 |        18.5 | 18.5 |       37 |
| Greece                           |      27.0 |        27.0 | 27.0 |       54 |
| Grenada                          |      13.0 |        13.0 | 13.0 |       26 |
| Guatemala                        |      18.5 |        18.5 | 18.5 |       37 |
| Guinea                           |      19.0 |        19.0 | 19.0 |       38 |
| Guinea-Bissau                    |      13.0 |        13.0 | 13.0 |       26 |
| Guyana                           |      19.5 |        19.5 | 19.5 |       39 |
| Haiti                            |      13.0 |        13.0 | 13.0 |       26 |
| Holy See                         |      22.5 |        22.5 | 22.5 |       45 |
| Honduras                         |      20.0 |        20.0 | 20.0 |       40 |
| Hungary                          |      23.5 |        23.5 | 23.5 |       47 |
| Iceland                          |      26.0 |        26.0 | 26.0 |       52 |
| India                            |      40.5 |        40.5 | 40.5 |       81 |
| Indonesia                        |      24.5 |        24.5 | 24.5 |       49 |
| Iran                             |      30.5 |        30.5 | 30.5 |       61 |
| Iraq                             |      28.0 |        28.0 | 28.0 |       56 |
| Ireland                          |      25.5 |        25.5 | 25.5 |       51 |
| Israel                           |      29.5 |        29.5 | 29.5 |       59 |
| Italy                            |      40.0 |        40.0 | 40.0 |       80 |
| Jamaica                          |      20.0 |        20.0 | 20.0 |       40 |
| Japan                            |      44.5 |        44.5 | 44.5 |       89 |
| Jordan                           |      24.0 |        24.0 | 24.0 |       48 |
| Kazakhstan                       |      19.0 |        19.0 | 19.0 |       38 |
| Kenya                            |      19.0 |        19.0 | 19.0 |       38 |
| Korea, South                     |      44.5 |        44.5 | 44.5 |       89 |
| Kosovo                           |      12.5 |        12.5 | 12.5 |       25 |
| Kuwait                           |      28.0 |        28.0 | 28.0 |       56 |
| Kyrgyzstan                       |      16.5 |        16.5 | 16.5 |       33 |
| Laos                             |      13.0 |        13.0 | 13.0 |       26 |
| Latvia                           |      24.5 |        24.5 | 24.5 |       49 |
| Lebanon                          |      29.5 |        29.5 | 29.5 |       59 |
| Liberia                          |      17.5 |        17.5 | 17.5 |       35 |
| Libya                            |      13.0 |        13.0 | 13.0 |       26 |
| Liechtenstein                    |      23.5 |        23.5 | 23.5 |       47 |
| Lithuania                        |      26.0 |        26.0 | 26.0 |       52 |
| Luxembourg                       |      25.5 |        25.5 | 25.5 |       51 |
| Madagascar                       |      13.0 |        13.0 | 13.0 |       26 |
| Malawi                           |       9.0 |         9.0 |  9.0 |       18 |
| Malaysia                         |      43.0 |        43.0 | 43.0 |       86 |
| Maldives                         |      21.5 |        21.5 | 21.5 |       43 |
| Mali                             |      13.0 |        13.0 | 13.0 |       26 |
| Malta                            |      22.0 |        22.0 | 22.0 |       44 |
| Martinique                       |       8.5 |         8.5 |  8.5 |       17 |
| Mauritania                       |      18.5 |        18.5 | 18.5 |       37 |
| Mauritius                        |      16.5 |        16.5 | 16.5 |       33 |
| Mexico                           |      26.0 |        26.0 | 26.0 |       52 |
| Moldova                          |      21.5 |        21.5 | 21.5 |       43 |
| Monaco                           |      25.5 |        25.5 | 25.5 |       51 |
| Mongolia                         |      20.5 |        20.5 | 20.5 |       41 |
| Montenegro                       |      17.0 |        17.0 | 17.0 |       34 |
| Morocco                          |      24.5 |        24.5 | 24.5 |       49 |
| Mozambique                       |      13.0 |        13.0 | 13.0 |       26 |
| MS Zaandam                       |      11.5 |        11.5 | 11.5 |       23 |
| Namibia                          |      18.5 |        18.5 | 18.5 |       37 |
| Nepal                            |      43.0 |        43.0 | 43.0 |       86 |
| Netherlands                      |      26.5 |        26.5 | 26.5 |       53 |
| New Zealand                      |      26.0 |        26.0 | 26.0 |       52 |
| Nicaragua                        |      13.0 |        13.0 | 13.0 |       26 |
| Niger                            |      13.0 |        13.0 | 13.0 |       26 |
| Nigeria                          |      26.0 |        26.0 | 26.0 |       52 |
| North Macedonia                  |      27.0 |        27.0 | 27.0 |       54 |
| Norway                           |      27.0 |        27.0 | 27.0 |       54 |
| Oman                             |      28.0 |        28.0 | 28.0 |       56 |
| Pakistan                         |      27.0 |        27.0 | 27.0 |       54 |
| Panama                           |      20.5 |        20.5 | 20.5 |       41 |
| Papua New Guinea                 |      13.0 |        13.0 | 13.0 |       26 |
| Paraguay                         |      21.5 |        21.5 | 21.5 |       43 |
| Peru                             |      22.5 |        22.5 | 22.5 |       45 |
| Philippines                      |      40.5 |        40.5 | 40.5 |       81 |
| Poland                           |      23.5 |        23.5 | 23.5 |       47 |
| Portugal                         |      24.5 |        24.5 | 24.5 |       49 |
| Qatar                            |      25.5 |        25.5 | 25.5 |       51 |
| Romania                          |      27.0 |        27.0 | 27.0 |       54 |
| Russia                           |      40.0 |        40.0 | 40.0 |       80 |
| Rwanda                           |      18.5 |        18.5 | 18.5 |       37 |
| Saint Kitts and Nevis            |      13.0 |        13.0 | 13.0 |       26 |
| Saint Lucia                      |      18.5 |        18.5 | 18.5 |       37 |
| Saint Vincent and the Grenadines |      18.5 |        18.5 | 18.5 |       37 |
| San Marino                       |      26.5 |        26.5 | 26.5 |       53 |
| Sao Tome and Principe            |       7.0 |         7.0 |  7.0 |       14 |
| Saudi Arabia                     |      24.5 |        24.5 | 24.5 |       49 |
| Senegal                          |      24.5 |        24.5 | 24.5 |       49 |
| Serbia                           |      22.5 |        22.5 | 22.5 |       45 |
| Seychelles                       |      18.5 |        18.5 | 18.5 |       37 |
| Sierra Leone                     |      10.0 |        10.0 | 10.0 |       20 |
| Singapore                        |      44.0 |        44.0 | 44.0 |       88 |
| Slovakia                         |      22.5 |        22.5 | 22.5 |       45 |
| Slovenia                         |      23.0 |        23.0 | 23.0 |       46 |
| Somalia                          |      17.5 |        17.5 | 17.5 |       35 |
| South Africa                     |      23.0 |        23.0 | 23.0 |       46 |
| South Sudan                      |       7.5 |         7.5 |  7.5 |       15 |
| Spain                            |      39.5 |        39.5 | 39.5 |       79 |
| Sri Lanka                        |      42.0 |        42.0 | 42.0 |       84 |
| Sudan                            |      19.0 |        19.0 | 19.0 |       38 |
| Suriname                         |      18.5 |        18.5 | 18.5 |       37 |
| Sweden                           |      40.0 |        40.0 | 40.0 |       80 |
| Switzerland                      |      27.5 |        27.5 | 27.5 |       55 |
| Syria                            |      13.0 |        13.0 | 13.0 |       26 |
| Taiwan\*                         |      44.5 |        44.5 | 44.5 |       89 |
| Tanzania                         |      17.5 |        17.5 | 17.5 |       35 |
| Thailand                         |      44.5 |        44.5 | 44.5 |       89 |
| Timor-Leste                      |      13.0 |        13.0 | 13.0 |       26 |
| Togo                             |      22.5 |        22.5 | 22.5 |       45 |
| Trinidad and Tobago              |      18.5 |        18.5 | 18.5 |       37 |
| Tunisia                          |      23.5 |        23.5 | 23.5 |       47 |
| Turkey                           |      20.0 |        20.0 | 20.0 |       40 |
| Uganda                           |      13.0 |        13.0 | 13.0 |       26 |
| Ukraine                          |      24.0 |        24.0 | 24.0 |       48 |
| United Arab Emirates             |      41.0 |        41.0 | 41.0 |       82 |
| United Kingdom                   |      40.0 |        40.0 | 40.0 |       80 |
| Uruguay                          |      18.5 |        18.5 | 18.5 |       37 |
| US                               |      28.0 |        28.0 | 28.0 |       56 |
| Uzbekistan                       |      18.0 |        18.0 | 18.0 |       36 |
| Venezuela                        |      18.5 |        18.5 | 18.5 |       37 |
| Vietnam                          |      44.0 |        44.0 | 44.0 |       88 |
| West Bank and Gaza               |      12.5 |        12.5 | 12.5 |       25 |
| Western Sahara                   |       7.5 |         7.5 |  7.5 |       15 |
| Yemen                            |       5.0 |         5.0 |  5.0 |       10 |
| Zambia                           |      16.5 |        16.5 | 16.5 |       33 |
| Zimbabwe                         |      13.0 |        13.0 | 13.0 |       26 |

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
