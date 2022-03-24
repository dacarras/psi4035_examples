Paper: Rehabilitating civilian victims of war
================

# Paper

-   Title: “Rehabilitating civilian victims of war through psychosocial
    intervention in Sierra Leone”

-   References

Mughal, U., Carrasco, D., Brown, R., & Ayers, S. (2015). Rehabilitating
civilian victims of war through psychosocial intervention in Sierra
Leone. Journal of Applied Social Psychology, 45(11), 593–601.
<https://doi.org/10.1111/jasp.12322>

# Agregar puntajes de escalas

``` r
# -----------------------------------------------------------------------------
# mean scores
# -----------------------------------------------------------------------------

# -----------------------------------------------
# open csv
# -----------------------------------------------

data_model <- readr::read_csv(
    url(
        'https://raw.githubusercontent.com/dacarras/psi4035_examples/master/data/ptsd_data.csv'
        )
    )
```

    ## Rows: 100 Columns: 62
    ## ── Column specification ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (2): cond, nat
    ## dbl (60): id_i, ie01, ie02, ie03, ie04, ie05, ie06, ie07, ie08, ie09, ie10, ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# -----------------------------------------------
# scoring functions
# -----------------------------------------------

mean_score <- function(..., na.rm=TRUE){
rowMeans(cbind(...), na.rm=na.rm)
# source: https://stackoverflow.com/questions/33401788/dplyr-using-mutate-like-rowmeans
# by: https://stackoverflow.com/users/1191259/frank
}


reverse <- function(var){
# remove labels
var <- labelled::remove_labels(var)
var <- haven::zap_labels(var)

# get max and min of vector
max <- max(var, na.rm = TRUE)
min <- min(var, na.rm = TRUE)

# produced reverse score
return(max + min - var)
}



# -----------------------------------------------
# measures presented in the paper
# -----------------------------------------------

data_ptsd <- data_model %>%
              # PTSD
              mutate(ptsd = mean_score(
              ie01, # Any reminder brought back feelings about it
              ie02, # I had trouble staying asleep
              ie03, # Other things kept making me think about it
              ie04, # I felt irritable and angry
              ie05, # I avoided letting myself get upset when I thought about it or was reminded of it
              ie06, # I thought about it when I didn't mean to
              ie07, # I felt as if it hadn't happened or wasn't real.
              ie08, # Pictures about it popped into my head
              ie09, # I was jumpy and easily startled
              ie10, # I was aware that I still had a lot of feelings about it, but I didn't deal with them
              ie11, # I found myself acting or feeling like I was back at that time
              ie12, # I had waves of strong feelings about it
              ie14, # I had trouble concentrating
              ie15, # Reminders of it caused me to have physical reactions such as sweating, trouble breathing, nausea or a pounding heart
              ie16  # I felt watchful and on-guard
                )) %>%
              # Intergroup anxiety
              ## create reverse scores
              mutate(anx1_r = reverse(anx1)) %>%
              mutate(anx4_r = reverse(anx4)) %>%
              ## mean score
              mutate(anxi = mean_score(
              anx1_r  ,  # [R] Relaxed
              anx2    ,  # Threatened
              anx3    ,  # Awkward
              anx4_r  ,  # [R] Safe
              anx5    ,  # Nervous
              anx6       # Anxious
              )) %>%
              # Out-group blame
              mutate(outb = mean_score(
              noe6,      # I think that the rebels are entirely to blame for what they have done during the war 
              noe7       # I think that the rebels are responsible for everything they did
              )) %>%
              # Intergroup forgiveness
              mutate(ifor = mean_score(
              for6,  # I think my group should reach out to the rebels and forgive them what they have done
              for5   # I should forgive the rebels their misdeeds              
              )) %>%
              # In-group (National) Identification
              mutate(iden = mean_score(
              ide1,  # I am proud to be a Sierra Leonine
              ide2   # I have very strong ties with Sierra Leone
              )) %>%
              # Out-group contact
              mutate(cont = con) %>%
              # Personal War Trauma Experience
              mutate(wart = mean_score(
              war2,  # I have seen dead people
              war3,  # I have lost people of my family in the war
              war4,  # I was attacked
              war5,  # I have seen how people were killed
              war6   # I have been fighting                
              )) %>%
              # dummy variables
              ## sex
              mutate(sex_original = sex) %>%
              dplyr::select(-sex) %>%
              mutate(sex = case_when(
                sex_original == 1 ~ 1, # male
                sex_original == 2 ~ 0  # female
                )) %>%
              ## employment
              mutate(emp_original = emp) %>%
              dplyr::select(-emp) %>%
              mutate(emp = case_when(
                emp_original == 1 ~ 0, # unemployed
                emp_original == 2 ~ 1  # employed
                )) %>%
              ## marital status
              mutate(mar_original = mar) %>%
              dplyr::select(-mar) %>%
              mutate(mar = case_when(
                mar_original == 1 ~ 1, # married
                mar_original == 2 ~ 0  # not married
                )) %>%
              ## religion
              mutate(rel_original = rel) %>%
              dplyr::select(-rel) %>%
              mutate(rel = case_when(
                rel_original == 1 ~ 0, # muslim
                TRUE ~ 1               # non-muslim
                )) %>%
              ## treated
              mutate(trt = case_when(
                cond == 'c1' ~ 0, # delayed control
                cond == 't1' ~ 1  # treated
                )) %>%
              dplyr::glimpse()
```

    ## Rows: 100
    ## Columns: 76
    ## $ id_i         <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17…
    ## $ cond         <chr> "c1", "c1", "c1", "c1", "c1", "c1", "c1", "c1", "c1", "c1…
    ## $ ie01         <dbl> 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 4, 5, 5, 5, 5, …
    ## $ ie02         <dbl> 5, 4, 2, 5, 5, 5, 1, 1, 4, 5, 1, 5, 3, 4, 4, 5, 1, 2, 4, …
    ## $ ie03         <dbl> 4, 3, 4, 5, 5, 5, 4, 4, 3, 5, 5, 5, 5, 3, 4, 5, 5, 5, 5, …
    ## $ ie04         <dbl> 5, 4, 4, 5, 3, 5, 3, 3, 4, 5, 5, 5, 5, 5, 5, 4, 5, 5, 5, …
    ## $ ie05         <dbl> 4, 4, 5, 5, 5, 5, 4, 4, 3, 5, 5, 1, 1, 2, 3, 4, 5, 5, 4, …
    ## $ ie06         <dbl> 4, 4, 5, 5, 5, 2, 3, 4, 4, 5, 4, 3, 5, 3, 5, 4, 3, 5, 4, …
    ## $ ie07         <dbl> 5, 4, 4, 1, 3, 4, 5, 3, 4, 5, 5, 3, 5, 1, 1, 1, 3, 1, 5, …
    ## $ ie08         <dbl> 5, 4, 1, 5, 5, 5, 4, 2, 3, 5, 5, 4, 3, 5, 5, 3, 5, 5, 4, …
    ## $ ie09         <dbl> 5, 5, 2, 5, 5, 5, 3, 3, 2, 5, 5, 4, 3, 5, 5, 1, 5, 3, 2, …
    ## $ ie10         <dbl> 4, 3, 2, 1, 4, 4, 1, 4, 4, 2, 1, 4, 3, 5, 5, 5, 5, 2, 3, …
    ## $ ie11         <dbl> 4, 4, 2, 5, 5, 4, 1, 2, 1, 5, 5, 4, 1, 2, 1, 3, 1, 4, 1, …
    ## $ ie12         <dbl> 5, 3, 2, 5, 3, 5, 5, 5, 4, 5, 5, 4, 5, 5, 5, 5, 5, 4, 3, …
    ## $ ie13         <dbl> 3, 4, 5, 3, 2, 5, 3, 4, 3, 1, 1, 2, 4, 4, 5, 4, 4, 4, 4, …
    ## $ ie14         <dbl> 4, 3, 1, 5, 3, 5, 1, 1, 1, 5, 5, 5, 4, 2, 1, 4, 2, 4, 3, …
    ## $ ie15         <dbl> 5, 3, 1, 5, 4, 5, 3, 2, 3, 1, 5, 4, 4, 2, 3, 3, 2, 5, 4, …
    ## $ ie16         <dbl> 4, 4, 1, 5, 4, 5, 2, 4, 4, 5, 5, 5, 4, 3, 5, 4, 2, 5, 3, …
    ## $ ie17         <dbl> 4, 3, 1, 1, 4, 5, 4, 5, 3, 1, 1, 2, 4, 5, 5, 5, 5, 5, 3, …
    ## $ ptc1         <dbl> 3, 3, 1, 2, 4, 1, 3, 3, 4, 4, 1, 4, 2, 5, 1, 2, 5, 2, 4, …
    ## $ ptc2         <dbl> 2, 4, 1, 1, 4, 2, 3, 4, 5, 1, 5, 4, 4, 1, 1, 1, 2, 2, 5, …
    ## $ ide1         <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 1, 1, 5, 5, 5, 1, 5, 5, 2, …
    ## $ ide2         <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 1, 5, 5, 5, 5, 5, 5, 4, …
    ## $ noe1         <dbl> 5, 5, 5, 5, 5, 2, 4, 5, 3, 1, 5, 5, 5, 5, 5, 5, 4, 5, 2, …
    ## $ noe2         <dbl> 5, 4, 5, 4, 5, 5, 3, 3, 1, 1, 5, 5, 5, 5, 5, 5, 3, 5, 1, …
    ## $ for1         <dbl> 4, 4, 5, 4, 3, 1, 4, 4, 3, 1, 1, 4, 4, 1, 3, 5, 3, 4, 3, …
    ## $ for2         <dbl> 3, 4, 5, 5, 3, 1, 2, 5, 4, 5, 1, 4, 2, 1, 3, 5, 4, 2, 5, …
    ## $ for3         <dbl> 3, 4, 2, 3, 3, 1, 4, 5, 3, 1, 5, 4, 4, 1, 3, 3, 4, 2, 4, …
    ## $ for4         <dbl> 3, 4, 1, 3, 4, 1, 4, 3, 4, 5, 2, 1, 2, 4, 4, 2, 4, 4, 3, …
    ## $ for5         <dbl> 3, 5, 1, 4, 4, 1, 4, 3, 4, 5, 2, 1, 4, 4, 4, 2, 4, 4, 2, …
    ## $ con          <dbl> 3, 5, 1, 3, 3, 1, 3, 3, 3, 5, 1, 1, 4, 1, 1, 2, 3, 1, 1, …
    ## $ noe3         <dbl> 3, 4, 1, 3, 4, 2, 3, 4, 3, 5, 5, 5, 4, 5, 1, 2, 1, 5, 3, …
    ## $ noe4         <dbl> 4, 3, 5, 4, 4, 5, 4, 2, 4, 5, 5, 5, 3, 2, 1, 1, 1, 4, 5, …
    ## $ noe5         <dbl> 4, 3, 1, 1, 5, 5, 3, 3, 1, 1, 5, 5, 4, 4, 5, 1, 3, 5, 5, …
    ## $ for6         <dbl> 4, 3, 1, 4, 5, 5, 2, 3, 3, 5, 5, 1, 4, 4, 4, 1, 4, 4, 3, …
    ## $ noe6         <dbl> 5, 5, 1, 5, 4, 1, 3, 5, 3, 5, 1, 5, 4, 4, 1, 1, 2, 4, 2, …
    ## $ noe7         <dbl> 5, 5, 1, 5, 5, 1, 2, 5, 4, 5, 1, 5, 4, 1, 1, 5, 4, 5, 4, …
    ## $ noe8         <dbl> 2, 3, 3, 4, 2, 4, 4, 2, 5, 1, 1, 5, 1, 1, 3, 2, 3, 1, 3, …
    ## $ anx1         <dbl> 2, 4, 5, 3, 4, 1, 4, 2, 1, 5, 1, 1, 4, 5, 3, 2, 5, 1, 4, …
    ## $ anx2         <dbl> 4, 2, 1, 1, 2, 5, 1, 5, 1, 1, 5, 5, 1, 1, 3, 4, 1, 5, 4, …
    ## $ anx3         <dbl> 4, 3, 3, 1, 4, 5, 3, 5, 2, 1, 5, 5, 1, 1, 1, 5, 1, 1, 2, …
    ## $ anx4         <dbl> 3, 4, 3, 5, 4, 1, 4, 2, 4, 5, 1, 1, 1, 1, 3, 1, 5, 5, 3, …
    ## $ anx5         <dbl> 4, 2, 4, 2, 4, 5, 1, 5, 1, 1, 5, 5, 5, 5, 1, 1, 1, 1, 5, …
    ## $ anx6         <dbl> 5, 1, 1, 4, 4, 5, 1, 2, 5, 1, 5, 1, 5, 1, 1, 5, 5, 1, 4, …
    ## $ war1         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ war2         <dbl> 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, …
    ## $ war3         <dbl> 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, …
    ## $ war4         <dbl> 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, …
    ## $ war5         <dbl> 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, …
    ## $ war6         <dbl> 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, …
    ## $ war7         <dbl> 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, …
    ## $ war8         <dbl> 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, …
    ## $ ie18         <dbl> 2, 1, 2, 4, 3, 4, 3, 5, 3, 1, 3, 3, 3, 2, 3, 1, 3, 2, 3, …
    ## $ fut1         <dbl> 2, 2, 1, 2, 2, 2, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, …
    ## $ fut2         <dbl> 3, 5, 5, 4, 2, 4, 5, 3, 5, 5, 5, 5, 4, 5, 5, 1, 5, 4, 2, …
    ## $ age          <dbl> 47, 38, 34, 65, 28, 37, 43, 45, 27, 22, 15, 18, 27, 25, 3…
    ## $ nat          <chr> "TAM", "MAK", "SLE", "MAP", "MAK", "MAK", "SLE", "MAK", "…
    ## $ ski          <dbl> 1, 1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 1, 2, 2, 2, …
    ## $ ptsd         <dbl> 4.466667, 3.733333, 2.666667, 4.466667, 4.266667, 4.60000…
    ## $ anx1_r       <dbl> 4, 2, 1, 3, 2, 5, 2, 4, 5, 1, 5, 5, 2, 1, 3, 4, 1, 5, 2, …
    ## $ anx4_r       <dbl> 3, 2, 3, 1, 2, 5, 2, 4, 2, 1, 5, 5, 5, 5, 3, 5, 1, 1, 3, …
    ## $ anxi         <dbl> 4.000000, 2.000000, 2.166667, 2.000000, 3.000000, 5.00000…
    ## $ outb         <dbl> 5.0, 5.0, 1.0, 5.0, 4.5, 1.0, 2.5, 5.0, 3.5, 5.0, 1.0, 5.…
    ## $ ifor         <dbl> 3.5, 4.0, 1.0, 4.0, 4.5, 3.0, 3.0, 3.0, 3.5, 5.0, 3.5, 1.…
    ## $ iden         <dbl> 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 3.0, 1.…
    ## $ cont         <dbl> 3, 5, 1, 3, 3, 1, 3, 3, 3, 5, 1, 1, 4, 1, 1, 2, 3, 1, 1, …
    ## $ wart         <dbl> 0.4, 1.0, 1.0, 0.8, 0.6, 0.2, 0.0, 0.4, 0.8, 0.0, 0.4, 0.…
    ## $ sex_original <dbl> 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 2, 1, 2, 2, 2, 1, 1, 2, …
    ## $ sex          <dbl> 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, …
    ## $ emp_original <dbl> 2, 2, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 2, 1, 2, 1, 1, …
    ## $ emp          <dbl> 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, …
    ## $ mar_original <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 1, 1, …
    ## $ mar          <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, …
    ## $ rel_original <dbl> 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 1, 1, 2, 1, 1, 1, 2, 1, 1, …
    ## $ rel          <dbl> 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, …
    ## $ trt          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …

``` r
# -----------------------------------------------
# add labels
# -----------------------------------------------

data_ptsd <- data_ptsd %>%
labelled::set_variable_labels(
ptsd   = 'post traumatic stress disorder',
anx1_r = '[R] Relaxed',
anx4_r = '[R] Safe',
anxi   = 'Anxiety',
outb   = 'Outgroup blame',
ifor   = 'Intergroup forgiveness',
iden   = 'Ingroup (national) identification',
cont   = 'Outgroup contact',
wart   = 'War trauma',
sex    = 'Sex (1 = male, 0 = female)',
emp    = 'Employment Status (1 = employed, 0 = unemployed)',
mar    = 'Marital Status (1 = married, 0 = not married',
rel    = 'Religion (1 = not muslim, 0 = muslim)',
trt    = 'Treated (1 = treated, 0 = not treated'
)


# -----------------------------------------------
# chequear value labels
# -----------------------------------------------

data_ptsd %>%
labelled::look_for() %>%
labelled::lookfor_to_long_format() %>%
tibble::as_tibble() %>%
knitr::kable()
```

| pos | variable     | label                                            | col_type | levels | value_labels |
|----:|:-------------|:-------------------------------------------------|:---------|:-------|:-------------|
|   1 | id_i         |                                                  | dbl      |        |              |
|   2 | cond         |                                                  | chr      |        |              |
|   3 | ie01         |                                                  | dbl      |        |              |
|   4 | ie02         |                                                  | dbl      |        |              |
|   5 | ie03         |                                                  | dbl      |        |              |
|   6 | ie04         |                                                  | dbl      |        |              |
|   7 | ie05         |                                                  | dbl      |        |              |
|   8 | ie06         |                                                  | dbl      |        |              |
|   9 | ie07         |                                                  | dbl      |        |              |
|  10 | ie08         |                                                  | dbl      |        |              |
|  11 | ie09         |                                                  | dbl      |        |              |
|  12 | ie10         |                                                  | dbl      |        |              |
|  13 | ie11         |                                                  | dbl      |        |              |
|  14 | ie12         |                                                  | dbl      |        |              |
|  15 | ie13         |                                                  | dbl      |        |              |
|  16 | ie14         |                                                  | dbl      |        |              |
|  17 | ie15         |                                                  | dbl      |        |              |
|  18 | ie16         |                                                  | dbl      |        |              |
|  19 | ie17         |                                                  | dbl      |        |              |
|  20 | ptc1         |                                                  | dbl      |        |              |
|  21 | ptc2         |                                                  | dbl      |        |              |
|  22 | ide1         |                                                  | dbl      |        |              |
|  23 | ide2         |                                                  | dbl      |        |              |
|  24 | noe1         |                                                  | dbl      |        |              |
|  25 | noe2         |                                                  | dbl      |        |              |
|  26 | for1         |                                                  | dbl      |        |              |
|  27 | for2         |                                                  | dbl      |        |              |
|  28 | for3         |                                                  | dbl      |        |              |
|  29 | for4         |                                                  | dbl      |        |              |
|  30 | for5         |                                                  | dbl      |        |              |
|  31 | con          |                                                  | dbl      |        |              |
|  32 | noe3         |                                                  | dbl      |        |              |
|  33 | noe4         |                                                  | dbl      |        |              |
|  34 | noe5         |                                                  | dbl      |        |              |
|  35 | for6         |                                                  | dbl      |        |              |
|  36 | noe6         |                                                  | dbl      |        |              |
|  37 | noe7         |                                                  | dbl      |        |              |
|  38 | noe8         |                                                  | dbl      |        |              |
|  39 | anx1         |                                                  | dbl      |        |              |
|  40 | anx2         |                                                  | dbl      |        |              |
|  41 | anx3         |                                                  | dbl      |        |              |
|  42 | anx4         |                                                  | dbl      |        |              |
|  43 | anx5         |                                                  | dbl      |        |              |
|  44 | anx6         |                                                  | dbl      |        |              |
|  45 | war1         |                                                  | dbl      |        |              |
|  46 | war2         |                                                  | dbl      |        |              |
|  47 | war3         |                                                  | dbl      |        |              |
|  48 | war4         |                                                  | dbl      |        |              |
|  49 | war5         |                                                  | dbl      |        |              |
|  50 | war6         |                                                  | dbl      |        |              |
|  51 | war7         |                                                  | dbl      |        |              |
|  52 | war8         |                                                  | dbl      |        |              |
|  53 | ie18         |                                                  | dbl      |        |              |
|  54 | fut1         |                                                  | dbl      |        |              |
|  55 | fut2         |                                                  | dbl      |        |              |
|  56 | age          |                                                  | dbl      |        |              |
|  57 | nat          |                                                  | chr      |        |              |
|  58 | ski          |                                                  | dbl      |        |              |
|  59 | ptsd         | post traumatic stress disorder                   | dbl      |        |              |
|  60 | anx1_r       | \[R\] Relaxed                                    | dbl      |        |              |
|  61 | anx4_r       | \[R\] Safe                                       | dbl      |        |              |
|  62 | anxi         | Anxiety                                          | dbl      |        |              |
|  63 | outb         | Outgroup blame                                   | dbl      |        |              |
|  64 | ifor         | Intergroup forgiveness                           | dbl      |        |              |
|  65 | iden         | Ingroup (national) identification                | dbl      |        |              |
|  66 | cont         | Outgroup contact                                 | dbl      |        |              |
|  67 | wart         | War trauma                                       | dbl      |        |              |
|  68 | sex_original |                                                  | dbl      |        |              |
|  69 | sex          | Sex (1 = male, 0 = female)                       | dbl      |        |              |
|  70 | emp_original |                                                  | dbl      |        |              |
|  71 | emp          | Employment Status (1 = employed, 0 = unemployed) | dbl      |        |              |
|  72 | mar_original |                                                  | dbl      |        |              |
|  73 | mar          | Marital Status (1 = married, 0 = not married     | dbl      |        |              |
|  74 | rel_original |                                                  | dbl      |        |              |
|  75 | rel          | Religion (1 = not muslim, 0 = muslim)            | dbl      |        |              |
|  76 | trt          | Treated (1 = treated, 0 = not treated            | dbl      |        |              |
