# setwd
setwd('C:/Users/User/Desktop/quantitative_regional_economics_project')

# packages
library(readxl)
library(dplyr)
library(RColorBrewer)
library(car)
library(splm)

# clean data and join
ls_files <- list.files(paste(getwd(),'final_dataset', sep = '/'))

data_cleaning <- function(wd, ls_files_name) {
  file <- read_excel(paste(wd,'final_dataset',ls_files_name, sep = '/'))
  old.colname <- colnames(file)
  old.colname <- gsub("\\.\\.\\..*", "", old.colname)
  first.row.year <- as.character(file[c(1),])
  first.row.year[is.na(first.row.year)] <- ""
  new.colname <- paste(old.colname, first.row.year, sep = "_")
  new.colname[1:3] <- gsub("_", "", new.colname[1:3])
  new.colname <- gsub(" ", "_", new.colname )
  new.colname <- gsub("-", "", new.colname )
  new.colname <- gsub("²", "2", new.colname )
  new.colname <- tolower(new.colname)
  colnames(file) <- new.colname
  file <- file[c(2:nrow(file)),]
  colnames(file)[1:3] <- c('id', 'space_unit', 'aggregate')
  num.part <- as.data.frame(apply(file[,c(4:ncol(file))], 2, as.numeric))
  str.part <- file[,c(1:3)]
  output.ds <- cbind(str.part, num.part)
}

# upload all files
ls_results_ds <- sapply(ls_files, function(i) data_cleaning(getwd(), i))

# merge datasets
merge.ds <- merged_dataset <- Reduce(function(x, y) merge(x, y, by = c('id', 'space_unit', 'aggregate')), ls_results_ds)

# estimate nan values per column
colSums(is.na(merge.ds))

# filter dataset based on year and specific features
col.selected <- colnames(merge.ds)[grep("id|space_unit|aggregate|2016|2017|2018|2019|2020", colnames(merge.ds))]
# y ~ Xs
features.names.filtered <- tolower(c("Bruttoinlandsprodukt", 
                                     "Beschäftigte", 
                                     "Kleinstbetriebe", "Kleinbetriebe", "Mittlere", "Großunternehmen",
                                     "Einwohner_von",
                                     "Haushalte",
                                     "Siedlungsdichte",
                                     "Ausgaben") )

col.selected.w.year.name <- col.selected[grep(
  paste( "id|space_unit|aggregate|",paste(features.names.filtered, collapse = "|"),  collapse = "|",sep = ''), 
  col.selected)]

# final dataset
ds.final <- merge.ds[,as.vector(col.selected.w.year.name)]
ds.final$id <- as.numeric(ds.final$id)
dim(ds.final)
colSums(is.na(ds.final))

# ausgaben_für_sachinvestitionen = expenditure on physical investments -- Expenditure on physical investments in € per inhabitant
# beschäftigte_am_ao_mit berufsabschluss = employed_am_ao_with vocational qualification %
# beschäftigte_am_ao_ohne_berufsabschluss = employed_am_ao_without vocational qualification %
# beschäftigte_in_it_und_naturwissenschaftlichen_dienstleistungsberufen = employees in it and scientific service occupations
# beschäftigtenquote = employment rate
# beschäftigtenquote_frauen = women employment rate
# beschäftigtenquote_männer = man employment rate
# bruttoinlandsprodukt_in_1000_euro = gross domestic product_in_1000_euro  -- 1000
# einwohner_von_50_bis_unter_65_jahren = residents_from_50_to_under_65_years
# großunternehmen = big company (4)
# kleinbetriebe = small businesses (2)
# kleinstbetriebe = micro-enterprises (1)
# mittlere_unternehmen = medium_enterprises (3)
# haushalte_mit_mittlerem_einkommen_2018 = households_with_middle_income_2018 -- %
# siedlungsdichte_in_km²_2019 = settlement_density_in_km²_2019 -- Inhabitants per km² settlement and transport area - expenditure_for_tangible_investments_miles




##-------------------------------
# make geofile
##-------------------------------

#library(rgdal)
#library(broom)
#shapefile <- readOGR("other/vg2500_krs/", "vg2500_krs")
#shapedata <- tidy(shapefile, region="ARS")
#shapedata$id <- as.numeric(shapedata$id)
#geoinkar <- merge(shapedata, inkar3, by.x="id", by.y="ID", all.x=T)
#geoinkar <- geoinkar[order(geoinkar$order), ]

# create long dataset
library(tidyverse)
ds.final.long <- ds.final %>%
  gather(key, value, -id, -space_unit, -aggregate) %>%
  mutate(year = sub(".*_(\\d+)$", "\\1", key)) %>% 
  mutate(key = str_replace(key, "_\\d+$", "")) %>% 
  spread(key, value)

# count missing values
colnames(ds.final.long)[colSums(is.na(ds.final.long))>0]

# impute missing values
ds.final.long.imp <- ds.final.long %>%
  group_by(id) %>%
  fill(everything()) %>% 
  mutate(year = as.numeric(year))

colnames(ds.final.long.imp)[colSums(is.na(ds.final.long.imp))>0]

ds.final.long.imp <- ds.final.long.imp %>%
  mutate(ausgaben_für_sachinvestitionen = if_else(ausgaben_für_sachinvestitionen== 0, NA_real_, ausgaben_für_sachinvestitionen)) %>% 
  mutate(ausgaben_für_sachinvestitionen = if_else(is.na(ausgaben_für_sachinvestitionen), 
                                                  median(ds.final.long.imp$ausgaben_für_sachinvestitionen, na.rm = TRUE), 
                                                  ausgaben_für_sachinvestitionen))

# combination of variables and exclude others
ds.final.long.imp['einwohner_von_18_bis_unter_50_jahren'] <- (ds.final.long.imp$einwohner_von_18_bis_unter_25_jahren + 
                                                                ds.final.long.imp$einwohner_von_25_bis_unter_30_jahren + 
                                                                ds.final.long.imp$einwohner_von_30_bis_unter_50_jahren )
ds.final.long.imp$bruttoinlandsprodukt_in_millon_euro <- (ds.final.long.imp$bruttoinlandsprodukt_in_1000_euro/1000)
ds.final.long.imp$siedlungsdichte_in_km2_miles <- (ds.final.long.imp$siedlungsdichte_in_km2/1000)
ds.final.long.imp$ausgaben_für_sachinvestitionen_miles <- (ds.final.long.imp$ausgaben_für_sachinvestitionen/1000)
ds.final.long.imp$bruttoinlandsprodukt_in_millon_euro_ln <- log(ds.final.long.imp$bruttoinlandsprodukt_in_millon_euro)


omit.variables <- c('einwohner_von_18_bis_unter_25_jahren', "einwohner_von_25_bis_unter_30_jahren",
                    'einwohner_von_30_bis_unter_50_jahren', "einwohner_von_50_bis_unter_65_jahren",
                    "beschäftigtenquote_männer", "beschäftigtenquote_frauen", "bruttoinlandsprodukt_in_1000_euro",
                    "siedlungsdichte_in_km2", "ausgaben_für_sachinvestitionen")
ds.final.long.imp <- select(ds.final.long.imp, -one_of(omit.variables))


View(ds.final.long.imp)
dim(ds.final.long.imp)
colnames(ds.final.long.imp)
summary(ds.final.long.imp)
new.colnames.ds <- c("id","space_unit",
                     "aggregate","year", 
                     "employeesAO_w_vocat_qualif_perc","employeesAO_wo_vocat_qualif_perc",
                     "employees_IT_scient_serv_occup_perc", "employment_rate", 
                     "big_companies_perc","households_high_income_perc", 
                     "households_middle_income_perc","households_low_income_perc",
                     "small_companies_perc", "micro_companies_perc",
                     "medium_companies_perc", "residents_18_to_50yrs_perc",
                     "gross_domest_prod_millon", "settlement_density_km2_miles",
                     "Expenditure_physical_invest_per_inhab_miles", "gross_domest_prod_millon_ln" )
colnames(ds.final.long.imp) <- new.colnames.ds
ds.final.long.imp$Expenditure_physical_invest_per_inhab_miles_ln <- log(ds.final.long.imp$Expenditure_physical_invest_per_inhab_miles)
ds.final.long.imp$Expenditure_physical_invest_per_inhab_ln <- log(( (ds.final.long.imp$Expenditure_physical_invest_per_inhab_miles)*1000) )

library(sf)
library(corrplot)
shapefile <- st_read("other/vg2500_krs/vg2500_krs.shp")
shapefile$ARS <- as.numeric(shapefile$ARS)
ds.final.long.imp.w.geodata <- merge(shapefile, ds.final.long.imp, by.x="ARS", by.y="id", all.x=T)
colSums(is.na(ds.final.long.imp.w.geodata))
str(ds.final.long.imp.w.geodata)
dim(ds.final.long.imp.w.geodata)

class(ds.final.long.imp.w.geodata %>%
        filter(year == 2020) %>% as.tibble())
# use only data 2020 for the example
ds.2020 <- ds.final.long.imp.w.geodata %>%
  filter(year == 2020) %>% as.tibble()

colnames(ds.2020)

# quick plot
for (i_name in colnames(ds.2020)[13:30]) {
  print( ggplot(ds.2020 ) +
          geom_sf(aes(geometry = geometry, fill = !!as.name(i_name))) +
           scale_fill_viridis_c(option = "G", direction = -1)+
           labs(title = i_name, fill = "") 
         )
}


quantile_plot <- function(ds, col.name.var, num.cut, title.name) {
  quantile_var = quantile(ds[[col.name.var]], probs = seq(0, 1, by = 1/num.cut))
  name.var <- paste(col.name.var,'quantile', sep = '_')
  ds[name.var] <- cut(ds[[col.name.var]], breaks = quantile_var,
                                                     include.lowest = TRUE)

  print(ggplot(ds) +
    geom_sf(aes(geometry = geometry, fill = !!as.name(name.var) )) +
      scale_fill_brewer(direction = 1, palette = 1) +
      labs(title = title.name, fill = "Ranges")
    )
  
  # print(name.var)
}

# tech
quantile_plot(ds.2020, "employees_IT_scient_serv_occup_perc", 6, "Rate of IT employees")
# income
quantile_plot(ds.2020, "gross_domest_prod_millon_ln", 6, "log of GDP")
# capital
#quantile_plot(ds.2020, "Expenditure_physical_invest_per_inhab_miles", 4)
#quantile_plot(ds.2020, "Expenditure_physical_invest_per_inhab_miles_ln", 4)
quantile_plot(ds.2020, "Expenditure_physical_invest_per_inhab_ln", 5, "ln of Physical investment")
# labor
quantile_plot(ds.2020, "employment_rate", 5, "Employment rate")
# controls
#quantile_plot(ds.2020, "employeesAO_w_vocat_qualif_perc", 4)
#quantile_plot(ds.2020, "employeesAO_wo_vocat_qualif_perc", 4)
quantile_plot(ds.2020, "residents_18_to_50yrs_perc", 5, "% of residents from 18 to 50 years")
quantile_plot(ds.2020, "settlement_density_km2_miles", 5,  "Settlement density by km2 (1k)")
#quantile_plot(ds.2020, "big_companies_perc", 4)
#quantile_plot(ds.2020, "small_companies_perc", 4)
#quantile_plot(ds.2020, "medium_companies_perc", 4)
#quantile_plot(ds.2020, "micro_companies_perc", 4)
#quantile_plot(ds.2020, "households_high_income_perc", 4)
#quantile_plot(ds.2020, "households_middle_income_perc", 4)
#quantile_plot(ds.2020, "households_low_income_perc", 4)

colnames(ds.2020)
col.matrix.corr <- c("Rate of IT employees", "log of GDP", "ln of Physical investment", "% of residents from 18 to 50 years", "Settlement density by km2 (1k)")
# calculate correlations
corr_results  <- cor(ds.2020 %>% 
                       select_if(is.numeric) %>% 
                       as.data.frame() %>% 
                       select(-c(ARS, ADE, IBZ, year)) %>% 
                       select(employees_IT_scient_serv_occup_perc,
                              gross_domest_prod_millon_ln,
                              Expenditure_physical_invest_per_inhab_ln,
                              residents_18_to_50yrs_perc,
                              settlement_density_km2_miles) %>% 
                       rename(!!!setNames(names(.), col.matrix.corr)), method="spearman")


# plot the correlation matrix
corrplot(corr_results, type="lower", diag = FALSE,
         mar=c(0,0,0,0), tl.cex= 0.7, tl.col = "black", tl.offset = 0.5, tl.srt = 70,
         method = "color", addCoef.col = "black", number.cex=0.9,
         outline=FALSE, number.font = 2)

# histogram plot
par(mfrow = c(4, 4))
for (variable in colnames(ds.2020)[13:30]) {
  hist(ds.2020[[variable]], main = variable, xlab = "")
}

colnames(ds.2020)


# ols model
ols_results <- list()
f_ols <- gross_domest_prod_millon_ln ~ 
  employees_IT_scient_serv_occup_perc + Expenditure_physical_invest_per_inhab_ln + employment_rate +
  residents_18_to_50yrs_perc + settlement_density_km2_miles
col.name.model <- c("gross_domest_prod_millon_ln", "employees_IT_scient_serv_occup_perc", 
"Expenditure_physical_invest_per_inhab_ln", "employment_rate", 
"residents_18_to_50yrs_perc", "settlement_density_km2_miles")


for (yr in 2016:2020){
  filter.ds <- ds.final.long.imp.w.geodata %>% as.data.frame() %>% 
    filter(year == yr) %>% 
    select(all_of(col.name.model) )
  model.year <- lm(f_ols, data = filter.ds)
  ols_results[[as.character(yr)]] <- model.year
}

summary(ols_results[['2020']])
car::vif(ols_results[['2020']])
corrplot(cor(ds.2020 %>% 
               select_if(is.numeric) %>% 
               as.data.frame() %>% 
               select(-c(ARS, ADE, IBZ, year)) %>% 
               select(all_of(col.name.model)), method="spearman"), type="lower", diag = FALSE,
         mar=c(0,0,0,0), tl.cex= 0.5, tl.col = "black", tl.offset = 0.5, tl.srt = 10,
         method = "color", addCoef.col = "black", number.cex=0.9,
         outline=FALSE, number.font = 2)
colnames(ds.final.long.imp.w.geodata)

# compute spatial weight matrix
library(spdep)
require("rgdal")
krs_shp_mtrx <- readOGR("other/vg2500_krs", "vg2500_krs")
# according to queen=T
krs_mtrx <- poly2nb(krs_shp_mtrx, queen = TRUE)
# plot the neigbor-relations
plot(krs_shp_mtrx, border="grey60")
plot(krs_mtrx, coordinates(krs_shp_mtrx), add=TRUE, pch=19, cex=0.6)
# form weight matrix w=row-standardized
krs_w <- nb2listw(krs_mtrx, style="W")

# estimate Moran's I test - spatial autocrrelation in the data
# all the results are positive spatial autocorrelation, except big_companies
moran.test(ds.2020$gross_domest_prod_millon_ln, krs_w)
moran.test(ds.2020$employees_IT_scient_serv_occup_perc, krs_w)
moran.test(ds.2020$Expenditure_physical_invest_per_inhab_miles, krs_w)
moran.test(ds.2020$Expenditure_physical_invest_per_inhab_miles_ln, krs_w)
moran.test(ds.2020$Expenditure_physical_invest_per_inhab_ln, krs_w)
moran.test(ds.2020$employment_rate, krs_w)
moran.test(ds.2020$employeesAO_w_vocat_qualif_perc, krs_w)
moran.test(ds.2020$residents_18_to_50yrs_perc, krs_w)
moran.test(ds.2020$settlement_density_km2_miles, krs_w)
moran.test(ds.2020$big_companies_perc, krs_w) # no spatial correlation
moran.test(ds.2020$medium_companies_perc, krs_w)
moran.test(ds.2020$small_companies_perc, krs_w)
moran.test(ds.2020$households_high_income_perc, krs_w)
moran.test(ds.2020$households_middle_income_perc, krs_w)

spatial.autocor.val <- function(col.vec, year.val, dst, kr.mtx){
  dst <- dst %>%
    filter(year == year.val) %>% as.tibble()
  
  for (nam.var in col.vec){
    print("*************************************")
    cat(nam.var, year.val, "\n" , sep = '___')
    print(moran.test(dst[[nam.var]], kr.mtx))
    print("*************************************")
  }
}

spatial.autocor.val( col.name.model[1], 2020, ds.final.long.imp.w.geodata, krs_w ) 



# Lagrange multiplier diagnostics for spatial dependence - Anselin
# spatial dependence from the errors on the dependent variable
LM.test.results <- function(ols.rs.ls, kr.w){
  for (iy in as.character(2016:2020)){
    cat(iy, "\n" , sep = '___****************')
    rest.error.dep.lag.tst <- lm.LMtests(ols.rs.ls[[iy]], listw=kr.w, test=c("RLMerr", "RLMlag") )
    print(rest.error.dep.lag.tst)
    print('*****************************************************')
  }
}

LM.test.results(ols_results, krs_w)

# repeat making logitudinal dataframe if including spatially lagged variables!
# create spatially lagged variables
ols.results.wlags <- list()
lgs.ds <- list()
for (n.yr in 2016:2020){
  filter.ds <- ds.final.long.imp.w.geodata %>% as.data.frame() %>% 
    filter(year == n.yr) %>% 
    select(all_of(c("ARS", "year",col.name.model) ) )
  filter.ds[paste('gross_domest_ln_lag_', n.yr, sep = '')] <- lag.listw(krs_w, filter.ds$gross_domest_prod_millon_ln)
  filter.ds[paste('employ_IT_lag_', n.yr, sep = '')] <- lag.listw(krs_w, filter.ds$employees_IT_scient_serv_occup_perc)
  filter.ds[paste('exp_phy_inv_lag_', n.yr, sep = '')] <- lag.listw(krs_w, filter.ds$Expenditure_physical_invest_per_inhab_ln)
  filter.ds[paste('employ_rate_lag_', n.yr, sep = '')] <- lag.listw(krs_w, filter.ds$employment_rate)
  # form ds of lags
  ds.lags <- filter.ds %>% 
    select(ARS, year, paste('gross_domest_ln_lag_', n.yr, sep = ''), paste('employ_IT_lag_', n.yr, sep = ''),
           paste('exp_phy_inv_lag_', n.yr, sep = ''), paste('employ_rate_lag_', n.yr, sep = ''))
  colnames(ds.lags) <- gsub("_\\d+", "", colnames(ds.lags))
  lgs.ds[[as.character(n.yr)]] <- ds.lags
  
  # create formulas
  orig.f_ols <- paste(as.character( deparse(f_ols)),collapse = " ")
  new.features <- paste(paste('gross_domest_ln_lag_', n.yr, sep = ''),paste('employ_IT_lag_', n.yr, sep = ''),
                        paste('exp_phy_inv_lag_', n.yr, sep = ''), paste('employ_rate_lag_', n.yr, sep = ''), sep = ' + ' )
  final.form <- paste(orig.f_ols, new.features, sep = " + ")
  ols.res.wlags <- lm(formula = final.form, data = filter.ds)
  ols.results.wlags[[as.character(n.yr)]] <- ols.res.wlags
}

summary(ols.results.wlags[["2020"]])
spatial.lags <- Reduce(rbind, lgs.ds)
ds.final.long.imp.w.geodata.incldLags <- merge(spatial.lags, ds.final.long.imp.w.geodata %>% as.tibble(), by = c('ARS', 'year')) %>% as.tibble()


# estimate model
colnames(ds.final.long.imp.w.geodata.incldLags)

f_spt_w_lags_controls <- gross_domest_prod_millon_ln ~ 
  employees_IT_scient_serv_occup_perc + Expenditure_physical_invest_per_inhab_ln + employment_rate +
  employ_IT_lag + exp_phy_inv_lag + employ_rate_lag +
  residents_18_to_50yrs_perc + settlement_density_km2_miles

f_spt_wo_lags <- gross_domest_prod_millon_ln ~ 
  employees_IT_scient_serv_occup_perc + Expenditure_physical_invest_per_inhab_ln + employment_rate +
  residents_18_to_50yrs_perc + settlement_density_km2_miles 

f_spt_w_lags_wo_controls <- gross_domest_prod_millon_ln ~ 
  employees_IT_scient_serv_occup_perc + Expenditure_physical_invest_per_inhab_ln + employment_rate +
  employ_IT_lag + exp_phy_inv_lag + employ_rate_lag

fspatlag.wlag.dep.controls <- spml(f_spt_w_lags_controls, data = ds.final.long.imp.w.geodata.incldLags,
                                   index = c("ARS","year"),
                                   listw = krs_w , model="within", spatial.error="none",
                                   effect = "twoways", method = "eigen",
                                   lag=TRUE)
summary(fspatlag.wlag.dep.controls)

fspatlag.wolag <- spml(f_spt_wo_lags, data = ds.final.long.imp.w.geodata.incldLags,
                          index = c("ARS","year"),
                         listw = krs_w , model="within", spatial.error="none",
                         effect = "twoways", method = "eigen",
                         lag=TRUE) 

summary(fspatlag.wolag)

fspatlag.wocontrol <- spml(f_spt_w_lags_wo_controls, data = ds.final.long.imp.w.geodata.incldLags,
                       index = c("ARS","year"),
                       listw = krs_w , model="within", spatial.error="none",
                       effect = "twoways", method = "eigen",
                       lag=TRUE) 

summary(fspatlag.wocontrol)

write.csv(ds.final.long.imp.w.geodata.incldLags, "code/data_process_final/dsfinallongimpwgeodataincldLags.csv")
library(writexl)
write_xlsx(ds.final.long.imp.w.geodata.incldLags, "code/data_process_final/dsfinallongimpwgeodataincldLags.xlsx")

## calculate impact measures
f.spat.impacts <- spatialreg::impacts(fspatlag.wlag.dep.controls, listw = krs_w, time = 11)
summary(f.spat.impacts, zstats=TRUE, short=TRUE)
                  



    