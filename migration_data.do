
gzuse usa_00002, clear
labvalch3 *, strfcn(proper(`"@"'))
label define STATEFIP 99 "National", modify

// education by state and migration status and year
preserve
keep if year >= 1990
keep if age >= 25 & age <= 65

drop if inlist(educd, 0, 1, 999) // missings
gen lths = educd >= 2 & educd <= 61
gen hs = educd >= 62 & educd <= 64
gen some_college = educd >= 65 & educd <= 100
gen bachelor = educd == 101
gen graduate = educd >= 110 & educd <= 116
gen college = bachelor == 1 | graduate == 1

gen immigrant = bpl >= 150 & bpl <= 900
drop if bpl > 900 // missings

expand 2, gen(new) // we want a national group as well
replace statefip = 99 if new == 1

gcollapse lths hs some_college bachelor graduate college [pw=perwt], by(statefip immigrant year)

compress
save educ_state, replace
export delimited using educ_state.csv, replace
restore
// education by state, migration status, year, and new migrants
preserve
keep if year >= 1990
keep if age >= 25 & age <= 65

drop if inlist(educd, 0, 1, 999)
gen lths = educd >= 2 & educd <= 61
gen hs = educd >= 62 & educd <= 64
gen some_college = educd >= 65 & educd <= 100
gen bachelor = educd == 101
gen graduate = educd >= 110 & educd <= 116
gen college = bachelor == 1 | graduate == 1

gen immigrant = bpl >= 150 & bpl <= 900
drop if bpl > 900 // missings

gen yrs_mig = year - yrimmig
gen new_migrant = yrs_mig <= 5

expand 2, gen(new) // we want a national group as well
replace statefip = 99 if new == 1

gcollapse lths hs some_college bachelor graduate college [pw=perwt], by(statefip immigrant year new_migrant)
drop if immigrant == 0 & new_migrant == 1
replace new_migrant = 1 if immigrant == 0

compress
save educ_state_new, replace
export delimited using educ_state_new.csv, replace
restore
// education by state, migration status, year, and gender
preserve
keep if year >= 1990
keep if age >= 25 & age <= 65

drop if inlist(educd, 0, 1, 999) // missings
gen lths = educd >= 2 & educd <= 61
gen hs = educd >= 62 & educd <= 64
gen some_college = educd >= 65 & educd <= 100
gen bachelor = educd == 101
gen graduate = educd >= 110 & educd <= 116
gen college = bachelor == 1 | graduate == 1

gen immigrant = bpl >= 150 & bpl <= 900
drop if bpl > 900 // missings

expand 2, gen(new) // we want a national group as well
replace statefip = 99 if new == 1

gcollapse lths hs some_college bachelor graduate college [pw=perwt], by(statefip immigrant year sex)

compress
save educ_state_sex, replace
export delimited using educ_state_sex.csv, replace
restore
// education by state, migration status, year, sex, and new migrants
preserve
keep if year >= 1990
keep if age >= 25 & age <= 65

drop if inlist(educd, 0, 1, 999)
gen lths = educd >= 2 & educd <= 61
gen hs = educd >= 62 & educd <= 64
gen some_college = educd >= 65 & educd <= 100
gen bachelor = educd == 101
gen graduate = educd >= 110 & educd <= 116
gen college = bachelor == 1 | graduate == 1

gen immigrant = bpl >= 150 & bpl <= 900
drop if bpl > 900 // missings

gen yrs_mig = year - yrimmig
gen new_migrant = yrs_mig <= 5

expand 2, gen(new) // we want a national group as well
replace statefip = 99 if new == 1

gcollapse lths hs some_college bachelor graduate college [pw=perwt], by(statefip immigrant year new_migrant sex)
drop if immigrant == 0 & new_migrant == 1
replace new_migrant = 1 if immigrant == 0

compress
save educ_state_sex_new, replace
export delimited using educ_state_sex_new.csv, replace
restore
// education by state, migration region, and year
preserve
keep if year >= 1990
keep if age >= 25 & age <= 65

drop if inlist(educd, 0, 1, 999)
gen lths = educd >= 2 & educd <= 61
gen hs = educd >= 62 & educd <= 64
gen some_college = educd >= 65 & educd <= 100
gen bachelor = educd == 101
gen graduate = educd >= 110 & educd <= 116
gen college = bachelor == 1 | graduate == 1

gen immigrant = bpl >= 150 & bpl <= 900
drop if bpl > 900 // missings

gen region = "Non-Migrants"
replace region = "North America" if bpl >= 150 & bpl <= 199
replace region = "Latin America" if bpl >= 200 & bpl <= 399
replace region = "Europe" if bpl >= 400 & bpl <= 499
replace region = "Asia" if bpl >= 500 & bpl <= 599
replace region = "Africa" if bpl >= 600 & bpl <= 699
replace region = "Oceania" if bpl >= 700 & bpl <= 799

expand 2, gen(new) // we want a national group as well
replace statefip = 99 if new == 1

gcollapse lths hs some_college bachelor graduate college [pw=perwt], by(statefip region immigrant year)

compress
save educ_state_region, replace
export delimited using educ_state_region.csv, replace
restore
// education by state, migration region, new migrant, and year
preserve
keep if year >= 1990
keep if age >= 25 & age <= 65

drop if inlist(educd, 0, 1, 999)
gen lths = educd >= 2 & educd <= 61
gen hs = educd >= 62 & educd <= 64
gen some_college = educd >= 65 & educd <= 100
gen bachelor = educd == 101
gen graduate = educd >= 110 & educd <= 116
gen college = bachelor == 1 | graduate == 1

gen immigrant = bpl >= 150 & bpl <= 900
drop if bpl > 900 // missings

gen region = "Non-Migrants"
replace region = "North America" if bpl >= 150 & bpl <= 199
replace region = "Latin America" if bpl >= 200 & bpl <= 399
replace region = "Europe" if bpl >= 400 & bpl <= 499
replace region = "Asia" if bpl >= 500 & bpl <= 599
replace region = "Africa" if bpl >= 600 & bpl <= 699
replace region = "Oceania" if bpl >= 700 & bpl <= 799

gen yrs_mig = year - yrimmig
gen new_migrant = yrs_mig <= 5

expand 2, gen(new) // we want a national group as well
replace statefip = 99 if new == 1

gcollapse lths hs some_college bachelor graduate college [pw=perwt], by(statefip region year immigrant new_migrant)
drop if region == "Non-Migrants" & new_migrant == 1
replace new_migrant = 1 if immigrant == 0

compress
save educ_state_region_new, replace
export delimited using educ_state_region_new.csv, replace
restore
// education by state, migration region, sex, and year
preserve
keep if year >= 1990
keep if age >= 25 & age <= 65

drop if inlist(educd, 0, 1, 999)
gen lths = educd >= 2 & educd <= 61
gen hs = educd >= 62 & educd <= 64
gen some_college = educd >= 65 & educd <= 100
gen bachelor = educd == 101
gen graduate = educd >= 110 & educd <= 116
gen college = bachelor == 1 | graduate == 1

gen immigrant = bpl >= 150 & bpl <= 900
drop if bpl > 900 // missings

gen region = "Non-Migrants"
replace region = "North America" if bpl >= 150 & bpl <= 199
replace region = "Latin America" if bpl >= 200 & bpl <= 399
replace region = "Europe" if bpl >= 400 & bpl <= 499
replace region = "Asia" if bpl >= 500 & bpl <= 599
replace region = "Africa" if bpl >= 600 & bpl <= 699
replace region = "Oceania" if bpl >= 700 & bpl <= 799

gen yrs_mig = year - yrimmig
gen new_migrant = yrs_mig <= 5

expand 2, gen(new) // we want a national group as well
replace statefip = 99 if new == 1

gcollapse lths hs some_college bachelor graduate college [pw=perwt], by(statefip region immigrant year sex)

compress
save educ_state_region_sex, replace
export delimited using educ_state_region_sex.csv, replace
restore
// education by state, migration region, new migrant, sex, and year
preserve
keep if year >= 1990
keep if age >= 25 & age <= 65

drop if inlist(educd, 0, 1, 999)
gen lths = educd >= 2 & educd <= 61
gen hs = educd >= 62 & educd <= 64
gen some_college = educd >= 65 & educd <= 100
gen bachelor = educd == 101
gen graduate = educd >= 110 & educd <= 116
gen college = bachelor == 1 | graduate == 1

gen immigrant = bpl >= 150 & bpl <= 900
drop if bpl > 900 // missings

gen region = "Non-Migrants"
replace region = "North America" if bpl >= 150 & bpl <= 199
replace region = "Latin America" if bpl >= 200 & bpl <= 399
replace region = "Europe" if bpl >= 400 & bpl <= 499
replace region = "Asia" if bpl >= 500 & bpl <= 599
replace region = "Africa" if bpl >= 600 & bpl <= 699
replace region = "Oceania" if bpl >= 700 & bpl <= 799

gen yrs_mig = year - yrimmig
gen new_migrant = yrs_mig <= 5

expand 2, gen(new) // we want a national group as well
replace statefip = 99 if new == 1

gcollapse lths hs some_college bachelor graduate college [pw=perwt], by(statefip region year new_migrant immigrant sex)
drop if region == "Non-Migrants" & new_migrant == 1
replace new_migrant = 1 if immigrant == 0

compress
save educ_state_region_sex_new, replace
export delimited using educ_state_region_sex_new.csv, replace
restore
