
import delimited using nhgis0046_ds176_20105_2010_county.csv, clear
keep statea countya jmae001
gen year = 2010
ren jmae001 population
save pop2010_county, replace

import delimited using nhgis0046_ds177_20105_2010_county.csv, clear
keep statea countya jwoe*
local n = 1
foreach var of varlist jwoe* {
	ren `var' jwoe`n'
	local n = `n' + 1
}
reshape long jwoe, i(statea countya) j(numimm)

gen origin = ""
replace origin = "Total" if numimm==1
replace origin = "Europe" if numimm==2
replace origin = "Asia" if numimm==47
replace origin = "Africa" if numimm==91
replace origin = "Oceania" if numimm==116
replace origin = "Latin America" if numimm==123
replace origin = "Northern America" if numimm==159

drop if missing(origin)

merge m:1 statea countya using pop2010_county, nogen
gen imm_dens = jwoe/population*100midpoint
gen fips = real(string(statea, "%02.0f") + string(countya, "%03.0f"))
drop numimm statea countya
save imm_dens, replace
export delimited using imm_dens.csv, replace
