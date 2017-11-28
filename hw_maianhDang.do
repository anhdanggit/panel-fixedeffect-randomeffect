
* Specify Working Directory
cd "C:\Users\utilisateur\iCloudDrive\TSE-EEE-S1\Panel\HW1"

* Import data
use wagedata.dta

* Open log file:
log using "MaiAnhDang_panel_hw.log", replace text //to store the result




* Change the format to panel data
reshape long lwage educ black hisp expersq exper married union, i(nr) j(year)

* Create var of number of obs. periods for each individual
sort nr
by nr: gen numobs = _n

* Check the balance 
tab year
tab numobs

* Data Manipulation (enable xtoverid)

// we have to create the dummies of years by hands, because unluckily xtoverid 
// has not supported auto factors 
// also we have to drop multicolinearity var manually, to make xtoverid works

gen year1980 = 0
gen year1981 = 0
gen year1982 = 0
gen year1983 = 0
gen year1984 = 0
gen year1985 = 0
gen year1986 = 0
gen year1987 = 0

replace year1980 = 1 if year == 1980
replace year1981 = 1 if year == 1981
replace year1982 = 1 if year == 1982
replace year1983 = 1 if year == 1983
replace year1984 = 1 if year == 1984
replace year1985 = 1 if year == 1985
replace year1986 = 1 if year == 1986
replace year1987 = 1 if year == 1987

* Basic summary statistics for the first year of observation:
describe
summarize lwage educ black hisp exper expersq married union if year==1980

* Declare individual and time dimensions:
xtset nr year

* Summary statistics of the main variables:
hist lwage
hist lwage if lwage > -1, fraction
xtsum lwage educ exper expersq // overal, between, within variation





**** Pooled OLS
global xlist black hisp married union
global yearlist year1981 year1982 year1983 year1984 year1985 year1986 year1987 //drop year1980

reg lwage educ exper expersq $xlist $yearlist  // usual
reg lwage educ exper expersq $xlist $yearlist, cluster(nr)

**** Fixed Effect
xtreg lwage educ expersq $xlist $yearlist exper , fe cluster(nr)

**** Random Effect
xtreg lwage educ exper expersq $xlist $yearlist, re cluster(nr)

//xttest0 ** for the RE vs POLS

**** Hausman Test: RE vs. FE
xtoverid // p-val < 0.05 reject RE

**** Hausman-Taylor model
xtreg lwage educ exper expersq $xlist, fe cluster(nr)

xthtaylor lwage educ exper expersq $xlist, endog(educ) 

**** Test: HT vs. FE
xtoverid

** Hausman-Taylor estimator has the stronger assumption on exgeneous var
** and the independence of some regressor on fixed-effect
** Rejection implies that these strong assumption is not valid

log close
