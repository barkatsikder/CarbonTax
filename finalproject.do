/* ECON 35 Final Project for Professor Syon Bhanot.
 Authors: Marion, Zach, Barkat. 
 Due: 20 Dec 2020
*/


clear 
//log close
// Set up directories 

*Barkat Sikder
if "`c(username)'" == "barkatsikder" {
    global root = "/Users/barkatsikder/Dropbox/econ35project"
	}


//set up data directory
global data = "$root/submission"
//set up log 
log using "$root/finalproject", replace



// START



// SECTION III: Descriptive Statistics
use "$data/finaldata-FE.dta", clear

preserve
drop if year <1990
drop if year >2016
eststo clear
estpost summarize co2_tonpc Price_Rate gdppcPPPconst foFuel elProdRenew alnucEnergy energyUse agValue trade popDense gdpGrowth, quietly
esttab using descriptive.rtf, replace cells("count(fmt(a2)) mean sd min max") label title("Table 1: Descriptive Statistics") nomtitle nonumber noobs 
restore 




// SECTION IV: State-time fixed effects
use "$data/finaldata-FE.dta", clear



// M1: MODEL for LEVELS - NO LAG
preserve
// generate logs etc for models
gen logCO2pc = log(co2_tonpc)
gen logGDPpcPPPconst = log(gdppcPPPconst)
drop if year <1990
drop if year >2016
//1 OLS
*reg logCO2pc Price_Rate, r 
*est store R1
//FE
xtset id year
//2 only state
xtreg logCO2pc Price_Rate, fe r
est store R2
//3 state-time 
xtreg logCO2pc Price_Rate i.year, fe r
est store R3
//4 Initial controls
xtreg logCO2pc Price_Rate i.year logGDPpcPPPconst foFuel elProdRenew agValue, fe r
est store R4
//5 Robustness 
xtreg logCO2pc Price_Rate i.year logGDPpcPPPconst foFuel elProdRenew alnucEnergy energyUse agValue trade enImports popDense gdpGrowth, fe r
est store R5
//tabulate 
esttab R2 R3 R4 R5, se r2 ar2
display _N
esttab R3 R4 R5 using tableFElevel.rtf, se r2 replace keep(Price_Rate logGDPpcPPPconst foFuel elProdRenew alnucEnergy energyUse agValue trade popDense gdpGrowth) title("FE levels") addnotes("Note 1" "Note 2") mtitle("logCO2pc") //label
restore





// M2: MODEL for LEVELS - LAGGED
preserve

// generate logs and lags etc for models
gen logCO2pc = log(co2_tonpc)
gen logGDPpcPPPconst = log(gdppcPPPconst)
sort id year 
by id: gen lag1price = Price_Rate[_n-1]
by id: gen lag2price = Price_Rate[_n-2]
by id: gen lag3price = Price_Rate[_n-3]

drop if year <1993
drop if year >2016

//1 lag 
xtreg logCO2pc lag1price i.year logGDPpcPPPconst foFuel elProdRenew alnucEnergy agValue gdpGrowth, fe r
est store L1
//2 lag 
xtreg logCO2pc lag2price i.year logGDPpcPPPconst foFuel elProdRenew alnucEnergy agValue gdpGrowth, fe r
est store L2 
//3 lag 
xtreg logCO2pc lag3price i.year logGDPpcPPPconst foFuel elProdRenew alnucEnergy agValue gdpGrowth, fe r
est store L3

esttab L1 L2 L3, se r2 ar2
display _N
esttab L1 L2 L3 using tableFElevellag.rtf, se r2 replace keep(lag1price lag2price lag3price logGDPpcPPPconst foFuel elProdRenew alnucEnergy agValue gdpGrowth) title("FE lag levels") addnotes("Note 1" "Note 2") mtitle("logCO2pc") //label
restore





// M3: MODEL for GROWTH - LAGGED
preserve

// generate logs and lags for models
gen logCO2pc = log(co2_tonpc)
gen logGDPpcPPPconst = log(gdppcPPPconst)

sort id year 
by id: gen lag1emission = co2_tonpc[_n-1]
by id: gen lag2emission = co2_tonpc[_n-2]
by id: gen lag3emission = co2_tonpc[_n-3]
by id: gen lag4emission = co2_tonpc[_n-4]
by id: gen lag5emission = co2_tonpc[_n-5]

gen loglag1emission = log(lag1emission)
gen loglag2emission = log(lag2emission)
gen loglag3emission = log(lag3emission)
gen loglag4emission = log(lag4emission)
gen loglag5emission = log(lag5emission)

gen lag1price = Price_Rate[_n-1]
gen lag2price = Price_Rate[_n-2]
gen lag3price = Price_Rate[_n-3]
gen lag4price = Price_Rate[_n-4]
gen lag5price = Price_Rate[_n-5]

gen em1 = (logCO2pc - loglag1emission)
gen em2 = (logCO2pc - loglag2emission)/2
gen em3 = (logCO2pc - loglag3emission)/3
gen em4 = (logCO2pc - loglag4emission)/4
gen em5 = (logCO2pc - loglag5emission)/5


drop if year <1995
drop if year >2016

//1 lag 
xtreg em1 lag1price loglag1emission i.year logGDPpcPPPconst foFuel elProdRenew alnucEnergy agValue trade popDense gdpGrowth, fe r
est store L4
//2 lag 
xtreg em2 lag2price loglag2emission i.year logGDPpcPPPconst foFuel elProdRenew alnucEnergy agValue trade popDense gdpGrowth, fe r
est store L5
//3 lag 
xtreg em3 lag3price loglag3emission i.year logGDPpcPPPconst foFuel elProdRenew alnucEnergy agValue trade popDense gdpGrowth, fe r
est store L6
//4 lag 
xtreg em4 lag4price loglag4emission i.year logGDPpcPPPconst foFuel elProdRenew alnucEnergy agValue trade popDense gdpGrowth, fe r
est store L7
//5 lag 
xtreg em5 lag5price loglag5emission i.year logGDPpcPPPconst foFuel elProdRenew alnucEnergy agValue trade popDense gdpGrowth, fe r
est store L8
esttab L4 L5 L6 L7 L8, se r2 ar2
display _N
esttab L4 L6 L8 using tableFEgrowth.rtf, se r2 replace keep(lag1price loglag1emission lag3price loglag3emission lag5price loglag5emission logGDPpcPPPconst foFuel elProdRenew alnucEnergy agValue trade popDense gdpGrowth) title("FE growth") addnotes("Note 1" "Note 2") mtitle("logCO2pc") //label
restore








// SECTION V: synthetic DinD
use "$data/finaldata-synth.dta", clear

*synthetic controls info
/*
      synth depvar predictorvars , trunit(#) trperiod(#) [ counit(numlist)
        xperiod(numlist) mspeperiod() resultsperiod() nested allopt
        unitnames(varname) figure keep(file) customV(numlist) optsettings ]
*/

*Set up panel data for synthetic controls 
*encode wbcode, g(id) label(wbcode)
*gen id2= id, after(id)
tsset id year

*Carbon tax implemented in 2010 in Ireland 
*Including all countries for synthetic control 
#delimit;
synth co2_tonpc trade energyUse co2_tonpc(1997) co2_tonpc(1998)  co2_tonpc(1999) co2_tonpc(2000)  
 co2_tonpc(2001)  co2_tonpc(2002)  co2_tonpc(2003) co2_tonpc(2004) co2_tonpc(2005) co2_tonpc(2006) co2_tonpc(2007) co2_tonpc(2008)  co2_tonpc(2009)
   gdpPPPconst alnucEnergy elProdRenew agLand popDense laArea , 
 trunit(68) trperiod(2010) fig mspeperiod(1997(1)2014) resultsperiod (1997(1)2014)
 ;
#delimit cr

*Including just countries that have not implemented a carbon tax
#delimit;
synth co2_tonpc  co2_tonpc(1997) co2_tonpc(1998)  co2_tonpc(1999) co2_tonpc(2000)  
 co2_tonpc(2001)  co2_tonpc(2002)  co2_tonpc(2003) co2_tonpc(2004) co2_tonpc(2005) co2_tonpc(2006) co2_tonpc(2007) co2_tonpc(2008)  co2_tonpc(2009)
  trade energyUse  gdpPPPconst alnucEnergy elProdRenew agLand popDense laArea , 
 trunit(68) trperiod(2010) counit(9 11 13 27 28 51 57 58 73 94 95 115 135) fig mspeperiod(1997(1)2014) resultsperiod (1997(1)2014) 

 ;
#delimit cr    
	* title("Synthetic Ireland vs Ireland (countries without tax)")
* xtitle("Carbon Dioxide emissions (metric tons per capita)")
 





//THE END
clear
log close


