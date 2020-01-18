///////////////////////// 1. Union premium

ssc install bcuse
bcuse wagepan, clear

// Estimation without controls
did_multiplegt lwage nr year union, placebo(2) breps(50) cluster(nr)
ereturn list 

// Estimation with controls
did_multiplegt lwage nr year union, controls(black) placebo(2) breps(50) cluster(nr)
ereturn list 

// Estimation with worker-specific linear trends
did_multiplegt lwage nr year union, trends_lin(nr) placebo(2) breps(50) cluster(nr)
ereturn list

// Estimation with dynamic effects
did_multiplegt lwage nr year union, dynamic(2) placebo(2) breps(50) cluster(nr)
ereturn list  

///////////////////////// 2. Effect of newspaper on electoral turnout 

use "C:\Users\Clement\Dropbox\Double fixed effects\Applications\Newspapers entry and exit\voting_cnty_clean.dta", clear

set matsize 800

////////////// a. Data preparation

// Gentzkow et al's regression = first difference regression => we need to go from their first difference sample to a sample in levels. I.e.: if a county*year is not in their first difference sample but the same county in the next year is in their
// sample, that county*year is in our sample.

gen sample=0
gen tminus1sample=0
forvalue i=1872(4)1928 {
replace sample=1 if (year==`i')&mainsample==1
sort cnty90 year
replace tminus1sample=1 if sample==0&sample[_n+1]==1&cnty90==cnty90[_n+1]&year==`i'-4
replace sample=1 if sample[_n+1]==1&cnty90==cnty90[_n+1]&year==`i'-4
}
tab sample mainsample 
keep if sample==1
drop sample

///////////////// b. DIDM computation, and placebo, without and with state specific trends

// Treatment, number of newspapers in each county*year, takes many values. Values above 3 are pretty rare => DIDM estimator with original treatment would be too noisy => we lump treatment values above 3 together. 
// Instead, we could lump values above 4 together, would not change results much. 

tab numdailies
gen D_cat=(numdailies==1)+2*(numdailies==2)+3*(numdailies>=3)

// without state-specific trends 
did_multiplegt prestout cnty90 year numdailies, recat(D_cat) placebo(1) breps(50)
ereturn list

// with state-specific trends 
did_multiplegt prestout cnty90 year numdailies, recat(D_cat) trends_nonparam(st) placebo(1) breps(50)
ereturn list

////////////////////////// 3. Various tests on madeup data

///////////////// a. Test of weight option

clear all
set obs 9
gen G=(_n>=4)+(_n>=7)
gen T=(_n-3*floor((_n-1)/3))-1
gen D=(G==0&T>=1)+(G==1&T>=2)
gen Y=T+G+D*(G==0)*1+D*(G==1)*3
gen weight2=1+(G==1)
di (1+3*2)/3
did_multiplegt Y G T D
ereturn list
did_multiplegt Y G T D, weight(weight2)
ereturn list

///////////////// b. Test of what command does when more than 1 observation per group*time period

clear all
set obs 12
gen G=(_n>=4)+(_n>=10)
gen T=(_n-3*floor((_n-1)/3))-1
gen D=(G==0&T>=1)+(G==1&T>=2)
gen Y=T+G+D*(G==0)*1+D*(G==1)*3
did_multiplegt Y G T D
ereturn list

///////////////// c. Test of controls option

clear all
set obs 30
gen G=floor((_n-1)/3)
gen T=(_n-3*floor((_n-1)/3))-1
gen X=(uniform()>=0.5)
gen D=(G<=2&T==1)+(G<=5&T==2)
gen Y=T+G+D*(G<=2)+D*(G>2&G<=5)*3+X
did_multiplegt Y G T D, controls(X)
ereturn list

///////////////// d. Test of trends_lin option

clear all
set obs 40
gen G=floor((_n-1)/4)
gen T=(_n-4*floor((_n-1)/4))-1
gen D=(G<=2&T==2)+(G<=5&T==3)
gen Y=T+G*T+D*(G<=2)+D*(G>2&G<=5)*3
did_multiplegt Y G T D, trends_lin(G)
ereturn list















