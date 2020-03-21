/*Do File for Thesis*/

use "C:\Users\Christopher\Desktop\NHANES DATA\1NHANES", clear

 use "C:\Users\csp324\Desktop\1NHANES.dta"
*Survey Weights* USE MEC for A1c, 2 cycles 2011-2014
gen wtmec6yr = wtmec2yr/3
svyset [pw = wtmec6yr], psu(sdmvpsu) strata(sdmvstra) singleunit(centered)
/*gen includions not missing, following exlcusion */
// Gen sample population, Asians over 20
gen asian = .
replace asian = 1 if ridreth3 == 6
replace asian = 0 if ridreth3 == 1
replace asian = 0 if ridreth3 == 2
replace asian = 0 if ridreth3 == 3
replace asian = 0 if ridreth3 == 4
replace asian = 0 if ridreth3 == 7
replace asian = 0 if ridreth3 == .
//diabetes type1 control
replace asian = . if ridageyr<=25


// A1c
gen a1c = . 
label define a1c_label 0 "Normal A1c" 1 "Prediabetic A1c" 2 "Diabetic A1c"
label values a1c a1c_label
replace a1c = 0 if (lbxgh <5.7) & (lbxgh >3)
replace a1c = 1 if (lbxgh >= 5.7) & (lbxgh <= 6.4)
replace a1c = 2 if (lbxgh >6.4) & (lbxgh <18)

*outcome 
*diabetes
gen diabetes=. 
replace diabetes=0 if a1c==0
replace diabetes=1 if a1c==2

*pre diabetes
gen prediabetes=. 
replace prediabetes=0 if a1c==0
replace prediabetes=1 if a1c==1

//Sleep Hours
tab sld012
gen slpd = .
replace slpd = 1 if(sld012>1)&(sld012<5.5)
replace slpd = 2 if(sld012>=5.5)&(sld012<7)
replace slpd = 3 if(sld012>=7)&(sld012<=8)
replace slpd = 4 if(sld012>8)&(sld012<15)

gen slpd1 = .
recode slpd1 (99=.)(77=.)
replace slpd1 = 1 if (sld010h>1)&(sld010h<5.5)
replace slpd1 = 2 if (sld010h>=5.5)&(sld010h<7)
replace slpd1 = 3 if (sld010h>=7)&(sld010h<=8)
replace slpd1 = 4 if (sld010h>8)&(sld010h<13)

gen slp =. 
replace slp = 1 if slpd==1 | slpd1==1 
replace slp = 2 if slpd==2 | slpd1==2
replace slp = 3 if slpd==3 | slpd1==3
replace slp = 4 if slpd==4 | slpd1==4
label define slp 1 "Very Short Sleep" 2 "Short Sleep" 3 "Normative Sleep" 4 "Long Sleep" 
label values slp  slp

tab slp
//sleep quality 
recode slq050 (9 7 = .)
*exp

gen slp_zz = .
replace slp_zz=1 if slq050==2 & slp==1 // good, very short
replace slp_zz=2 if slq050==2 & slp==2 // good, short
replace slp_zz=3 if slq050==2 & slp==3 // good, normal
replace slp_zz=4 if slq050==2 & slp==4 // good, long
replace slp_zz=5 if slq050==1 & slp==1 // bad, short
replace slp_zz=6 if slq050==1 & slp==2 // bad, very shor
replace slp_zz=7 if slq050==1 & slp==3 // bad, normal
replace slp_zz=8 if slq050==1 & slp==4 // bad, long

tab slp_zz


*/
*sample 
gen asian_a = 0
replace asian_a= 1 if asian==1 & a1c!=. & slp !=. 
*PHQ
	recode dpq010 (7=.)(9=.)
	recode dpq020 (7=.)
	recode dpq030 (7=.)(9=.)
	recode dpq040 (7=.)
	recode dpq050 (9=.)
	recode dpq060 (7=.)(9=.)
	recode dpq070 (9=.)
	recode dpq080 (9=.)
	recode dpq090 (7=.)(9=.)

	recode dpq020 (0=.)(1=.)(2=.)(3=.) if dpq010==.
	recode dpq030 (0=.)(1=.)(2=.)(3=.) if dpq020==.
	recode dpq040 (0=.)(1=.)(2=.)(3=.) if dpq030==.
	recode dpq050 (0=.)(1=.)(2=.)(3=.) if dpq040==.
	recode dpq060 (0=.)(1=.)(2=.)(3=.) if dpq050==.
	recode dpq070 (0=.)(1=.)(2=.)(3=.) if dpq060==.
	recode dpq080 (0=.)(1=.)(2=.)(3=.) if dpq070==.
	recode dpq090 (0=.)(1=.)(2=.)(3=.) if dpq080==.

	recode dpq010 (0=.)(1=.)(2=.)(3=.) if dpq090==.
	recode dpq020 (0=.)(1=.)(2=.)(3=.) if dpq090==.
	recode dpq030 (0=.)(1=.)(2=.)(3=.) if dpq090==.
	recode dpq040 (0=.)(1=.)(2=.)(3=.) if dpq090==.
	recode dpq050 (0=.)(1=.)(2=.)(3=.) if dpq090==.
	recode dpq060 (0=.)(1=.)(2=.)(3=.) if dpq090==.
	recode dpq070 (0=.)(1=.)(2=.)(3=.) if dpq090==.
	recode dpq080 (0=.)(1=.)(2=.)(3=.) if dpq090==.

	
	recode dpq010 (.=99)
	recode dpq020 (.=99)
	recode dpq030 (.=99)
	recode dpq040 (.=99)
	recode dpq050 (.=99)
	recode dpq060 (.=99)
	recode dpq070 (.=99)
	recode dpq080 (.=99)
	recode dpq090 (.=99)
	
	egen PHQ_1 = rowtotal(dpq010 dpq020 dpq030 dpq040 dpq050 dpq060 dpq070 dpq080 dpq090)
	
gen PHQ_2=.
replace PHQ_2 = 0 if PHQ_1<10 & PHQ_1 !=891
replace PHQ_2 = 1 if PHQ_1>=10 & PHQ_1 !=891
replace PHQ_2 = 99 if PHQ_1 == 891

recode PHQ_2 (99=.)

tab PHQ_2,m
*
	recode dpq100 (7=.)(9=.)

	
*Gender
label define riagendr_label 1 "Male" 2 "Female"
label values riagendr riagendr_label

*Age
tab ridageyr
gen age=. 
replace age= ridageyr if  ridageyr >0

*Race/Ethnicity
label define ridreth3_label 1 "Mexican American" 2 "Other Hispanic" 3 "Non-Hispanic White" 4 "Non-Hispanic Black" 6 "Non-Hispanic Asian" 7 "Other race - including multiracial"
label values ridreth3 ridreth3_label

*Citizenship status
recode dmdcitzn (9=.)(7=.)
label define dmdcitzn_label 1 "Citizen by birth or naturalization" 2 "Not a US citizen"
label values dmdcitzn dmdcitzn_label

*Educational Level - Adults 20+
recode dmdeduc2 (9=.)(7=.)
label define dmdeduc2_label 1 "Less than 9th grade" 2 "9-11th grade (includes 12th grade w/o diploma)" 3 "HS graduate/GED or equivalent" 4 "Some college or AA degree" 5 "College Grad or above"
label values dmdeduc2 dmdeduc2_label 

gen education=. 
replace education=0 if dmdeduc2 ==4 | dmdeduc2 ==5
replace education=1 if dmdeduc2 ==3 
replace education=2 if dmdeduc2 ==2 | dmdeduc2 ==1


*Marital Status
recode dmdmartl (99=.)(77=.)
label define dmdmartl_label 1 "Married" 2 "Widowed" 3 "Divorced" 4 "Seperated" 5 "Never Married" 6 "Living with partner"
label values dmdmartl dmdmartl_label

gen marital=. 
replace marital=0 if dmdmartl==1 // married
replace marital=1 if dmdmartl==2 | dmdmartl==3 | dmdmartl==4 // formar married
replace marital=2 if dmdmartl==5 // never married
replace marital=3 if dmdmartl==6 // living with partenr

*smoking 
gen smoking=. 
replace smoking = 1 if smq020==2 //not smoker
replace smoking = 2 if smq020==1 & smq040== 3 // former smoker
replace smoking = 3 if smq020==1 & smq040== 1 | smq040== 2 // current smoker


*Blood Pressure
gen bloodpressure = (bpxsy2+bpxsy3)/2
replace bloodpressure = 1 if (bloodpressure>=90)&(bloodpressure<=120)
replace bloodpressure = 2 if (bloodpressure>120)&(bloodpressure<130)
replace bloodpressure = 3 if (bloodpressure>=130)&(bloodpressure<140)
replace bloodpressure = 4 if (bloodpressure>=140)&(bloodpressure<180)
replace bloodpressure = . if (bloodpressure>=180)&(bloodpressure<240)
replace bloodpressure = . if (bloodpressure<90)&(bloodpressure>60)
label define bloodpressure_label 1 "Good" 2 "Slightly elevated" 3 "HTN1" 4 "HTN2"
label values bloodpressure bloodpressure_label
*BMI 
rename bmxbmi bmi
replace bmi = 1 if (bmi>=18.5)&(bmi<25)
replace bmi = 2 if (bmi>11)&(bmi<18.5)
replace bmi = 3 if (bmi>=25)&(bmi<30)
replace bmi = 4 if (bmi>=30)&(bmi<83)
label define bmi_label 1 "Normal" 2 "Underweight"  3 "Overweight" 4 "Obese"
label values bmi bmi_label

*US born
tab dmdborn4
recode  dmdborn4 (77=.) (99=.)

*PAQ
foreach x of varlist paq605 paq620 paq635 paq650 paq665 {
   replace `x'= .  if `x' == 7
    replace `x'= .  if `x' == 9
}
*Recode refused (77) and don't know (99) as missing (.)
foreach x of varlist paq610 paq625 paq640 paq655 paq670 {
   replace `x'= .  if `x' == 77
   replace `x'= .  if `x' == 99
}

*Recode refused (7777) and don't know (9999) as missing (.)
foreach x of varlist pad615 pad630 pad645 pad660 pad675 pad680 {
   replace `x'= .  if `x' == 7777
   replace `x'= .  if `x' == 9999
}
gen vwminw=. // Vigorous work min/wk
gen vwminpw=. //Vigorous work MET 
replace vwminw=0 if paq605==2
replace vwminpw=0 if paq605==2

replace vwminw=pad615*paq610 if paq605==1
replace vwminpw=vwminw*8

label var vwminw "Vigorous work min/wk"
label var vwminpw "Vigorous work MET"

*Moderate work activity min/wk and met min per week
gen mwminw=. // Moderate work min/wk
gen mwminpw=. // Moderate work MET 
replace mwminw=0 if paq620==2
replace mwminpw=0 if paq620==2

replace mwminw=pad630*paq625 if paq620==1
replace mwminpw=mwminw*4

label var mwminw "Moderate work min/wk"
label var mwminpw "Moderate work MET"

gen PAQ = .
replace PAQ = 0 if (PAQ != mwminpw) | (PAQ != vwminpw)
replace PAQ = 1 if mwminpw
replace PAQ = 2 if vwminpw

*alcohol
recode alq101 (9=.)
rename alq101 alcohol
 

*Survey Weights*	
svyset [pw = wtmec6yr], psu(sdmvpsu) strata(sdmvstra) singleunit(centered) vce(jackknife)

global covariates "age i.riagendr i.dmdcitzn i.marital i.PAQ i.bmi i.bloodpressure i.alcohol i.smoking"
global cp "svy, subpop(asian_a):"
global form ", col obs cellwidth(12) format(%12.2g)"

$cp mean age
$cp tab riagendr $form
$cp tab diabetes $form
$cp tab prediabetes $form
$cp tab slp_zz $form
$cp tab PHQ_2 $form
$cp tab dmdcitzn $form
$cp tab marital $form
$cp tab bmi $form
$cp tab smoking $form
$cp tab bloodpressure $form
$cp tab PAQ $form
$cp tab alcohol $form

//Bivarate resutls//
*diabetes
$cp mean age, over(diabetes)
test [age]0 = [age]1
$cp tab riagendr diabetes $form
$cp tab slp_zz diabetes $form
$cp tab PHQ_2 diabetes $form
$cp tab dmdcitzn diabetes $form
$cp tab marital diabetes $form
$cp tab bmi diabetes $form
$cp tab smoking diabetes $form
$cp tab bloodpressure diabetes $form
$cp tab PAQ diabetes $form
$cp tab alcohol diabetes $form

*prediabetes
$cp tab riagendr prediabetes $form
$cp tab slp_zz prediabetes $form
$cp tab PHQ_2 prediabetes $form
$cp tab dmdcitzn prediabetes $form
$cp tab marital prediabetes $form
$cp tab bmi prediabetes $form
$cp tab smoking prediabetes $form
$cp tab bloodpressure prediabetes $form
$cp tab PAQ prediabetes $form
$cp tab alq151 prediabetes $form

***MV
$cp logistic diabetes ib3.slp_zz (age i.riagendr i.dmdcitzn i.marital)
$cp logistic diabetes ib3.slp_zz (age i.riagendr i.dmdcitzn i.marital) (i.alcohol i.smoking)
$cp logistic diabetes ib3.slp_zz (age i.riagendr i.dmdcitzn i.marital) (i.alcohol i.smoking) (i.PAQ i.bmi i.bloodpressure)

test age // walds test
nestreg, quietly: $cp logistic diabetes slp_zz (age riagendr dmdcitzn marital) (alcohol smoking) (PAQ bmi bloodpressure) // nested reg

$cp logistic diabetes ib3.slp_zz $covariates // 2011-2016


