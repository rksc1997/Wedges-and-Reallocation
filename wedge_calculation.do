/*====================================================
This do file executes the following tasks:
(1) Cleans ASI and Prowess Financial dataset
(2) Estimate Industry-wise production functions and Firm-level Cost-Gaps
(3) Pass-through regressions: Impact of lending to zombies firms on their cost-gaps
(4) Aggregate Productivity Growth Disaggregation and Counterfactual Simulations
Author: Rahul Singh Chauhan
Date Made: 05/05/2020
====================================================*/
// Preamble
clear all
set more off
set matsize 11000
version 15
cap log close
gl wedges "C:\Users\rahul.chauhan\Desktop\Wedges"
gl lp "C:\Users\rahul.chauhan\Desktop\Wedges\lp"
// (1) Clean ASI and Prowess Data
//Cleaning Industry IDs
cd "$wedges"
import delimited "$wedges\id\43091_1_5_20200217_152402_dat.txt", delimiter("|") varnames(1) 
tostring nic_prod_code,replace
gen nic_code_digit = length(nic_prod_code)
gen nic_2_digit = substr(nic_prod_code,1,2) 
drop if co_code ==.
gen ind_classification =""						
replace ind_classification = 	"Agriculture-Direct & Indirect Finance" if nic_2_digit == "01" | nic_2_digit =="02" | nic_2_digit =="03"
replace ind_classification = 	"Mining and Quarrying"	if nic_2_digit == 		"05"
replace ind_classification = 	"Mining and Quarrying"	if nic_2_digit == 		"6"
replace ind_classification = 	"Mining and Quarrying"	if nic_2_digit == 		"07"
replace ind_classification = 	"Mining and Quarrying"	if nic_2_digit == 		"8"
replace ind_classification = 	"Mining and Quarrying"	if nic_2_digit == 		"09"
replace ind_classification = 	"Trade"			if nic_2_digit == "46" | nic_2_digit == "45"
replace ind_classification = 	"Trade"			if nic_2_digit == "47"
replace ind_classification = 	"Food Manufacturing  & Processing"			if nic_2_digit == "10"
replace ind_classification = 	"Food Manufacturing  & Processing"			if nic_2_digit == 		"11"
replace ind_classification = 	"Food Manufacturing  & Processing"			if nic_2_digit == 		"12"
replace ind_classification = 	"Textiles, Leather & Leather Products"			if nic_2_digit == 		"13"
replace ind_classification = 	"Textiles, Leather & Leather Products"			if nic_2_digit == 		"14"
replace ind_classification = 	"Textiles, Leather & Leather Products"			if nic_2_digit == 		"15"
replace ind_classification = 	"Paper, Paper Products & Printing"			if nic_2_digit == 		"17"
replace ind_classification = 	"Paper, Paper Products & Printing"			if nic_2_digit == 		"18"
replace ind_classification = 	"Chemical and Chemical prod."			if nic_2_digit == 		"21"
replace ind_classification = 	"Rubber and Plastic Prod."			if nic_2_digit == 		"22"
replace ind_classification = 	"Chemical and Chemical prod."			if nic_2_digit == 		"20"
replace ind_classification = 	"Petroleum, Coal Products & Nuclear Fuels"			if nic_2_digit == 		"06"
replace ind_classification = 	"Manufacture of Cement & Cement Products"			if nic_2_digit == 		"23"
replace ind_classification = 	"Basic Metals and Metal Products"			if nic_2_digit == 		"24"
replace ind_classification = 	"Basic Metals and Metal Products"			if nic_2_digit == 		"25"
replace ind_classification = 	"Engineering"			if nic_2_digit == 		"29"
replace ind_classification = 	"Engineering"			if nic_2_digit == 		"28"
replace ind_classification = 	"Engineering"			if nic_2_digit == 		"27"
replace ind_classification = 	"Engineering"			if nic_2_digit == 		"26"
replace ind_classification = 	"Vehicles,Vehicle Parts & Transport Equipments"			if nic_2_digit == 		"30"
replace ind_classification = 	"Vehicles,Vehicle Parts & Transport Equipments"			if nic_2_digit == 		"29"
replace ind_classification = 	"Other Industries"			if nic_2_digit == 		"32"
replace ind_classification = 	"Other Industries"			if nic_2_digit == 		"33"
replace ind_classification = 	"Other Industries"			if nic_2_digit == 		"31"
replace ind_classification = 	"Wood and Product of Wood"			if nic_2_digit == 		"16"
replace ind_classification = 	"Electricity, Gas & Water"			if nic_2_digit == 		"35"
replace ind_classification = 	"Construction"			if nic_2_digit == 		"41"
replace ind_classification = 	"Construction"			if nic_2_digit == 		"42"
replace ind_classification = 	"Construction"			if nic_2_digit == 		"43"
replace ind_classification = 	"Transport Operators"			if nic_2_digit == 		"53"
replace ind_classification = 	"Transport Operators"			if nic_2_digit == 		"49"
replace ind_classification = 	"Transport Operators"			if nic_2_digit == 		"50"
replace ind_classification = 	"Transport Operators"			if nic_2_digit == 		"51"
replace ind_classification = 	"Transport Operators"			if nic_2_digit == 		"52"
replace ind_classification = 	"Finance"			if nic_2_digit == "64" | nic_2_digit == "65"  | nic_2_digit == "66"
replace ind_classification = 	"Petroleum, Coal Products & Nuclear Fuels"			if nic_2_digit == "06" | nic_2_digit == "19"
replace ind_classification = "Professional Services" if ind_classification ==""
rename ind_classification sector
save identity.dta, replace
clear all
//non-financial data superset
import delimited "$wedges\for_wedges2.txt", delimiter("|") clear
rename sa_finance1_cocode co_code
merge m:1 co_code using "$wedges\identity.dta", keep(3) nogen
g date=date(sa_finance1_year, "DMY")
g year=year(date)
sort sector year
encode sector, generate(sec_id)
egen id=group(sector)
qui sum id
return list
local max=r(max)
rename sa_company_name name
drop sa_ann_rep* sa_fs_format sa_finance1_year
destring sa*, replace force
duplicates drop co_code year, force
save non_financial.dta, replace //raw-financial data
//Production data
clear all
import delimited C:\Users\rahul.chauhan\Desktop\Wedges\product.txt, delimiter("|") 
g date=date(prod_date, "DMY")
g year=year(date)
destring production sales_qty, replace force
collapse (sum) production sales_qty, by(company_name year)
rename company_name sa_company_name
cd "C:\Users\rahul.chauhan\Desktop\Wedges"
save prod.dta, replace // production(use if needed)
// Company identifcation
import delimited cin.txt, delimiter("|") clear
drop if cin_code=="NA"
rename cin_code cin
egen id=group(cin)
save cin.dta, replace
merge 1:1 cin using feb12_cin.dta, gen(_mer_cin)
save simple_cin_merge.dta, replace 

// Interest rates
import delimited interest.txt, delimiter("|") clear
g date=date(sa_finance1_year, "DMY")
g year=year(date)
rename sa_finance1_cocode co_code
destring sa_interest_exp sa_debt sa_secured_borrowings sa_unsecured_borrowings, replace force
duplicates tag sa_company_name year, generate(dup)
drop if dup>0
drop dup
merge 1:1 sa_company_name year using prod.dta, gen(_merge)
duplicates tag co_code year, generate(dup2)
drop if dup2>0
drop dup2
merge 1:1 co_code year using non_financial.dta, gen(_k)
drop if _k!=3
save financials.dta, replace
//final financial dataset used 

//Rawmaterial data 
import delimited rawmat.txt, delimiter("|") clear
destring rawmat_consump_qty rawmat_consump_val, replace force
g date=date(rawmat_date, "DMY")
g year=year(date)
rename company_name sa_company_name
collapse (sum) rawmat_consump_val rawmat_consump_qty, by(sa_company_name year)
g avg_rawmat_price=(rawmat_consump_val*1000000)/rawmat_consump_qty
save rawmat.dta, replace

//Incorporation year 
import delimited incorp.txt, delimiter("|") clear
destring incorporation_year, replace force
rename incorporation_year inc_year
save incorp.dta, replace

//Credit_ratings
import delimited credit_rating.txt, delimiter("|") clear
egen rating_group=group(rating)
egen rat_defn=group(rating_grade_defn)
g date=date(rating_date, "DMY")
g year=year(date)
rename ratig_cocode co_code
rename company_name sa_company_name
save rating.dta, replace
g rating_score=8 if rat_defn==5
replace rating_score=7 if rat_defn==4
replace rating_score=6 if rat_defn==7
replace rating_score=5 if rat_defn==1
replace rating_score=4 if rat_defn==6
replace rating_score=3 if rat_defn==9
replace rating_score=2 if rat_defn==3
replace rating_score=1 if rat_defn==2
drop if rat_defn==8
collapse rating_score, by(co_code sa_company_name year)
save rating_score.dta, replace
****

//Variable Generation
cd "C:\Users\rahul.chauhan\Desktop\Wedges"
use financials.dta, clear
bys co_code year: gen duplicate=cond(_N==1, 0, _n)
drop if duplicate>1
keep if sa_compensation_to_employees>0 & sa_rawmat_stores_spares>0 & sa_net_fixed_assets>0 & sa_sale_of_goods>0
bys id year: gen wage=sa_compensation_to_employees/sa_no_of_employees
gen intpay=sa_interest_exp/sa_debt
bys id year: egen mean_intpay=mean(intpay)
bys id year: egen mean_wage=mean(wage)
gen labor=sa_compensation_to_employees/mean_wage
drop if sa_compensation_to_employees==.
g labor_coeff=.
g cap_coeff=.
mat A = J(1,2,.)
xtset co_code year
winsor2 sa_sales sa_net_fixed_assets sa_compensation_to_employees sa_rawmat_stores_spares, cuts(1 99)
merge 1:1 sa_company_name year using rating_score.dta, nogen
merge 1:1 sa_company_name year using rawmat.dta, nogen
merge m:1 co_code using incorp.dta, nogen
merge m:1 sector year using def.dta, nogen
merge m:1 year using cf_def.dta, nogen
drop if year<1988 
sort sector year
g k=sa_net_fixed_assets/agri_def if id==7
replace k=sa_net_fixed_assets/min_def if id==9
replace k=sa_net_fixed_assets/mfg_def if id==1|id==2|id==5|id==6|id==11|id==12|id==14|id==15|id==18|id==19
replace k=sa_net_fixed_assets/elec_def if id==4
replace k=sa_net_fixed_assets/const_def if id==3
replace k=sa_net_fixed_assets/trd_def if id==16
replace k=sa_net_fixed_assets/trans_def if id==17
replace k=sa_net_fixed_assets/fin_def if id==6
replace k=sa_net_fixed_assets/prof_def if id==13
replace k=sa_net_fixed_assets/oth_def if id==10
g l=labor
g pq=sa_sales/GO_def
g m=sa_rawmat_stores_spares/M_def
egen s=rowtotal(sa_power_fuel_water_charges sa_oth_op_exp_hotel_restrnt sa_oth_op_exp_transport_cos ///
 sa_oth_op_exp_telecom_cos sa_oth_op_exp_hospitals sa_oth_op_exp_recreation_cos sa_oth_op_exp_edu_cos sa_fin_serv_exp)
g s_def=s/S_def
g lns=ln(s)
g lny=ln(pq)
g lnl=ln(l)
g lnk=ln(k)
g lnm=ln(m)
sort co_code year
egen y=group(year)
save for_calc.dta, replace
//(2) Estimating Firm-level cost gaps) 
****Levinsohn-Petrin Estimation****
cd "C:\Users\rahul.chauhan\Desktop\Wedges"
use for_calc.dta, clear
qui sum id
return list
local m=r(max)
//Generate three year panels
g yy=1 if y<4
replace yy=2 if y>3 & y<7
replace yy=3 if y>6 & y<10
replace yy=4 if y>9 & y<13
replace yy=5 if y>12 & y<16
replace yy=6 if y>15 & y<19
replace yy=7 if y>18 & y<22
replace yy=8 if y>21 & y<25
replace yy=9 if y>24 & y<28
replace yy=10 if y>27 & y<31
replace yy=11 if y>30 
erase dat.dta
tempfile dat
save dat.dta, replace
//Within industry-estimation of labor and capital coefficients
su y, detail
forvalues i=1/`r(max)'{
use if id==`i' using dat.dta, clear
qui su yy
local beg=r(min)
local end=r(max)
forvalues j=`beg'/`end'{
use if id==`i' & yy==`j' using dat.dta, clear
drop if lny==.|lnk==.|lnm==.|lnl==.|lns==.
xtset co_code y
local n=r(N)
capture prodest lny, method(lp) free(lnl) state(lnk) proxy(lnm) control(lns) fsres(tfp_lp) acf
capture mat A=e(b)
capture scalar bw = A[1,1]
g labor_coeff`i'`j' = bw if id==`i'
capture scalar by = A[1,2]
g cap_coeff`i'`j' = by if id==`i'
g tfp_lp`i'`j'=e(tfp_lp)
save "C:\Users\rahul.chauhan\Desktop\Wedges\lp3\est_ind`i'`j'.dta", replace
}
}
cd "C:\Users\rahul.chauhan\Desktop\Wedges\lp3"
! dir *.dta /a-d /b >"C:\Users\rahul.chauhan\Desktop\Wedges\lp3\filelist3.txt"
file open myfile3 using "C:\Users\rahul.chauhan\Desktop\Wedges\lp3\filelist3.txt", read
file read myfile3 line3
use `line3'
save est_lp.dta, replace
file read myfile3 line3
while r(eof)==0 { /* while you're not at the end of the file */
	append using `line3'
	file read myfile3 line3
}
file close myfile3
egen lab_lp=rowtotal(labor_coeff*)
egen cap_lp=rowtotal(cap_coeff*)
egen tfp_levin=rowtotal(tfp_lp*)
save est_lp.dta, replace

******Estimating cost gaps*****

cd "C:\Users\rahul.chauhan\Desktop\Wedges\lp"
use est_lp.dta, clear
g pq_k=pq/k
g pq_l=pq/l
g mrs_k=pq_k*cap_lp
g mrs_l=pq_l*lab_lp
g k_gap=mrs_k-intpay
g l_gap=mrs_l-mean_wage
sort co_code year
g dkgap=D.k_gap
g dlnkgap=dkgap/k_gap
g dlgap=D.l_gap
g dlnlgap=dlgap/l_gap
g dtfp=D.tfp_levin
g dlntfp=dtfp/tfp_levin
bys id year: egen tot_debt=sum(sa_debt)
g co_debt_share=sa_debt/tot_debt
bys co_code year: g co_age=year-inc_year
bys co_code: g sales_g=D.sa_sales/L.sa_sales
keep id year sector co_code company_name pq dlntfp  dlnkgap dlnlgap l_gap k_gap lab_lp s VA_r cap_lp l tfp_levin sa_debt co_debt_share sa_sales /*
*/sales_g sa_interest_exp sa_debt mean_wage sa_net_fixed_assets k co_age rating_score labor
save "$wedges\lp_fin.dta", replace



//(3) Pass Through Regressions
use "$wedges\simple_cin_merge.dta", clear
duplicates drop co_code year, force
merge 1:m co_code using C:\Users\rahul.chauhan\Desktop\Wedges\lp_fin.dta, nogen
duplicates drop co_code year, force
xtset co_code year
winsor2 sa_debt k_gap k_gap_int l_gap, cuts(1 99)
g lnd=ln(sa_debt)
g lnl=ln(labor)
g del_debt=D.lnd
g m_t=mfeb12_treat*mzm7
g debt_share_undep=co_debt_share*mzm7
g intpay=sa_interest_exp/sa_debt
g assets_sales=sa_net_fixed_assets/sa_sales
sort co_code year
bys co_code: g del_wage=D.mean_wage
sort co_code year
bys co_code: g del_l= D.lnl
gl controls co_age rating_score 
gl controls_first sales_g assets_sales intpay
reghdfe del_debt debt_share_undep co_debt_share $controls_first, noabsorb vce(cl id year) nocons
predict p_del_debt, xb
reghdfe l_gap p_del_debt $controls, noabsorb nocons
outreg2 using pass_through.doc, replace
reghdfe k_gap_int p_del_debt $controls, noabsorb nocons
outreg2 using pass_through.doc, append
reghdfe del_wage p_del_debt $controls, noabsorb nocons
outreg2 using pass_through.doc, append
reghdfe del_l p_del_debt $controls, noabsorb nocons
outreg2 using pass_through.doc, append
bys year: egen lab_tot=sum(labor)
bys year: egen cap_tot=sum(k)
g lab_share=labor/lab_tot
g cap_share=k/cap_tot
sort co_code year
bys co_code: g del_lab_shar=D.lab_share
bys co_code: g del_cap_shar=D.cap_share
g dlsl=del_lab_shar/lab_share
g dlsk=del_cap_shar/cap_share
winsor2 sa_debt k_gap l_gap dlsl dlsk, cuts(1 99) replace
g lnd=ln(sa_debt)
g lnl=ln(labor)
g del_debt=D.lnd
g m_t=mfeb12_treat*mzm7
g debt_share_undep=co_debt_share*mzm7
g intpay=sa_interest_exp/sa_debt
g assets_sales=sa_net_fixed_assets/sa_sales
bys year: egen sum_VA=sum(VA_r)
g DOM=pq/sum_VA
g lnA=ln(tfp_levin)
sort co_code year
g dA=D.tfp_levin
g dlnA=dA/tfp_levin
g DOM_dlnA=DOM*dlnA
bys year: egen tfp_y=sum(DOM_dlnA)

sort co_code year
g dK=D.cap_tot
g dlnK=dK/cap_tot
sort co_code year
g dL=D.lab_tot
g dlnL=dL/lab_tot
sort co_code year
bys co_code: g del_wage=D.mean_wage
sort co_code year
bys co_code: g del_l= D.lnl
gl controls co_age sales_g
gl controls_first sales_g assets_sales 
local levelsof year
*erase for_reg.dta
tempfile for_reg
save for_reg.dta
preserve
forvalues i=1997/2016{
use if `i'==year using for_reg.dta, clear
reghdfe del_debt debt_share_undep co_debt_share $controls_first, noabsorb nocons
g b_treatment`i'=_b[co_debt_share]
g b_treatgroup`i'=_b[debt_share_undep]
predict p_del_debt, xb
outreg2 using first_stage`i'.doc, replace
reghdfe dlnlgap p_del_debt $controls, noabsorb nocons
g b_l_gap`i'=_b[p_del_debt]
predict p_dlgap, xb
egen tot_dlgap=sum(p_dlgap)
outreg2 using pass_through_lp`i'.doc, replace
reghdfe dlnkgap p_del_debt $controls, noabsorb nocons
g b_k_gap`i'=_b[p_del_debt]
predict p_dkgap, xb
egen tot_dkgap=sum(p_dkgap)
outreg2 using pass_through_lp`i'.doc, append
reghdfe dlsl p_del_debt $controls, noabsorb nocons
predict pdlsl, xb
g b_pdlsl`i'=_b[p_del_debt]
egen tot_pdlsl=sum(pdlsl)
outreg2 using pass_through_lp`i'.doc, append
reghdfe dlsk p_del_debt $controls, noabsorb nocons
predict pdlsk, xb
g b_pdlsk`i'=_b[p_del_debt]
egen tot_pdlsk=sum(pdlsk)
outreg2 using pass_through_lp`i'.doc, append
reghdfe dlntfp p_del_debt $controls, noabsorb nocons
g b_tfp`i'=_b[p_del_debt]
predict p_dlntfp, xb
egen tot_dlntfp=sum(p_dlntfp)
outreg2 using pass_through_lp`i'.doc, append
save reg_stage`i'.dta, replace
}
restore
reghdfe del_debt debt_share_undep co_debt_share $controls_first, noabsorb nocons
g b_treatment=_b[co_debt_share]
g b_treatgroup=_b[debt_share_undep]
predict p_del_debt, xb
outreg2 using first_stage.doc, replace
reghdfe dlnlgap p_del_debt $controls, noabsorb nocons
g b_l_gap=_b[p_del_debt]
predict p_dlgap, xb
egen tot_dlgap=sum(p_dlgap)
outreg2 using pass_through_lp.doc, replace
reghdfe dlnkgap p_del_debt $controls, noabsorb nocons
g b_k_gap=_b[p_del_debt]
predict p_dkgap, xb
egen tot_dkgap=sum(p_dkgap)
outreg2 using pass_through_lp.doc, append
reghdfe dlsl p_del_debt $controls, noabsorb nocons
predict pdlsl, xb
g b_pdlsl=_b[p_del_debt]
egen tot_pdlsl=sum(pdlsl)
outreg2 using pass_through_lp.doc, append
reghdfe dlsk p_del_debt $controls, noabsorb nocons
predict pdlsk, xb
g b_pdlsk=_b[p_del_debt]
egen tot_pdlsk=sum(pdlsk)
outreg2 using pass_through_lp.doc, append
reghdfe dlntfp p_del_debt $controls, noabsorb nocons
g b_tfp=_b[p_del_debt]
predict p_dlntfp, xb
egen tot_dlntfp=sum(p_dlntfp)
outreg2 using pass_through_lp.doc, append
g dom_g_dlsk=DOM*dlsk*k_gap
g dom_g_dlsl=DOM*dlsl*l_gap
bys year: egen tot_dom_dlsl=sum(dom_g_dlsl)
bys year: egen tot_dom_dlsk=sum(dom_g_dlsk)
bys year: g reallc=tot_dom_dlsl+tot_dom_dlsk
//(4) Aggregate Productivity Disaggregation and Counterfactual Simulations
******APG*****
g dom_g_dlnK=DOM*k_gap_int*dlnK
g dom_g_dlnL=DOM*l_gap*dlnL
bys year: egen tot_dom_dlnL=sum(dom_g_dlnL)
bys year: egen tot_dom_dlnK=sum(dom_g_dlnK)
sort co_code year
egen dist_avg=rowtotal(tot_dom_dlnL tot_dom_dlnK)
****
egen apg=rowtotal(tfp_y dist_avg reallc)
collapse apg tfp_y reallc dist_avg tot_dom_dlsl tot_dom_dlsk tot_dom_dlnL tot_dom_dlnK , by(year)
save lp_apg_dist.dta, replace
*/

*****Counterfactual Simulation******
cd "$lp"
gl sample_lp "C:\Users\rahul.chauhan\Desktop\Wedges\samples_lp"
gl sample_lp2 "D:\samples_lp2"
forvalues k=1997/2016{
use reg_stage`k'.dta, clear
erase simulation.dta
tempfile simulaton
save simulation
forvalues i=1/10000{
use if year==`k' using simulation.dta, clear
sample 10 if mzm7!=1
g te=b_treatment`k'+b_treatgroup`k'
g l_gap_count=b_l_gap`k'*te*co_debt_share
g k_gap_count=b_k_gap`k'*te*co_debt_share
g dlsl_count=b_pdlsl`k'*te*co_debt_share
g dlsk_count=b_pdlsk`k'*te*co_debt_share
g tfp_count=b_tfp`k'*te*co_debt_share
g sim_id=`i'
save "$sample_lp2\sample`k'`i'.dta", replace
}
use "$sample_lp2\sample`k'1".dta, clear
forvalues j=2/10000{
append using "$sample_lp2\sample`k'`j'".dta
erase sample`k'`j'.dta
erase sample`k'1.dta
}
save "$sample_lp2\master_sample`k'.dta", replace
}
erase sample*.dta
cd $sample_lp2
forvalues k=1997/2016{
use $sample_lp2\master_sample`k'.dta, clear
g dom_g_dlsk=DOM*dlsk*k_gap
g dom_g_dlskc=DOM*dlsk_count*k_gap_count
g dom_g_dlsl=DOM*dlsl*l_gap
g dom_g_dlslc=DOM*dlsl_count*l_gap_count
bys year: egen tot_dom_dlsl=sum(dom_g_dlsl)
bys year: egen tot_dom_dlslc=sum(dom_g_dlslc)
bys year: egen tot_dom_dlsk=sum(dom_g_dlsk)
bys year: egen tot_dom_dlskc=sum(dom_g_dlskc)
bys year: g reallc=tot_dom_dlsl+tot_dom_dlsk
bys year: g reallc_c=tot_dom_dlskc+tot_dom_dlslc
******
g dom_g_dlnK=DOM*k_gap*dlnK
g dom_g_dlnKc=DOM*k_gap_count*dlnK
g dom_g_dlnL=DOM*l_gap*dlnL
g dom_g_dlnLc=DOM*l_gap_count*dlnL
bys year: egen tot_dom_dlnL=sum(dom_g_dlnL)
bys year: egen tot_dom_dlnL_c=sum(dom_g_dlnLc)
bys year: egen tot_dom_dlnK=sum(dom_g_dlnK)
bys year: egen tot_dom_dlnK_c=sum(dom_g_dlnKc)
*bys year: egen tot_dom_dlnM=sum(dom_g_dlnM)
sort co_code year
egen dist_avg=rowtotal(tot_dom_dlnL tot_dom_dlnK)
egen dist_avg_c=rowtotal(tot_dom_dlnL_c tot_dom_dlnK_c)
****
egen apg=rowtotal(tfp_y dist_avg reallc)
egen apg_c=rowtotal(tfp_y dist_avg_c reallc_c)
su apg 
su apg_c
collapse apg apg_c dist_avg dist_avg_c reallc reallc_c tot_dom_*, by(year)
save apg_results_lp`k'.dta, replace
}

use apg_results_lp1997.dta, clear
forvalues i=1998/2016{
append using apg_results_lp`i'.dta
erase apg_results_lp`i'.dta
}
erase apg_results_lp1997.dta
save apg_results_lp.dta
