clear
program drop _all
set more off
set matsize 10000

* enter your directory here
cd "C:\Users"

* This program runs each component of the results for the paper. To see the results of just one part, add a star to the front of the others.
program define run_all
summary_statistics // Table 1 and Table S2
figures_time // Fig. 2 and Fig. S4 panel (d)
figures_time_si // Panels (a)-(c) Fig. S4
main_figure // Fig. 3
main_figure_fe // Fig. S3
NPV_baseline // Statement end of section 3
tables_main_effect_income // Table S4-SA7
sunroof_panel // Table S8 and Table S9
survey // Table 2 and Table S10
financing// Table S11
end

************************************************************************************************************************************************************************************************************************************************
program define summary_statistics
use SI_attracts_sunlight_panel.dta, replace
sort townid
drop if approved_month==.

gen pop_density = round(population_town/land_area)
gen frac_republican = republican_registered/voters
gen frac_democratic = democratic_registered/voters
gen frac_detached_units = detached_units_town/housing_units_town
gen frac_families = families/population_town
gen frac_white = white/population_town
gen frac_black = black/population_town
gen frac_asian = asian/population_town
replace pop_over_65 = pop_over_65/100
replace college = college/100
replace travel_time_to_work_60_more = travel_time_to_work_60_more/100
replace percent_below_poverty_level = percent_below_poverty_level/100
replace median_income = median_income

format pop_density median_income %10.0fc
format median_age %10.1fc
format pop_over_65 frac_white frac_black frac_asian frac_families mean_travel_time_to_work travel_time_to_work_60_more college percent_below_poverty_level unemployment_rate frac_detached_units frac_republican frac_democratic %4.3f

label variable pop_density "Population Density"
label variable median_income "Median Household Income"
label variable median_age "Median Age"
label variable pop_over_65 "Fraction Pop Over 65"
label variable frac_white "Fraction White"
label variable frac_black "Fraction Black"
label variable frac_asian "Fraction Asian"
label variable frac_families "Fraction Families"
label variable mean_travel_time_to_work "Mean Travel Time To Work"
label variable travel_time_to_work_60_more "Fraction Commute 60+ Miles"
label variable college "Fraction College Grads"
label variable percent_below_poverty_level "Fraction Below Poverty"
label variable unemployment_rate "Fraction Unemployed"
label variable frac_detached_units "Fraction Detached Dwelling"
label variable frac_republican "Fraction Regist. Republican"
label variable frac_democratic "Fraction Regist. Democrat"

egen cum_installations = sum(installations) if approved_month <= tm(2017m3), by(townid) 
egen cum_installed_capacity = sum(installed_capacity) if approved_month <= tm(2017m3), by(townid)

label variable installations "Installations per month"
label variable installations_1000hh "Installations per month per 1000hh"
label variable installations_1000ooh "Installations per month per 1000ooh"
label variable cum_installations "Cumulative installations"
label variable installed_capacity "Capacity installed per month"
label variable installed_capacity_1000hh "Capacity installed per month per 1000hh"
label variable installed_capacity_1000ooh "Capacity installed per month per 1000ooh"
label variable owner_occupied_houses_town "Owner-occupied houses"

format installations installations_1000hh installations_1000ooh installed_capacity installed_capacity_1000hh installed_capacity_1000ooh %10.1fc
format cum_installations median_income owner_occupied_houses_town %10.0fc

************************************************************************************************************************
//TABLE A.2: PANEL A
************************************************************************************************************************
sutex installations installations_1000hh installations_1000ooh cum_installations installed_capacity installed_capacity_1000hh installed_capacity_1000ooh ///
cum_installed_capacity median_income owner_occupied_houses_town population_town, lab nobs key(descstat) minmax replace file(descstat_townmonth.tex)

replace group=1 if Individual_town==1
replace group=2 if Community_town==1
replace group=3 if Control_town==1
drop if group==.

************************************************************************************************************************
//TABLE A.2: PANEL B
************************************************************************************************************************
sutex installations installations_1000hh installations_1000ooh cum_installations installed_capacity installed_capacity_1000hh installed_capacity_1000ooh ///
cum_installed_capacity median_income owner_occupied_houses_town population_town, lab nobs key(descstat) minmax replace file(descstat_townmonth_Solarize.tex)

************************************************************************************************************************
//TABLE 1: Pooled sample balance of demographics and voter registration variables
************************************************************************************************************************
generate treat_Individual = (group==1)
generate treat_Community = (group==2)
balancetable (mean if group==1) (mean if group==2) (mean if group==3) (diff treat_Individual if group!=3) (diff treat_Individual if group!=2) (diff treat_Community if group!=1) ///
pop_density median_income median_age pop_over_65 frac_white frac_black frac_asian frac_families travel_time_to_work_60_more college percent_below_poverty_level frac_detached_units frac_republican frac_democratic using Table_of_Balance.tex ///
, vce(cluster townid) wide(mean sd pval) ctitles("Mean" "std. dev." "Mean" "std. dev." "Mean" "std. dev." "(1)(2)" "(1)(3)" "(2)(3)") replace varlabels booktabs displayformat nonumbers pvalues pval(fmt(%5.2f) nopar) staraux noobs ///
postfoot("\hline" "\multicolumn{10}{l}{Demographic variables from the 2013-2017 American Community Survey.  The p-values are for a pairwise two-}\\ \multicolumn{10}{l}{sided t-test of differences in means by group.} \end{tabular}") ///
 groups("(1) Self-interest" "(2) Pro-social" "(3) Control" "p-values", pattern(1 0 1 0 1 0 1) end("\cmidrule(lr){2-3} \cmidrule(lr){4-5} \cmidrule(lr){6-7} \cmidrule(lr){8-10}")) 
drop treat_Individual treat_Community

end
************************************************************************************************************************************************************************************************************************************************

************************************************************************************************************************
//FIGURE 2
************************************************************************************************************************
program define figures_time
use SI_attracts_sunlight_panel.dta, replace	
keep if Individual_town==1 | Community_town==1 | Control_town==1

replace group = 0 if Individual_town==1
replace group = 1 if Community_town==1
replace group = 2 if Control_town==1
sort townid approved_month

ren month_normalized Time_normalized

gen cost_kw=cost_total/installed_capacity/1000
gen av_capacity = installed_capacity/installations
drop if cost_kw == 0
drop if installed_capacity <1 & installed_capacity>0
drop if cost_kw>8  & cost_kw!=.

preserve
drop if Time_normalized<-13
keep if Time_normalized<0
collapse (sum) installations cost_total installed_capacity (mean) installations_1000ooh (max) owner_occupied_houses_town group, by(townid)
gen cost_kw = cost_total/installed_capacity/1000
gen av_capacity = installed_capacity/installations
oneway installations_1000ooh group, tabulate // self-interest, pro-social, control
pwmean installations_1000ooh, over(group) mcompare(tukey) effects
oneway cost_kw group, tabulate 
pwmean cost_kw, over(group) mcompare(tukey) effects
oneway av_capacity group, tabulate 
pwmean av_capacity, over(group) mcompare(tukey) effects
restore

//During campaign
preserve
collapse (sum) installations cost_total installed_capacity (mean) installations_1000ooh (max) owner_occupied_houses_town group, by(townid)
gen cost_kw = cost_total/installed_capacity
gen av_capacity = installed_capacity/installations
oneway installations_1000ooh group, tabulate 
pwmean installations_1000ooh, over(group) mcompare(tukey) effects
oneway cost_kw group, tabulate
pwmean cost_kw, over(group) mcompare(tukey) effects
oneway av_capacity group, tabulate
pwmean av_capacity, over(group) mcompare(tukey) effects
restore
drop if Time_normalized<-85

drop if R1to5_day==1 | R6to7_day==1 
collapse (mean) installations_1000ooh installations_1000hh cost_kw av_capacity (sem) installations_1000ooh_se=installations_1000ooh , by(group Time_normalized)

drop if Time_normalized>23
graph twoway (line installations_1000ooh Time_normalized if group==2, lw(medthick)) (line installations_1000ooh Time_normalized if group==1, lw(medthick)) (line installations_1000ooh Time_normalized if group==0, lw(medthick)), ///
	xline(2 3 3.9, lwidth(vvvthick) lcolor(gs12)) xline(0 6, lcolor(black)) ///
	legend(lab(1 "Control") lab(2 "Pro-social") lab(3 "Self-interest") row(1) region(col(white))) graphregion(color(white) lwidth(large)) bgcolor(white) ///
	plotregion(color(white)) ytitle("Installations per 1000 owner-occupied houses") xlabel(-80(10)20,nogrid) scheme(lean2) ///
	xtitle("Months before and after the start of the campaign")
	graph export Over_time_installations.eps, replace
	graph export Over_time_installations.png, replace
	
graph twoway (line cost_kw Time_normalized if group==2, lw(medthick)) (line cost_kw Time_normalized if group==1, lw(medthick)) (line cost_kw Time_normalized if group==0, lw(medthick)), ///
	xline(2 3 3.9, lwidth(vvvthick) lcolor(gs12)) xline(0 6, lcolor(black)) ///
	legend(lab(1 "Control") lab(2 "Pro-social") lab(3 "Self-interest") row(1) region(col(white))) graphregion(color(white) lwidth(large)) ///
	plotregion(color(white)) ytitle("Solar installation price [dollar/Watt]") xlabel(-80(10)20,nogrid) ///
	xtitle("Months before and after the start of the campaign")
	graph export Over_time_price.eps, replace
	graph export Over_time_price.png, replace
	
graph twoway (line av_capacity Time_normalized if group==2, lw(medthick)) (line av_capacity Time_normalized if group==1, lw(medthick)) (line av_capacity Time_normalized if group==0, lw(medthick)), ///
	xline(2 3 3.9, lwidth(vvvthick) lcolor(gs12)) xline(0 6, lcolor(black)) ///
	legend(lab(1 "Control") lab(2 "Pro-social") lab(3 "Self-interest") row(1) region(col(white))) graphregion(color(white) lwidth(large)) ///
	plotregion(color(white)) ytitle("Installation size [kW]") xlabel(-80(10)20,nogrid) ///
	xtitle("Months before and after the start of the campaign")
	graph export Over_time_size.eps, replace
	graph export Over_time_size.png, replace
end

************************************************************************************************************************************************************************************************************************************************
	
program define figures_time_si
use SI_attracts_sunlight_panel.dta, replace	
keep if Individual_town==1 | Community_town==1 | Control_town==1

replace group = 0 if Individual_town==1
replace group = 1 if Community_town==1
replace group = 2 if Control_town==1
sort townid approved_month
drop if R1to5_day==1 | R6to7_day==1
gen cost_kw=cost_total/installed_capacity/1000
drop if cost_kw == 0
drop if installed_capacity <1 & installed_capacity>0
drop if cost_kw>8  & cost_kw!=.
gen av_capacity = installed_capacity/installations
collapse (mean) installations_1000ooh installations_1000hh cost_kw av_capacity (sem) installations_1000ooh_se=installations_1000ooh , by(group approved_month)
	
graph twoway (line installations_1000ooh approved_month if group==2, lw(medthick)) (line installations_1000ooh approved_month if group==1, lw(medthick)) (line installations_1000ooh approved_month if group==0, lw(medthick)), ///
xline(688 689 690 691 692 692.5, lwidth(vthick) lcolor(gs12)) xline(694.5 695 696 697 698 698.5, lwidth(vthick) lcolor(gs12)) xline(700.5 701 702 703 704 704.54, lwidth(vthick) lcolor(gs12)) ///
	legend(lab(1 "Control") lab(2 "Pro-social") lab(3 "Self-interest") row(1) region(col(white))) graphregion(color(white) lwidth(large)) bgcolor(white) ///
	plotregion(color(white)) ytitle("Installations per 1000 owner-occupied houses") scheme(lean2) xtitle("Months in the sample") //xlabel(-80(10)20,nogrid) xline(0 6, lcolor(black)) xline(1.7 2.5 3.5 4.3, lwidth(vvvthick) lcolor(gs12)) ///
	graph export Over_time_installations_si.eps, replace
	graph export Over_time_installations_si.png, replace
	
graph twoway (line cost_kw approved_month if group==2, lw(medthick)) (line cost_kw approved_month if group==1, lw(medthick)) (line cost_kw approved_month if group==0, lw(medthick)), ///
xline(688 689 690 691 692 692.5, lwidth(vthick) lcolor(gs12)) xline(694.5 695 696 697 698 698.5, lwidth(vthick) lcolor(gs12)) xline(700.5 701 702 703 704 704.54, lwidth(vthick) lcolor(gs12)) ///
	legend(lab(1 "Control") lab(2 "Pro-social") lab(3 "Self-interest") row(1) region(col(white))) graphregion(color(white) lwidth(large)) ///
	plotregion(color(white)) ytitle("Solar installation price [dollar/Watt]") xtitle("Months in the sample") // xlabel(-80(10)20,nogrid) xline(0 6, lcolor(black)) xline(1.7 2.5 3.5 4.3, lwidth(vvvthick) lcolor(gs12)) ///
	graph export Over_time_price_si.eps, replace
	graph export Over_time_price_si.png, replace
	
graph twoway (line av_capacity approved_month if group==2, lw(medthick)) (line av_capacity approved_month if group==1, lw(medthick)) (line av_capacity approved_month if group==0, lw(medthick)), ///
xline(688 689 690 691 692 692.5, lwidth(vthick) lcolor(gs12)) xline(694.5 695 696 697 698 698.5, lwidth(vthick) lcolor(gs12)) xline(700.5 701 702 703 704 704.54, lwidth(vthick) lcolor(gs12)) ///
	legend(lab(1 "Control") lab(2 "Pro-social") lab(3 "Self-interest") row(1) region(col(white))) graphregion(color(white) lwidth(large)) ///
	plotregion(color(white)) ytitle("Installation size [kW]") xtitle("Months in the sample") //xlabel(-80(10)20,nogrid) xline(0 6, lcolor(black)) xline(1.7 2.5 3.5 4.3, lwidth(vvvthick) lcolor(gs12)) ///
	graph export Over_time_size_si.eps, replace
	graph export Over_time_size_si.png, replace
end
************************************************************************************************************************************************************************************************************************************************

************************************************************************************************************************
//FIGURE 3
************************************************************************************************************************
program define main_figure
//Panel (a)
use SI_attracts_sunlight_panel.dta, clear
gen average_cap = installed_capacity/installations
sort townid approved_month
replace group = 2 if Individual_town==1
replace group = 1 if Community_town==1
replace group = 0 if Control_town==1

drop if R8to10_towns==0
drop if contagion_individual==1 | contagion_community==1 | contagion_control==1 

keep if R8to10_individual==1 | R8to10_control==1 | R8to10_community==1
tab group, summarize(installations_1000ooh)
reg installations_1000ooh R8to10_individual R8to10_community
oneway installations_1000ooh group, tabulate // individual, community, control
pwmean installations_1000ooh, over(group) mcompare(tukey) effects

replace ami_level_indicator=0 if median_income < 1*ami_100
replace ami_level_indicator=1 if median_income >= 1*ami_100

	reg installations_1000ooh 1.hi#1.R8to10_individual 1.hi#1.R8to10_community 1.lmi#1.R8to10_individual 1.lmi#1.R8to10_community, cluster(townid)
	boottest 1.hi#1.R8to10_individual, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.hi#1.R8to10_community, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.R8to10_individual, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.R8to10_community, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.R8to10_community = 1.lmi#1.R8to10_individual , cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.hi#1.R8to10_individual = 1.lmi#1.R8to10_individual , cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.hi#1.R8to10_community = 1.lmi#1.R8to10_community , cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	
	reg installations_1000ooh R8to10_individual R8to10_community, cluster(townid)
	konfound R8to10_individual
	konfound R8to10_community
	
	reg installations_1000ooh 1.hi#1.R8to10_individual 1.hi#1.R8to10_community 1.lmi#1.R8to10_individual 1.lmi#1.R8to10_community, cluster(townid)
	reg installations_1000ooh R8to10_individual R8to10_community if hi==0, cluster(townid)
	reg installations_1000ooh R8to10_individual R8to10_community if hi==1, cluster(townid)
	konfound R8to10_individual
	konfound R8to10_community

	fvset base 50 townid
	egen mean_temp = mean(installations_1000ooh) if R8to10_control==1
	egen sd_temp = sd(installations_1000ooh) if R8to10_control==1
	egen mean_temp2 = max(mean_temp)
	egen sd_temp2 = max(sd_temp)
	egen number_temp = sum(R8to10_control) if ami_level_indicator==0
	egen number = max(number_temp)
	gen mean = mean_temp2 in 1
	gen ci_low = mean - invttail(number-1,0.025)*(sd_temp2/sqrt(number)) in 1
	gen ci_high = mean + invttail(number-1,0.025)*(sd_temp2/sqrt(number)) in 1
	reg installations_1000ooh R8to10_individual R8to10_community, cluster(townid)
	ereturn list
	matrix A = e(b)
	replace mean = mean[1] + A[1,2] in 2
	replace ci_low = mean[1] + A[1,2] - invttail(e(df_r),0.025)*_se[R8to10_community] in 2
	replace ci_high = mean[1] + A[1,2] + invttail(e(df_r),0.025)*_se[R8to10_community] in 2
	replace mean = mean[1] + A[1,1] in 3
	replace ci_low = mean[1] + A[1,1] - invttail(e(df_r),0.025)*_se[R8to10_individual] in 3
	replace ci_high = mean[1] + A[1,1] + invttail(e(df_r),0.025)*_se[R8to10_individual] in 3
	drop mean_temp
	drop mean_temp2
	drop sd_temp
	drop sd_temp2
	drop number
	drop number_temp

	fvset base 50 townid
	egen mean_temp = mean(installations_1000ooh) if R8to10_control==1 & ami_level_indicator==0
	egen sd_temp = sd(installations_1000ooh) if R8to10_control==1 & ami_level_indicator==0
	egen mean_temp2 = max(mean_temp)
	egen sd_temp2 = max(sd_temp)
	egen number_temp = sum(R8to10_control) if ami_level_indicator==0
	egen number = max(number_temp)
	replace mean = mean_temp2 in 5
	replace ci_low = mean - invttail(number-1,0.025)*(sd_temp2/sqrt(number)) in 5
	replace ci_high = mean + invttail(number-1,0.025)*(sd_temp2/sqrt(number)) in 5
	reg installations_1000ooh R8to10_individual R8to10_community if ami_level_indicator==0, cluster(townid)
	ereturn list
	matrix A = e(b)
	test _b[R8to10_community]=_b[R8to10_individual]
	replace mean = mean[5] + A[1,2] in 6
	replace ci_low = mean[5] + A[1,2] - invttail(e(df_r),0.025)*_se[R8to10_community] in 6
	replace ci_high = mean[5] + A[1,2] + invttail(e(df_r),0.025)*_se[R8to10_community] in 6
	replace mean = mean[5] + A[1,1] in 7
	replace ci_low = mean[5] + A[1,1] - invttail(e(df_r),0.025)*_se[R8to10_individual] in 7
	replace ci_high = mean[5] + A[1,1] + invttail(e(df_r),0.025)*_se[R8to10_individual] in 7
	drop mean_temp
	drop mean_temp2
	drop sd_temp
	drop sd_temp2
	drop number
	drop number_temp

	egen mean_temp = mean(installations_1000ooh) if R8to10_control==1 & ami_level_indicator==1
	egen sd_temp = sd(installations_1000ooh) if R8to10_control==1 & ami_level_indicator==1
	egen mean_temp2 = max(mean_temp)
	egen sd_temp2 = max(sd_temp)
	egen number_temp = sum(R8to10_control) if ami_level_indicator==1
	egen number = max(number_temp)
	replace mean = mean_temp2 in 9
	di number
	replace ci_low = mean[9] - invttail(number-1,0.025)*(sd_temp2/sqrt(number)) in 9
	replace ci_high = mean[9] + invttail(number-1,0.025)*(sd_temp2/sqrt(number)) in 9
	reg installations_1000ooh R8to10_individual R8to10_community if ami_level_indicator==1, cluster(townid)
	ereturn list
	matrix A = e(b)
	test _b[R8to10_community]=_b[R8to10_individual]
	replace mean = mean[9] + A[1,2] in 10
	replace ci_low = mean[9] + A[1,2] - invttail(e(df_r),0.025)*_se[R8to10_community] in 10
	replace ci_high = mean[9] + A[1,2] + invttail(e(df_r),0.025)*_se[R8to10_community] in 10
	replace mean = mean[9] + A[1,1] in 11
	replace ci_low = mean[9] + A[1,1] - invttail(e(df_r),0.025)*_se[R8to10_individual] in 11
	replace ci_high = mean[9] + A[1,1] + invttail(e(df_r),0.025)*_se[R8to10_individual] in 11
	drop mean_temp
	drop mean_temp2
	drop sd_temp
	drop sd_temp2
	drop number
	drop number_temp
	
	gen group1 = 0 in 1
	replace group1 = 1 in 2
	replace group1 = 2 in 3
	replace group1 = 0 in 5
	replace group1 = 1 in 6
	replace group1 = 2 in 7
	replace group1 = 0 in 9
	replace group1 = 1 in 10
	replace group1 = 2 in 11
	
	gen t = 1 in 1
	replace t = 2 in 2
	replace t = 3 in 3
	replace t = 5 in 5
	replace t = 6 in 6
	replace t = 7 in 7
	replace t = 9 in 9
	replace t = 10 in 10
	replace t = 11 in 11
	
	twoway (bar mean t if group1==0) (bar mean t if group1==1) (bar mean t if group1==2) (rcap ci_high ci_low t), ///
    legend(row(1) order(1 "Control" 2 "Pro-social" 3 "Self-interest") region(col(white))) xlabel(2 "Aggregate" 6 "Lower income" 10 "Higher income", noticks) xtitle("                                            Municipality median income") ytitle("Installations per month per 1000 OOH") ///
	ylabel(0(.5)2.25) graphregion(color(white) lwidth(large)) bgcolor(white)
	graph export Barchart_ami_campaign_installations_1000ooh_total_means.eps, replace
	graph export Barchart_ami_campaign_installations_1000ooh_total_means.png, replace

************************************************
//Panel (b)
use SI_attracts_sunlight_panel.dta, clear
set more off
set matsize 10000
sort townid approved_month
replace group = 0 if Control_town==1
replace group = 1 if Community_town==1
replace group = 2 if Individual_town==1
drop if approved_month < tm(2010m1)
drop if R8to10_towns==0
drop if R8to10_individual==1 | R8to10_community==1 | R8to10_control==1 

keep if contagion_individual==1 | contagion_control==1 | contagion_community==1
tab group, summarize(installations_1000ooh)
reg installations_1000ooh contagion_individual contagion_community
oneway installations_1000ooh group, tabulate // individual, community, control
pwmean installations_1000ooh, over(group) mcompare(tukey) effects

replace ami_level_indicator=0 if median_income < 1*ami_100
replace ami_level_indicator=1 if median_income >= 1*ami_100

	reg installations_1000ooh 1.hi#1.contagion_individual 1.hi#1.contagion_community 1.lmi#1.contagion_individual 1.lmi#1.contagion_community, cluster(townid)
	boottest 1.hi#1.contagion_individual, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.hi#1.contagion_community, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.contagion_individual, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.contagion_community, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.hi#1.contagion_community=1.lmi#1.contagion_community, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.hi#1.contagion_individual=1.lmi#1.contagion_individual, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph

	fvset base 50 townid
	egen mean_temp = mean(installations_1000ooh) if contagion_control==1
	egen sd_temp = sd(installations_1000ooh) if contagion_control==1
	egen mean_temp2 = max(mean_temp)
	egen sd_temp2 = max(sd_temp)
	egen number_temp = sum(contagion_control) if ami_level_indicator==0
	egen number = max(number_temp)
	gen mean = mean_temp2 in 1
	gen ci_low = mean - invttail(number-1,0.025)*(sd_temp2/sqrt(number)) in 1
	gen ci_high = mean + invttail(number-1,0.025)*(sd_temp2/sqrt(number)) in 1
	reg installations_1000ooh contagion_individual contagion_community, cluster(townid)
	ereturn list
	matrix A = e(b)
	replace mean = mean[1] + A[1,2] in 2
	replace ci_low = mean[1] + A[1,2] - invttail(e(df_r),0.025)*_se[contagion_community] in 2
	replace ci_high = mean[1] + A[1,2] + invttail(e(df_r),0.025)*_se[contagion_community] in 2
	replace mean = mean[1] + A[1,1] in 3
	replace ci_low = mean[1] + A[1,1] - invttail(e(df_r),0.025)*_se[contagion_individual] in 3
	replace ci_high = mean[1] + A[1,1] + invttail(e(df_r),0.025)*_se[contagion_individual] in 3
	drop mean_temp
	drop mean_temp2
	drop sd_temp
	drop sd_temp2
	drop number
	drop number_temp

	fvset base 50 townid
	egen mean_temp = mean(installations_1000ooh) if contagion_control==1 & ami_level_indicator==0
	egen sd_temp = sd(installations_1000ooh) if contagion_control==1 & ami_level_indicator==0
	egen mean_temp2 = max(mean_temp)
	egen sd_temp2 = max(sd_temp)
	egen number_temp = sum(contagion_control) if ami_level_indicator==0
	egen number = max(number_temp)
	replace mean = mean_temp2 in 5
	replace ci_low = mean - invttail(number-1,0.025)*(sd_temp2/sqrt(number)) in 5
	replace ci_high = mean + invttail(number-1,0.025)*(sd_temp2/sqrt(number)) in 5
	reg installations_1000ooh contagion_individual contagion_community if ami_level_indicator==0, cluster(townid)
	ereturn list
	matrix A = e(b)
	replace mean = mean[5] + A[1,2] in 6
	replace ci_low = mean[5] + A[1,2] - invttail(e(df_r),0.025)*_se[contagion_community] in 6
	replace ci_high = mean[5] + A[1,2] + invttail(e(df_r),0.025)*_se[contagion_community] in 6
	replace mean = mean[5] + A[1,1] in 7
	replace ci_low = mean[5] + A[1,1] - invttail(e(df_r),0.025)*_se[contagion_individual] in 7
	replace ci_high = mean[5] + A[1,1] + invttail(e(df_r),0.025)*_se[contagion_individual] in 7
	drop mean_temp
	drop mean_temp2
	drop sd_temp
	drop sd_temp2
	drop number
	drop number_temp
	
	egen mean_temp = mean(installations_1000ooh) if contagion_control==1 & ami_level_indicator==1
	egen sd_temp = sd(installations_1000ooh) if contagion_control==1 & ami_level_indicator==1
	egen mean_temp2 = max(mean_temp)
	egen sd_temp2 = max(sd_temp)
	egen number_temp = sum(contagion_control) if ami_level_indicator==1
	egen number = max(number_temp)
	replace mean = mean_temp2 in 9
	di number
	replace ci_low = mean[9] - invttail(number-1,0.025)*(sd_temp2/sqrt(number)) in 9
	replace ci_high = mean[9] + invttail(number-1,0.025)*(sd_temp2/sqrt(number)) in 9
	reg installations_1000ooh contagion_individual contagion_community if ami_level_indicator==1, cluster(townid)
	test contagion_individual
	test contagion_individual=contagion_community
	ereturn list
	matrix A = e(b)
	replace mean = mean[9] + A[1,2] in 10
	replace ci_low = mean[9] + A[1,2] - invttail(e(df_r),0.025)*_se[contagion_community] in 10
	replace ci_high = mean[9] + A[1,2] + invttail(e(df_r),0.025)*_se[contagion_community] in 10
	replace mean = mean[9] + A[1,1] in 11
	replace ci_low = mean[9] + A[1,1] - invttail(e(df_r),0.025)*_se[contagion_individual] in 11
	replace ci_high = mean[9] + A[1,1] + invttail(e(df_r),0.025)*_se[contagion_individual] in 11
	drop mean_temp
	drop mean_temp2
	drop sd_temp
	drop sd_temp2
	drop number
	drop number_temp
	
	gen group1 = 0 in 1
	replace group1 = 1 in 2
	replace group1 = 2 in 3
	replace group1 = 0 in 5
	replace group1 = 1 in 6
	replace group1 = 2 in 7
	replace group1 = 0 in 9
	replace group1 = 1 in 10
	replace group1 = 2 in 11
	
	gen t = 1 in 1
	replace t = 2 in 2
	replace t = 3 in 3
	replace t = 5 in 5
	replace t = 6 in 6
	replace t = 7 in 7
	replace t = 9 in 9
	replace t = 10 in 10
	replace t = 11 in 11

	twoway (bar mean t if group1==0) (bar mean t if group1==1) (bar mean t if group1==2) (rcap ci_high ci_low t), ///
    legend(row(1) order(1 "Control" 2 "Pro-social" 3 "Self-interest") region(col(white))) xlabel(2 "Aggregate" 6 "Lower income" 10 "Higher income", noticks) xtitle("                                            Municipality median income") ytitle("Installations per month per 1000 OOH") ///
	ylabel(0(.5)2.25) graphregion(color(white) lwidth(large)) bgcolor(white)
	graph export Barchart_ami_contagion_installations_1000ooh_total_means.eps, replace
	graph export Barchart_ami_contagion_installations_1000ooh_total_means.png, replace

end
************************************************************************************************************************************************************************************************************************************************\
program define main_figure_fe
//Panel (a)
use SI_attracts_sunlight_panel.dta, clear
gen average_cap = installed_capacity/installations
sort townid approved_month
replace group = 2 if Individual_town==1
replace group = 1 if Community_town==1
replace group = 0 if Control_town==1
drop if R8to10_towns==0

drop if contagion_individual==1 | contagion_community==1 | contagion_control==1 

	gen pop_density = round(population_town/land_area)
	gen frac_republican = republican_registered/voters
	gen frac_democratic = democratic_registered/voters
	gen frac_detached_units = detached_units_town/housing_units_town
	gen frac_families = families/population_town
	gen frac_white = white/population_town
	gen frac_black = black/population_town
	
	gen solarize = 1 if R8to10_individual==1 |  R8to10_community==1
	replace solarize=0 if solarize==.
	
	forvalues indicator=6(4)10 { 
	gen black_`indicator' = 1 if frac_black*100>`indicator'
	replace black_`indicator'=0 if black_`indicator'==.
	reg installations_1000ooh R8to10_individual R8to10_community 1.black_`indicator' 1.black_`indicator'#1.R8to10_individual 1.black_`indicator'#1.R8to10_community if campaign==1, cluster(townid) // 
	boottest 1.black_`indicator'#1.R8to10_individual, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.black_`indicator'#1.R8to10_community, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.black_`indicator'#1.R8to10_individual=1.black_`indicator'#1.R8to10_community, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	reg installations_1000ooh R8to10_individual R8to10_community 1.black_`indicator'#1.solarize if campaign==1, cluster(townid) //
	boottest 1.black_`indicator'#1.solarize, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph	
	}

replace ami_level_indicator=0 if median_income < 1*ami_100
replace ami_level_indicator=1 if median_income >= 1*ami_100

	fvset base 50 townid
	egen mean_temp = mean(installations_1000ooh) if R8to10_control==1
	egen sd_temp = sd(installations_1000ooh) if R8to10_control==1
	egen mean_temp2 = max(mean_temp)
	egen sd_temp2 = max(sd_temp)
	egen number_temp = sum(R8to10_control) if ami_level_indicator==0
	egen number = max(number_temp)
	gen mean = mean_temp2 in 1
	gen ci_low = mean - invttail(number-1,0.025)*(sd_temp2/sqrt(number)) in 1
	gen ci_high = mean + invttail(number-1,0.025)*(sd_temp2/sqrt(number)) in 1
	xtreg installations_1000ooh R8to10_individual R8to10_community year#month, fe cluster(townid)
	ereturn list
	matrix A = e(b)
	replace mean = mean[1] + A[1,2] in 2
	replace ci_low = mean[1] + A[1,2] - invttail(e(df_r),0.025)*_se[R8to10_community] in 2
	replace ci_high = mean[1] + A[1,2] + invttail(e(df_r),0.025)*_se[R8to10_community] in 2
	replace mean = mean[1] + A[1,1] in 3
	replace ci_low = mean[1] + A[1,1] - invttail(e(df_r),0.025)*_se[R8to10_individual] in 3
	replace ci_high = mean[1] + A[1,1] + invttail(e(df_r),0.025)*_se[R8to10_individual] in 3
	drop mean_temp
	drop mean_temp2
	drop sd_temp
	drop sd_temp2
	drop number
	drop number_temp

	fvset base 50 townid
	egen mean_temp = mean(installations_1000ooh) if R8to10_control==1 & ami_level_indicator==0
	egen sd_temp = sd(installations_1000ooh) if R8to10_control==1 & ami_level_indicator==0
	egen mean_temp2 = max(mean_temp)
	egen sd_temp2 = max(sd_temp)
	egen number_temp = sum(R8to10_control) if ami_level_indicator==0
	egen number = max(number_temp)
	replace mean = mean_temp2 in 5
	replace ci_low = mean - invttail(number-1,0.025)*(sd_temp2/sqrt(number)) in 5
	replace ci_high = mean + invttail(number-1,0.025)*(sd_temp2/sqrt(number)) in 5
	xtreg installations_1000ooh R8to10_individual R8to10_community year#month if ami_level_indicator==0, fe cluster(townid)
	ereturn list
	matrix A = e(b)
	test _b[R8to10_community]=_b[R8to10_individual]
	replace mean = mean[5] + A[1,2] in 6
	replace ci_low = mean[5] + A[1,2] - invttail(e(df_r),0.025)*_se[R8to10_community] in 6
	replace ci_high = mean[5] + A[1,2] + invttail(e(df_r),0.025)*_se[R8to10_community] in 6
	replace mean = mean[5] + A[1,1] in 7
	replace ci_low = mean[5] + A[1,1] - invttail(e(df_r),0.025)*_se[R8to10_individual] in 7
	replace ci_high = mean[5] + A[1,1] + invttail(e(df_r),0.025)*_se[R8to10_individual] in 7
	drop mean_temp
	drop mean_temp2
	drop sd_temp
	drop sd_temp2
	drop number
	drop number_temp

	egen mean_temp = mean(installations_1000ooh) if R8to10_control==1 & ami_level_indicator==1
	egen sd_temp = sd(installations_1000ooh) if R8to10_control==1 & ami_level_indicator==1
	egen mean_temp2 = max(mean_temp)
	egen sd_temp2 = max(sd_temp)
	egen number_temp = sum(R8to10_control) if ami_level_indicator==1
	egen number = max(number_temp)
	replace mean = mean_temp2 in 9
	di number
	replace ci_low = mean[9] - invttail(number-1,0.025)*(sd_temp2/sqrt(number)) in 9
	replace ci_high = mean[9] + invttail(number-1,0.025)*(sd_temp2/sqrt(number)) in 9
	xtreg installations_1000ooh R8to10_individual R8to10_community year#month if ami_level_indicator==1, fe cluster(townid)
	ereturn list
	matrix A = e(b)
	test _b[R8to10_community]=_b[R8to10_individual]
	replace mean = mean[9] + A[1,2] in 10
	replace ci_low = mean[9] + A[1,2] - invttail(e(df_r),0.025)*_se[R8to10_community] in 10
	replace ci_high = mean[9] + A[1,2] + invttail(e(df_r),0.025)*_se[R8to10_community] in 10
	replace mean = mean[9] + A[1,1] in 11
	replace ci_low = mean[9] + A[1,1] - invttail(e(df_r),0.025)*_se[R8to10_individual] in 11
	replace ci_high = mean[9] + A[1,1] + invttail(e(df_r),0.025)*_se[R8to10_individual] in 11
	drop mean_temp
	drop mean_temp2
	drop sd_temp
	drop sd_temp2
	drop number
	drop number_temp
	
	gen group1 = 0 in 1
	replace group1 = 1 in 2
	replace group1 = 2 in 3
	replace group1 = 0 in 5
	replace group1 = 1 in 6
	replace group1 = 2 in 7
	replace group1 = 0 in 9
	replace group1 = 1 in 10
	replace group1 = 2 in 11
	
	gen t = 1 in 1
	replace t = 2 in 2
	replace t = 3 in 3
	replace t = 5 in 5
	replace t = 6 in 6
	replace t = 7 in 7
	replace t = 9 in 9
	replace t = 10 in 10
	replace t = 11 in 11
	
	twoway (bar mean t if group1==0) (bar mean t if group1==1) (bar mean t if group1==2) (rcap ci_high ci_low t), ///
    legend(row(1) order(1 "Control" 2 "Pro-social" 3 "Self-interest") region(col(white))) xlabel(2 "Aggregate" 6 "Lower income" 10 "Higher income", noticks) xtitle("                                            Municipality median income") ytitle("Installations per month per 1000 OOH") ///
	ylabel(0(.5)2.25) graphregion(color(white) lwidth(large)) bgcolor(white)
	graph export Barchart_ami_campaign_installations_1000ooh_total.eps, replace
	graph export Barchart_ami_campaign_installations_1000ooh_total.png, replace
************************************************
//Panel (b)
use SI_attracts_sunlight_panel.dta, clear
set more off
set matsize 10000
sort townid approved_month
replace group = 0 if Control_town==1
replace group = 1 if Community_town==1
replace group = 2 if Individual_town==1
drop if approved_month < tm(2010m1)
drop if R8to10_towns==0
drop if R8to10_individual==1 | R8to10_community==1 | R8to10_control==1

replace ami_level_indicator=0 if median_income < 1*ami_100
replace ami_level_indicator=1 if median_income >= 1*ami_100

	fvset base 50 townid
	egen mean_temp = mean(installations_1000ooh) if contagion_control==1
	egen sd_temp = sd(installations_1000ooh) if contagion_control==1
	egen mean_temp2 = max(mean_temp)
	egen sd_temp2 = max(sd_temp)
	egen number_temp = sum(contagion_control) if ami_level_indicator==0
	egen number = max(number_temp)
	gen mean = mean_temp2 in 1
	gen ci_low = mean - invttail(number-1,0.025)*(sd_temp2/sqrt(number)) in 1
	gen ci_high = mean + invttail(number-1,0.025)*(sd_temp2/sqrt(number)) in 1
	xtreg installations_1000ooh contagion_individual contagion_community year#month, fe cluster(townid)
	ereturn list
	matrix A = e(b)
	replace mean = mean[1] + A[1,2] in 2
	replace ci_low = mean[1] + A[1,2] - invttail(e(df_r),0.025)*_se[contagion_community] in 2
	replace ci_high = mean[1] + A[1,2] + invttail(e(df_r),0.025)*_se[contagion_community] in 2
	replace mean = mean[1] + A[1,1] in 3
	replace ci_low = mean[1] + A[1,1] - invttail(e(df_r),0.025)*_se[contagion_individual] in 3
	replace ci_high = mean[1] + A[1,1] + invttail(e(df_r),0.025)*_se[contagion_individual] in 3
	drop mean_temp
	drop mean_temp2
	drop sd_temp
	drop sd_temp2
	drop number
	drop number_temp

	fvset base 50 townid
	egen mean_temp = mean(installations_1000ooh) if contagion_control==1 & ami_level_indicator==0
	egen sd_temp = sd(installations_1000ooh) if contagion_control==1 & ami_level_indicator==0
	egen mean_temp2 = max(mean_temp)
	egen sd_temp2 = max(sd_temp)
	egen number_temp = sum(contagion_control) if ami_level_indicator==0
	egen number = max(number_temp)
	replace mean = mean_temp2 in 5
	replace ci_low = mean - invttail(number-1,0.025)*(sd_temp2/sqrt(number)) in 5
	replace ci_high = mean + invttail(number-1,0.025)*(sd_temp2/sqrt(number)) in 5
	xtreg installations_1000ooh contagion_individual contagion_community year#month if ami_level_indicator==0, fe cluster(townid)
	ereturn list
	matrix A = e(b)
	replace mean = mean[5] + A[1,2] in 6
	replace ci_low = mean[5] + A[1,2] - invttail(e(df_r),0.025)*_se[contagion_community] in 6
	replace ci_high = mean[5] + A[1,2] + invttail(e(df_r),0.025)*_se[contagion_community] in 6
	replace mean = mean[5] + A[1,1] in 7
	replace ci_low = mean[5] + A[1,1] - invttail(e(df_r),0.025)*_se[contagion_individual] in 7
	replace ci_high = mean[5] + A[1,1] + invttail(e(df_r),0.025)*_se[contagion_individual] in 7
	drop mean_temp
	drop mean_temp2
	drop sd_temp
	drop sd_temp2
	drop number
	drop number_temp
	
	egen mean_temp = mean(installations_1000ooh) if contagion_control==1 & ami_level_indicator==1
	egen sd_temp = sd(installations_1000ooh) if contagion_control==1 & ami_level_indicator==1
	egen mean_temp2 = max(mean_temp)
	egen sd_temp2 = max(sd_temp)
	egen number_temp = sum(contagion_control) if ami_level_indicator==1
	egen number = max(number_temp)
	replace mean = mean_temp2 in 9
	di number
	replace ci_low = mean[9] - invttail(number-1,0.025)*(sd_temp2/sqrt(number)) in 9
	replace ci_high = mean[9] + invttail(number-1,0.025)*(sd_temp2/sqrt(number)) in 9
	xtreg installations_1000ooh contagion_individual contagion_community year#month if ami_level_indicator==1, fe cluster(townid)
	test contagion_individual
	test contagion_individual=contagion_community
	ereturn list
	matrix A = e(b)
	replace mean = mean[9] + A[1,2] in 10
	replace ci_low = mean[9] + A[1,2] - invttail(e(df_r),0.025)*_se[contagion_community] in 10
	replace ci_high = mean[9] + A[1,2] + invttail(e(df_r),0.025)*_se[contagion_community] in 10
	replace mean = mean[9] + A[1,1] in 11
	replace ci_low = mean[9] + A[1,1] - invttail(e(df_r),0.025)*_se[contagion_individual] in 11
	replace ci_high = mean[9] + A[1,1] + invttail(e(df_r),0.025)*_se[contagion_individual] in 11
	drop mean_temp
	drop mean_temp2
	drop sd_temp
	drop sd_temp2
	drop number
	drop number_temp
	
	gen group1 = 0 in 1
	replace group1 = 1 in 2
	replace group1 = 2 in 3
	replace group1 = 0 in 5
	replace group1 = 1 in 6
	replace group1 = 2 in 7
	replace group1 = 0 in 9
	replace group1 = 1 in 10
	replace group1 = 2 in 11
	
	gen t = 1 in 1
	replace t = 2 in 2
	replace t = 3 in 3
	replace t = 5 in 5
	replace t = 6 in 6
	replace t = 7 in 7
	replace t = 9 in 9
	replace t = 10 in 10
	replace t = 11 in 11

	twoway (bar mean t if group1==0) (bar mean t if group1==1) (bar mean t if group1==2) (rcap ci_high ci_low t), ///
    legend(row(1) order(1 "Control" 2 "Pro-social" 3 "Self-interest") region(col(white))) xlabel(2 "Aggregate" 6 "Lower income" 10 "Higher income", noticks) xtitle("                                            Municipality median income") ytitle("Installations per month per 1000 OOH") ///
	ylabel(0(.5)2.25) graphregion(color(white) lwidth(large)) bgcolor(white)
	graph export Barchart_ami_contagion_installations_1000ooh_total.eps, replace
	graph export Barchart_ami_contagion_installations_1000ooh_total.png, replace

end
************************************************************************************************************************************************************************************************************************************************

program define NPV_baseline
use SI_attracts_sunlight_town_npv_sunroof.dta, clear
ttest EV, by(ami_level_indicator)
reg EV ami_level_indicator
end

************************************************************************************************************************************************************************************************************************************************

program tables_main_effect_income
	************************
	//Table A.4, MEANS DURING: columns (1)(3)(5)
	************************
	use SI_attracts_sunlight_panel.dta, clear
	drop if approved_month < tm(2010m1)
	drop if R8to10_towns==0
	drop if contagion_individual==1 | contagion_community==1 | contagion_control==1 
	keep if R8to10_individual==1 | R8to10_control==1 | R8to10_community==1

	reg installations_1000ooh R8to10_individual R8to10_community, cluster(townid)
	test R8to10_community=R8to10_individual
	gen t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	test R8to10_individual=0
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	test R8to10_community=0
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	konfound R8to10_individual
	konfound R8to10_community
	boottest R8to10_community=R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_community, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	
	reg installations_1000ooh R8to10_individual if R8to10_individual==1 | R8to10_community==1, cluster(townid)	
	konfound R8to10_individual
	reg installations_1000ooh R8to10_individual if (R8to10_individual==1 | R8to10_community==1 ) & ami_level_indicator==1, cluster(townid)	
	konfound R8to10_individual
	
	reg installations_1000ooh R8to10_individual R8to10_community if ami_level_indicator==0, cluster(townid)
	test R8to10_community=R8to10_individual
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	boottest R8to10_community=R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_community, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	reg installations_1000ooh R8to10_individual R8to10_community if ami_level_indicator==1, cluster(townid)
	test R8to10_community=R8to10_individual
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	boottest R8to10_community=R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_community, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
			
	************************
	//Table S6, MEANS DURING: column (1)
	************************
	reg installations_1000ooh 1.hi#1.R8to10_individual 1.hi#1.R8to10_community 1.lmi#1.R8to10_individual 1.lmi#1.R8to10_community if campaign==1, cluster(townid)
	test 1.lmi#1.R8to10_community = 1.hi#1.R8to10_community
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	test 1.lmi#1.R8to10_individual = 1.hi#1.R8to10_individual
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	boottest 1.hi#1.R8to10_individual, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.hi#1.R8to10_community, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.R8to10_individual, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.R8to10_community, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.R8to10_community = 1.lmi#1.R8to10_individual , cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.R8to10_community = 1.hi#1.R8to10_community , cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.R8to10_individual = 1.hi#1.R8to10_individual , cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	
	************************
	//Table S7, MEANS DURING: column (1)
	************************
	reg installations_1000ooh R8to10_individual R8to10_community 1.hi#1.R8to10_individual 1.hi#1.R8to10_community if campaign==1, cluster(townid)
	boottest R8to10_individual, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest R8to10_community, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.hi#1.R8to10_individual, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.hi#1.R8to10_community, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph

	************************
	//Table A.4, FE DURING: columns (2)(4)(6)
	************************
	use SI_attracts_sunlight_panel.dta, clear
	drop if R8to10_towns==0
	drop if contagion_individual==1 | contagion_community==1 | contagion_control==1 
	
	xtreg installations_1000ooh R8to10_individual R8to10_community year#month, fe cluster(townid)
	test R8to10_community=R8to10_individual
	gen t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	test R8to10_individual=0
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	test R8to10_community=0
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	boottest R8to10_community=R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_community, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	
	xtreg installations_1000ooh R8to10_individual year#month if R8to10_individual==1 | R8to10_community==1, fe cluster(townid)	
	konfound R8to10_individual
	xtreg installations_1000ooh R8to10_individual year#month if (R8to10_individual==1 | R8to10_community==1 ) & ami_level_indicator==1, fe cluster(townid)	
	konfound R8to10_individual
	
	eststo m1: xtreg installations_1000ooh R8to10_individual R8to10_community year#month, fe cluster(townid)
	test R8to10_community=R8to10_individual
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	boottest R8to10_community=R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_community, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	eststo m2: xtreg installations_1000ooh R8to10_individual R8to10_community year#month if ami_level_indicator==0, fe cluster(townid)
	test R8to10_community=R8to10_individual
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	boottest R8to10_community=R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_community, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	eststo m3: xtreg installations_1000ooh R8to10_individual R8to10_community year#month if ami_level_indicator==1, fe cluster(townid)
	test R8to10_community=R8to10_individual
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	boottest R8to10_community=R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_community, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	esttab m1 m2 m3 using main_table_fullsample.tex, r2(%9.2f) b(%6.1g) replace keep(R8to10_individual R8to10_community) label se style(tex) nonotes
	
	************************
	//Table S6, FE DURING: column (2)
	************************
	xtreg installations_1000ooh 1.hi#1.R8to10_individual 1.hi#1.R8to10_community 1.lmi#1.R8to10_individual 1.lmi#1.R8to10_community year#month, fe cluster(townid)
	test 1.lmi#1.R8to10_community = 1.hi#1.R8to10_community
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	test 1.lmi#1.R8to10_individual = 1.hi#1.R8to10_individual
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	boottest 1.hi#1.R8to10_individual, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.hi#1.R8to10_community, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.R8to10_individual, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.R8to10_community, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.R8to10_community = 1.lmi#1.R8to10_individual , cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.R8to10_community = 1.hi#1.R8to10_community , cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.R8to10_individual = 1.hi#1.R8to10_individual , cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	
	************************
	//Table S7, FE DURING: column (2)
	************************
	xtreg installations_1000ooh R8to10_individual R8to10_community 1.hi#1.R8to10_individual 1.hi#1.R8to10_community year#month, fe cluster(townid)
	boottest R8to10_individual, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest R8to10_community, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.hi#1.R8to10_individual, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.hi#1.R8to10_community, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph	
	
	************************
	//Table A.5, MEANS AFTER: columns (1)(3)(5)
	************************
	use SI_attracts_sunlight_panel.dta, clear
	drop if approved_month < tm(2010m1)
	drop if R8to10_towns==0
	drop if R8to10_individual==1 | R8to10_community==1 | R8to10_control==1 
	keep if contagion_individual==1 | contagion_control==1 | contagion_community==1

	reg installations_1000ooh contagion_individual contagion_community, cluster(townid)
	test contagion_individual
	gen t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	test contagion_community
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	test contagion_community=contagion_individual
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	boottest contagion_community=contagion_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest contagion_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest contagion_community, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	reg installations_1000ooh contagion_individual contagion_community if ami_level_indicator==0, cluster(townid)
	test contagion_community=contagion_individual
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	boottest contagion_community=contagion_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest contagion_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest contagion_community, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	reg installations_1000ooh contagion_individual contagion_community if ami_level_indicator==1, cluster(townid)
	test contagion_community=contagion_individual
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	boottest contagion_community=contagion_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest contagion_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest contagion_community, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	
	************************
	//Table S6, MEANS DURING: column (3)
	************************
	reg installations_1000ooh 1.hi#1.contagion_individual 1.hi#1.contagion_community 1.lmi#1.contagion_individual 1.lmi#1.contagion_community, cluster(townid)
	boottest 1.hi#1.contagion_individual, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.hi#1.contagion_community, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.contagion_individual, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.contagion_community, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.contagion_community = 1.lmi#1.contagion_individual , cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.contagion_community = 1.hi#1.contagion_community , cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.contagion_individual = 1.hi#1.contagion_individual , cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	test 1.lmi#1.contagion_community = 1.hi#1.contagion_community
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	test 1.lmi#1.contagion_individual = 1.hi#1.contagion_individual
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	
	************************
	//Table S7, MEANS AFTER: column (3)
	************************	
	reg installations_1000ooh contagion_individual contagion_community 1.hi#1.contagion_individual 1.hi#1.contagion_community if contagion_individual==1 | contagion_control==1 | contagion_community==1, cluster(townid)
	boottest contagion_individual, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest contagion_community, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.hi#1.contagion_individual, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.hi#1.contagion_community, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph


	************************
	//Table S5, FE AFTER: columns (2)(4)(6)
	************************
	use SI_attracts_sunlight_panel.dta, clear
	drop if approved_month < tm(2010m1)
	drop if R8to10_towns==0 
	drop if R8to10_individual==1 | R8to10_community==1 | R8to10_control==1

	xtreg installations_1000ooh contagion_individual contagion_community year#month, fe cluster(townid)
	test contagion_community=contagion_individual
	gen t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	boottest contagion_community=contagion_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest contagion_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest contagion_community, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	xtreg installations_1000ooh contagion_individual contagion_community year#month if ami_level_indicator==0, fe cluster(townid) 
	test contagion_community=contagion_individual
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	boottest contagion_community=contagion_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest contagion_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest contagion_community, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	xtreg installations_1000ooh contagion_individual contagion_community year#month if ami_level_indicator==1, fe cluster(townid)
	test contagion_community=contagion_individual
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	boottest contagion_community=contagion_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest contagion_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest contagion_community, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	
	************************
	//Table S7, FE DURING: column (4)
	************************	
	xtreg installations_1000ooh 1.hi#1.contagion_individual 1.hi#1.contagion_community 1.lmi#1.contagion_individual 1.lmi#1.contagion_community year#month, fe cluster(townid)
	boottest 1.hi#1.contagion_individual, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.hi#1.contagion_community, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.contagion_individual, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.contagion_community, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.contagion_community = 1.lmi#1.contagion_individual , cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.contagion_community = 1.hi#1.contagion_community , cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.lmi#1.contagion_individual = 1.hi#1.contagion_individual , cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	test 1.lmi#1.contagion_community = 1.hi#1.contagion_community
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	test 1.lmi#1.contagion_individual = 1.hi#1.contagion_individual
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	
	************************
	//Table S7, FE AFTER: column (4)
	************************	
	xtreg installations_1000ooh contagion_individual contagion_community 1.hi#1.contagion_individual 1.hi#1.contagion_community year#month, fe cluster(townid)
	boottest contagion_individual, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest contagion_community, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.hi#1.contagion_individual, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
	boottest 1.hi#1.contagion_community, cluster(townid) bootcluster(townid) seed(990) reps(1000) nograph
end

************************************************************************************************************************************************************************************************************************************************

program define sunroof_panel
use SI_attracts_sunlight_sunroof_installations.dta, clear

//Table A.8
	reg kwstc R8to10_individual R8to10_community year#month i.townid i.contractorid, cluster(townid)
	boottest R8to10_community=R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_community, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	test R8to10_community=R8to10_individual
	gen t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	reg full_load_hours R8to10_individual R8to10_community year#month i.townid i.contractorid, cluster(townid)
	boottest R8to10_community=R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_community, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	test R8to10_community=R8to10_individual
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	reg npv_savings R8to10_individual R8to10_community year#month i.townid i.contractorid, cluster(townid)
	boottest R8to10_community=R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_community, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	test R8to10_community=R8to10_individual
	test R8to10_individual
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result

//Table A.9
	//LMI
	reg kwstc R8to10_individual R8to10_community year#month i.townid i.contractorid if ami_level_indicator==0, cluster(townid)
	boottest R8to10_community=R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_community, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	test R8to10_community=R8to10_individual
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	reg full_load_hours R8to10_individual R8to10_community year#month i.townid i.contractorid if ami_level_indicator==0, cluster(townid)
	boottest R8to10_community=R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_community, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	test R8to10_community=R8to10_individual
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	reg npv_savings R8to10_individual R8to10_community year#month i.townid i.contractorid if ami_level_indicator==0, cluster(townid)
	boottest R8to10_community=R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_community, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	test R8to10_community=R8to10_individual
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	test R8to10_individual
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	
	//HI
	reg kwstc R8to10_individual R8to10_community year#month i.townid i.contractorid if ami_level_indicator==1, cluster(townid)
	boottest R8to10_community=R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_community, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	test R8to10_community=R8to10_individual
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	reg full_load_hours R8to10_individual R8to10_community year#month i.townid i.contractorid if ami_level_indicator==1, cluster(townid)
	boottest R8to10_community=R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_community, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	test R8to10_community=R8to10_individual
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	reg npv_savings R8to10_individual R8to10_community year#month i.townid i.contractorid if ami_level_indicator==1, cluster(townid)
	boottest R8to10_community=R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_individual, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	boottest R8to10_community, cluster(townid) bootcluster(townid) seed(999) reps(1000) nograph
	test R8to10_community=R8to10_individual
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result
	test R8to10_individual
	replace t_test_result = round(sqrt(r(F)),0.01)
	di t_test_result

end
************************************************************************************************************************************************************************************************************************************************
program define survey
use SI_attracts_sunlight_survey.dta, replace

gen income_number = 25000 if income_hh==1
replace income_number = 62500 if income_hh==2
replace income_number = 87500 if income_hh==3
replace income_number = 122500 if income_hh==4
replace income_number = 137500 if income_hh==5
replace income_number = 175000 if income_hh==6
replace income_number = 225000 if income_hh==7

//Respondent characteristics
label variable norm_household_size "In families with 3+ people" //Please indicate the number of people in your home: 1 "1-2" 2 "3-4" 3 "5 or more"
label variable republican "Registered as republican" //
label variable democrat "Registered as democrat" //
label variable expectation_solar_prices_share "Expecting solar prices to go up" // do you expect solar installation prices in the next five years to
label variable expectation_elec_prices_share "Expecting elec. prices to go up" // do you expect electricity prices in the next five years to
label variable hybrid_vehicle "Hybrid vehicle" //
label variable plug_in_ev "Electric vehicle" //
label variable republican "Registered as republican" //
label variable democrat "Registered as democrat" //
//Solar installed
label variable si_reason_1_share "Money 1th reason to install" // What was the single most important reason for the decision to install solar? 
label variable si_reason_2_share "Money 1th and 2nd reason to install" // What was the second most important reason for the decision to install solar? 
label variable si_satisfied "Satisfied with installation" //Overall, I am satisfied with my installation
label variable si_recommended "Have recommended solar to friends" //I have recommended installing solar panels to my friends"
label variable si_would_recommend "Would recommend solar to friends" // I would recommend installing solar panels to my friends
label variable community_feeling "Measure of community feeling [0-1]"
label variable income_hh "Income group"
gen higher_education = 1 if degree>3 //"Some high school" 2 "High school diploma or equivalent" 3 "Some college" 4 "Undergraduate degree" 5 "Graduate degree"
replace higher_education = 0 if degree<=3
label variable higher_education "Share with undergraduate degree"
gen income_share = 1 if income_hh >= 4
replace income_share = 0 if income_hh < 4
label variable income_share "Share with income above 100,000"

//belief: Please rate how much you belief that installing solar panels... - ...is what people expect you to do
label variable belief_1 "Is what people expect you to do" //
label variable belief_2 "Will make people think you are a good person" //
label variable belief_4 "Is going to save you money in the long run" //
label variable belief_5 "Is going to lower your electricity bills" //
label variable belief_6 "Is a good long-term investment for you" //
label variable belief_7 "Will reduce your costs" //
label variable belief_8 "Will help protect the environment" //
label variable belief_9 "Is going to help future generations" //
label variable belief_10 "Will contribute to the public good" //
label variable belief_11 "Is going to make the planet healthier" //
label variable belief_12 "Will increase your property values" //
label variable belief_13 "Will reduce your community's reliance on the grid" //
label variable belief_14 "Is a good investment in your community" //
label variable belief_15 "Reduces your reliance on the grid" //
label variable belief_16 "Gives you more control over my energy costs" //

gen beliefs_individual = (belief_1+belief_2+belief_4+belief_5+belief_6+belief_7+belief_12+belief_15+belief_16)/9
gen beliefs_community = (belief_8+belief_9+belief_10+belief_11+belief_13+belief_14)/6

label variable beliefs_individual "Average individual belief" //
label variable beliefs_community "Average community belief" //

format community_feeling income_hh norm_household_size local_involvement expectation_elec_prices expectation_solar_prices si_recommended si_satisfied si_would_recommend ///
expectation_elec_prices_share si_reason_1_share si_reason_2_share si_satisfied si_recommended si_would_recommend hybrid_vehicle plug_in_ev republican democrat expectation_solar_prices_share ///
community_feeling higher_education income_share ///
belief_1 belief_2 belief_4 belief_5 belief_6 belief_7 belief_8 belief_9 belief_10 belief_11 belief_12 belief_13 belief_14 belief_15 belief_16 beliefs_individual beliefs_community %10.2fc
local varlist si_recommended si_satisfied si_would_recommend
foreach var of local varlist {
preserve
drop if group>1
ttest `var', by(group)
sort group `var'
drop in 1/15
ttest `var', by(group)
restore
}

************************************************************************************************************************
//Table 3: Individual vs community
************************************************************************************************************************
preserve
keep if group == 0 | group == 1
keep if have_solar == 1

ttest si_reason_1_share, by(group)
ttest si_reason_2_share, by(group)
ttest norm_household_size, by(group)
ttest plug_in_ev, by(group)
ttest si_would_recommend, by(group)
ttest si_satisfied, by(group)
ttest si_recommended, by(group)

balancetable group si_reason_1_share si_reason_2_share ///
norm_household_size income_share hybrid_vehicle plug_in_ev expectation_solar_prices_share expectation_elec_prices_share democrat republican higher_education local_involvement community_feeling ///
si_would_recommend si_satisfied si_recommended ///
using ToB_survey_summary.tex, vce(robust) wide(mean pval) pvalues staraux replace booktabs displayformat varlabels nonumbers pval(fmt(%5.3f) nopar) ///
postfoot("\hline" "\multicolumn{4}{l}{The p-values are for a pairwise two-sided t-test of differences in means by group.} \end{tabular}") ///
groups("Individual" "Community" "Difference", pattern(1 1 1 1))
restore
 
balancetable have_solar beliefs_individual belief_1 belief_2 belief_4 belief_5 belief_6 belief_7 belief_12 belief_15 belief_16 ///
beliefs_community belief_8 belief_9 belief_10 belief_11 belief_13 belief_14 ///
norm_household_size income_share hybrid_vehicle plug_in_ev expectation_solar_prices_share expectation_elec_prices_share democrat republican higher_education local_involvement community_feeling ///
using Survey_summary_adopters_nonadopters.tex, vce(robust) wide(mean pval) pvalues staraux replace booktabs displayformat  varlabels nonumbers pval(fmt(%5.3f) nopar) ///
postfoot("\hline" "\multicolumn{4}{l}{The p-values are for a pairwise two-sided t-test of differences in means by group.} \end{tabular}") ///
groups("Non-adopters" "Adopters" "Difference", pattern(1 1 1 1))  
 
end

************************************************************************************************************************************************************************************************************************************************
program define financing
use SI_attracts_sunlight_sunroof_installations.dta, clear
sort townid
	
keep if R8to10_individual==1 | R8to10_community==1

egen var1 = group(systemfinancingtype) 
tab systemfinancingtype
gen lease_buy = 0 if var1==1 | var1==3 | var1==4
replace lease_buy=1 if var1==2 
label define financing_label 0 "Loan/Lease" 1 "Purchase"
label values lease_buy financing_label

gen panel_usa = 1 if panel1mfrcountry=="USA"
replace panel_usa=0 if panel1mfrcountry!="USA"
gen inverter_usa = 1 if inverter1mfrcountry=="USA"
replace inverter_usa=0 if inverter1mfrcountry!="USA"

destring ppakwh, replace

replace ppakwh=. if ppakwh==0
format ppakwh %10.2fc

label variable panel_usa "Share with modules made in USA"
label variable inverter_usa "Share with inverters made in USA"
label variable lease_buy "Share that buys its installation"
label variable ppaleaseterm "Length of PPA [years]"
label variable ppakwh "PPA [dollar/kWh]"

format panel_usa lease_buy ppaleaseterm kwstc inverter_usa %10.2fc

balancetable group lease_buy ppakwh ppaleaseterm panel_usa inverter_usa ///
using ToB_matched_system.tex, vce(robust) wide(mean pval) pvalues staraux replace booktabs displayformat varlabels nonumbers pval(fmt(%5.2f) nopar) ///
groups("Self-interest" "Pro-social" "Difference", pattern(1 1 1 1)) 

ttest panel_usa, by(group)
reg panel_usa group
ttest lease_buy, by(group)
reg lease_buy group
end
 ************************************************************************************************************************************************************************************************************************************************
run_all
