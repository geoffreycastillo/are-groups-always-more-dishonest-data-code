clear
capture cd "/Users/lawrencechoo/Dropbox/Projects/Group Dishonesty/Stata/Lawrence/JEBO" //<---Insert your file path here!!!!!
graph set window fontface "Times New Roman"
set graphics off

capture program drop mannwhitneyu 
program mannwhitneyu
	quietly {
		local n1n2 = r(N_1)*r(N_2)
		local p1 = r(porder)
		local p2 = 1-`p1'
		local U = min(`p1',`p2')*`n1n2'
	}
	display _newline ///
		as text "Mann-Whitney U = " as result `U'
end


// ---------------------------------------------------------
// ---------------------------------------------------------
// ---------------------------------------------------------
// ---------------------------------------------------------
// ---------------------------------------------------------
// Clean up the chat data and merge information with the die-roll data
// ---------------------------------------------------------
// ---------------------------------------------------------
// ---------------------------------------------------------
// ---------------------------------------------------------
// ---------------------------------------------------------
// ---------------------------------------------------------

use chat_data.dta

//Step 1. Drop observations from G-Dictator where a group member dropped whilst online
drop if drop_in_session==1
drop drop_in_session
//Step 2. Drop observations from all treatments where subjects did not report the same number 
drop if samereport==0 
drop samereport
//Step 3. Identify those comments that are coded by at least one RA
gen code1 = 0
replace code1 = 1 if RA1_Code!=. | RA2_Code!=.
label define code1 0 "" 1 "Coded by at least one RA" 
label  values code1 code1
//Step 4. Identify those comments that are coded by both RAs
gen code2 = 0
replace code2 = 1 if RA1_Code!=. & RA2_Code!=.
label define code2 0 "" 1 "Coded by both RAs" 
label  values code2 code2
//Step 5. Identify those comments that are coded differently by the RAs
gen code_conflict = 0
replace code_conflict = 1 if code2==1 & RA1_Code!=RA2_Code
label define code_conflict 0 "" 1 "Conflict in coding" 
label  values code_conflict code_conflict
//Step 6. Identify those comments that are catergorise by at least one RA
gen cat1 = 0
replace cat1 = 1 if RA1_Cat!=. | RA2_Cat!=.
label define cat1 0 "" 1 "Categorised by at least one RA" 
label  values cat1 cat1
//Step 7. Identify those comments that are catergorise by both RAs
gen cat2 = 0
replace cat2 = 1 if RA1_Cat!=. & RA2_Cat!=.
label define cat2 0 "" 1 "Categorised by both RAs" 
label  values cat2 cat2
//Step 8. Identify those comments that are categorised differently by the RAs
gen cat_conflict = 0
replace cat_conflict = 1 if cat2==1 & RA1_Cat!=RA2_Cat
label define cat_conflict 0 "" 1 "Conflict in categorised" 
label  values cat_conflict cat_conflict

drop if moral==4 //Drop the chat from the dictator game
drop if RA1_Code!=RA2_Code //Drop all chats which are coded differently by the two RAs.
drop if RA1_Code >=. //Drop all chats that are not coded by the two RA.
gen x = 0
replace x  = 1 if RA1_Code>see2
collapse (mean) chat_ratio=x , by(moral matchgroup)
label variable chat_ratio "Proportion of messages for over-reporting in the group"
format chat_ratio %9.2f

merge m:m matchgroup using data_choice.dta 
drop _merge 
gsort moral team matchgroup


//------------------------------------------------------------------
// Section A. Analysis of  Die-roll experiments
//------------------------------------------------------------------
drop if moral==4 //Drop data from the dictator game

// ---------------------------------------------------------
// A.0 Graphing the scatter plot (Figure 2)
// ---------------------------------------------------------

preserve
collapse (median) see1 report1 see2 report2 see3 report3, by(team moral matchgroup)
expand 2, gen(duplicate)
expand 2, gen(duplicate1)
drop if duplicate == 1 & duplicate1==1
drop if moral==4
gen part = 1 
replace part = 2 if duplicate==1 & duplicate1==0
replace part = 3 if duplicate==0 & duplicate1==1
gen report = report1 if part==1
replace report = report2 if part==2
replace report = report3 if part==3
gen see = see1 if part==1
replace see = see2 if part==2
replace see = see3 if part==3
drop see1 report1 see2 report2 see3 report3 duplicate duplicate1
twoway ///
(sc report see, by(moral team part, col(3) note("")) jitter(3)  msymbol(circle_hollow) ///
 xline(0.5, lwidth(thin) lcolor(gs14)) xline(1.5, lwidth(thin) lcolor(gs14)) xline(2.5, lwidth(thin) lcolor(gs14)) xline(3.5, lwidth(thin) lcolor(gs14)) xline(4.5, lwidth(thin) lcolor(gs14)) ///
 yline(0.5, lwidth(thin) lcolor(gs14)) yline(1.5, lwidth(thin) lcolor(gs14)) yline(2.5, lwidth(thin) lcolor(gs14)) yline(3.5, lwidth(thin) lcolor(gs14)) yline(4.5, lwidth(thin) lcolor(gs14))) ///
, xlabel(0(1)5) ylabel(0(1)5) subtitle("",nobox) ysize(8) xsize(5) ytitle(Points {it:Reported}) xtitle(Points {it:Observed})
graph play grec_Figure2
graph export "Figure2.pdf", replace
restore


// ---------------------------------------------------------
// A.1 Subjects' behaviour in part I (Over-reporting) (footnote 3)
// ---------------------------------------------------------

tab over1 team if moral==1, chi2 
tab over1 team if moral==2, chi2
tab over1 team if moral==3, chi2


// ---------------------------------------------------------
// A.2 Subjects' behaviour in part II (Observation and Result 1)
// ---------------------------------------------------------

//A.2.1 Comparison of Under-reporting, exact-reporting and over-reporting types (Ind vs. Grp)
gen type = 1 if rep==1 & samereport==1 
replace type =0 if rep==1 & report2<see2 & samereport==1
replace type = 2 if rep==1 & report2>see2 & samereport==1
label define type 0 "Under-Report" 1 "Exact-Report" 2 "Over-Report"
label values type type
// these tests shown on Figure 3
tab type team if moral==1, chi2
tab type team if moral==2, chi2
tab type team if moral==3, chi2
// these tests pertain to Result 1

//A.2.1.1 Plotting of types (Figure 3)

preserve
drop if moral==4
graph bar (count), over(type) over(team) by(moral) stack per asy blabel(bar, position(center) format(%9.1f) color(white))
graph play grec_Figure3a.grec
graph play grec_Figure3b.grec
graph export "Figure3.pdf", replace
restore 


//A.2.2 Comparison of over-reporting types (Ind vs. Grp)
gen dis = 0 if rep==1 & samereport==1
replace dis = 1 if rep==1 & type!=2 & samereport==1
tab dis team if moral==1, chi2 // observation
tab dis team if moral==2, chi2 // Result 1
tab dis team if moral==3, chi2 // Result 1

//A.2.2.0 Reported points 
ranksum report2 if moral==1 & rep==1 & samereport==1, by(team) porder // Base
mannwhitneyu
ranksum report2 if moral==2 & rep==1 & samereport==1, by(team) porder // Charity
mannwhitneyu
ranksum report2 if moral==3 & rep==1 & samereport==1, by(team) porder // CharityR
mannwhitneyu

//A.2.2.1 Econometric analysis (Appendix B.1.1, Table B1)
gen x2 =  (report2>see2) if samereport==1
logit x2 i.team if moral==1 & see2<5, vce(cluster matchgroup) //I-Base vs. G-Base
logit x2 i.team over1 male see2 if moral==1 & see2<5, vce(cluster matchgroup) //I-Base vs. G-Base
logit x2 i.team if moral==2 & see2<5, vce(cluster matchgroup) //I-Charity vs. G-Charity
logit x2 i.team over1 male see2 if moral==2 & see2<5, vce(cluster matchgroup) //I-Charity vs. G-Charity
logit x2 i.team if moral==3 & see2<5, vce(cluster matchgroup) //I-CharityR vs. G-CharityR
logit x2 i.team over1 male see2 if moral==3 & see2<5, vce(cluster matchgroup) //I-CharityR vs. G-CharityR

//A.2.2.2 Using a broader definition of dishonesty as one who is NOT a exact-reporting type
preserve 
gen dis2 = 0 if rep==1 & samereport==1
replace dis2 = 1 if rep==1 & type!=1 & samereport==1
tab dis2 team if moral==1, chi2
tab dis2 team if moral==2, chi2 
tab dis2 team if moral==3, chi2
restore




// ---------------------------------------------------------
// A.3 The effects of Charity on Individual and group behaviour in part II (Result 2)
// ---------------------------------------------------------

//A.3.1 Individual proportion of over-reporting
tab over2 moral if team==0 & moral!=3 & samereport==1, chi2 //I-Base vs. I-Charity
tab over2 moral if team==0 & moral!=2 & samereport==1, chi2 //I-Base vs. I-CharityR
tab over2 moral if team==0 & moral!=1 & samereport==1, chi2 //I-Charity vs. I-CharityR
logit x2 i.moral  if team==0 & see2<5 & samereport==1 // Appendix B.1.1, Table B2)
logit x2 i.moral see2 if team==0 & see2<5 & samereport==1 // Appendix B.1.1, Table B2)

//A.3.2 Group proportion of over-reporting
tab over2 moral if team==1 & moral!=3 & samereport==1, chi2 //I-Base vs. I-Charity
tab over2 moral if team==1 & moral!=2 & samereport==1, chi2 //I-Base vs. I-CharityR
tab over2 moral if team==1 & moral!=1 & samereport==1, chi2 //I-Charity vs. I-CharityR
logit x2 i.moral if team==1 & see2<5 & samereport==1 // Appendix B.1.1, Table B2)
logit x2 i.moral see2 if team==1 & see2<5 & samereport==1 // Appendix B.1.1, Table B2)

// ---------------------------------------------------------
// A.4  What the over-reporting individuals/groups report (Result 3)
// ---------------------------------------------------------

//A.4.0 What did over-reporting individuals and groups see1
ranksum see2 if moral==1 & over2==1 & samereport==1, by(team) porder //I-Base vs. G-Base
mannwhitneyu
ranksum see2 if moral==2 & over2==1 & samereport==1, by(team) porder //I-Charity vs. G-Charity
mannwhitneyu
ranksum see2 if moral==3 & over2==1 & samereport==1, by(team) porder //I-CharityR vs. G-CharityR
mannwhitneyu

//A.4.1 Mann-Whitney test
ranksum report2 if moral==1 & over2==1 & samereport==1, by(team) porder //I-Base vs. G-Base
mannwhitneyu
ranksum report2 if moral==2 & over2==1 & samereport==1, by(team) porder //I-Charity vs. G-Charity
mannwhitneyu
ranksum report2 if moral==3 & over2==1 & samereport==1, by(team) porder //I-CharityR vs. G-CharityR
mannwhitneyu

//A.4.2 Ordered Logit test (Appendix B.1.1, Table B3)
ologit report2 i.team  if moral==1 & over2==1 & see2<5 & samereport==1 //I-Base vs. G-Base
ologit report2 i.team see2 if moral==1 & over2==1 & see2<5 & samereport==1 //I-Base vs. G-Base
ologit report2 i.team  if moral==2 & over2==1 & see2<5 & samereport==1 //I-Charity vs. G-Charity
ologit report2 i.team see2 if moral==2 & over2==1 & see2<5 & samereport==1 //I-Charity vs. G-Charity
ologit report2 i.team  if moral==3 & over2==1 & see2<5 & samereport==1 //I-CharityR vs. G-CharityR
ologit report2 i.team see2 if moral==3 & over2==1 & see2<5 & samereport==1 //I-CharityR vs. G-CharityR

//A.4.2.1 Plotting of over-reporting types reported numbers (Figure 4)

preserve
keep if rep==1 & samereport==1 & over2==1 // Focus on individuals or groups that have over-reported
gen x = 1
collapse (sum) x, by(moral team report2)
by moral team: egen total = sum(x)
gen freq = x / total
format freq %9.2f
twoway ///
(bar freq report2 if team==0, by(moral)) ///
(bar freq report2 if team==1, by(moral)) ///
, xlabel(0(1)5) ylabel(0(0.2)1)
graph play grec_Figure4.grec
graph export "Figure4.pdf", replace
restore 

//A.4.2.2: end of section 3.1 
ranksum report2 if team==0 & over2==1 & samereport==1 & moral!=3, by(moral) porder //I-Base vs. I-Charity
mannwhitneyu
ranksum report2 if team==1 & over2==1 & samereport==1 & moral!=3, by(moral) porder //G-Base vs. G-Charity
mannwhitneyu

ranksum report2 if team==0 & over2==1 & samereport==1 & moral!=1, by(moral) porder //I-CharityR vs. I-Charity
mannwhitneyu
ranksum report2 if team==1 & over2==1 & samereport==1 & moral!=1, by(moral) porder //G-CharityR vs. G-Charity
mannwhitneyu

// ---------------------------------------------------------
// A.5 Spillovers of dishonesty (Result 5 and 6)
// ---------------------------------------------------------

//A.5.1 Do dishonest individuals make groups more dishonest? (Result 5)
gsort matchgroup 
by matchgroup: egen total_over1 = sum(over1) if samereport==1 & team==1 & moral!=4
label variable total_over1 "Total number of group members who over-reported in part I"
kwallis total_over1 if team==1 & rep==1, by(moral) // Comparisons of total_over1 across the different treatments

spearman over2 total_over1 if moral==1 // Correlation of total number of over-reporting group members with group decision in part II (Base)
logit over2 see2 total_over1 if moral==1  & see2<5 // Appendix B.1.2 Table B4
spearman over2 total_over1 if moral==2 // Correlation of total number of over-reporting group members with group decision in part II (Charity)
logit over2 see2 total_over1 if moral==2  & see2<5 // Appendix B.1.2 Table B4
spearman over2 total_over1 if moral==3 // Correlation of total number of over-reporting group members with group decision in part II (CharityR)
logit over2 see2 total_over1 if moral==3  & see2<5 // Appendix B.1.2 Table B4 


//A.5.2 Do dishonest groups make individuals more dishonest? (Result 6)
gen exp = 1
replace exp = 2 if team==1 & x2==0
replace exp = 3 if team==1 & x2==1
label define exp 1 "IND" 2 "GRP-notOR" 3 "GRP-OR"
label values exp exp
label variable exp "Subjects' group experience in Part II'"

//A.5.2.1 Checking for multicollinearity (footnote 7)
reg over3 i.exp over1 see3 if see3<5 & moral==1 & samereport==1 & team==1, vce(cluster matchgroup) 
vif
reg over3 i.exp over1 see3 if see3<5 & moral==2 & samereport==1 & team==1, vce(cluster matchgroup) 
vif
reg over3 i.exp over1 see3 if see3<5 & moral==3 & samereport==1 & team==1, vce(cluster matchgroup) 
vif
reg over3 i.exp over1 see3 if see3<5 & moral>1 & samereport==1 & team==1, vce(cluster matchgroup) 
vif


//A.5.2.2 Running the regressions for the estimates in the paper (Manuscript Table 3)
logit over3 i.exp if see3<5 & moral==1 & samereport==1, vce(cluster matchgroup) //The Base treatments, Table 3 Model (1)
test 2.exp=3.exp
logit over3 i.exp see3 over1 if see3<5 & moral==1 & samereport==1, vce(cluster matchgroup) //The Base treatments, Table 3 Model (2)
test 2.exp=3.exp
logit over3 i.exp if see3<5 & moral>1 & samereport==1, vce(cluster matchgroup) // The charity treatments, Table 3 Model (3)
test 2.exp=3.exp
logit over3 i.exp see3 over1 if see3<5 & moral>1 & samereport==1, vce(cluster matchgroup) // The charity treatments, Table 3 Model (4)
test 2.exp=3.exp

//A.5.2.2.0 Plotting of over-reporting in Part III by different subgroups (Figure 5)
preserve
gen charity = 0 //Binary variable for charity treatments
replace charity = 1 if moral > 1
collapse (mean) x=over3, by(charity exp)
format x %9.2f
twoway ///
(bar x exp if exp==1, by(charity)) ///
(bar x exp if exp==2, by(charity)) ///
(bar x exp if exp==3, by(charity)) ///
(sc x exp, by(charity) mlabel(x)) ///
, xlabe(1(1)3) ylabel(0.4(0.2)0.7)
graph play grec_Figure6
graph export "Figure6.pdf", replace
restore 


// ---------------------------------------------------------
// A.6 Analysis of the Chat data (Result 4)
// ---------------------------------------------------------

//A.6.1 Correlation between chat and over-reporting in part II
spearman over2 chat_ratio if moral==1 & team==1 & rep==1 // Base 
spearman over2 chat_ratio if moral==2  & team==1 & rep==1  // Charity
spearman over2 chat_ratio if moral==3  & team==1 & rep==1  // CharityR

//A.6.2 Between treatment comparison of chat in part II 
ranksum chat_ratio if moral!=3 & team==1 & rep==1, by(moral) porder //Base vs. Charity
mannwhitneyu
ranksum chat_ratio if moral!=2 & team==1 & rep==1, by(moral) porder //Base vs. CharityR
mannwhitneyu
ranksum chat_ratio if moral!=1 & team==1 & rep==1, by(moral) porder //Charity vs. CharityR
mannwhitneyu

fracreg  logit chat_ratio i.moral if see2<5 & team==1 & rep==1 // Table 2, model (1)
test 2.moral = 3.moral

fracreg  logit chat_ratio i.moral see2 total_over1  if see2<5 & team==1 & rep==1 // Table 2, model (2)
test 2.moral = 3.moral

//A.6.2.0 Plotting of proportion of chat statements for over-reporting. (Figure 5)
preserve
keep if team==1 & rep==1 & samereport==1
collapse (mean) mean = chat_ratio (sd) sd = chat_ratio (count) n = chat_ratio,  by(moral)
generate hi = mean + invttail(n-1,0.025)*(sd / sqrt(n))
generate low = mean - invttail(n-1,0.025)*(sd / sqrt(n))
format mean %9.2f

twoway ///
(bar mean moral) ///
(rcap hi low moral), ///
xlabel(1 "Base" 2 "Charity" 3 "CharityR") ylabel(0(0.2)0.8)
graph play grec_Figure5
graph export "Figure5.pdf", replace
restore 

// ---------------------------------------------------------
// ---------------------------------------------------------
// ---------------------------------------------------------
// ---------------------------------------------------------
// ---------------------------------------------------------
// B. Analysis of the Dictator game decisions (Result 7)
// ---------------------------------------------------------
// ---------------------------------------------------------
// ---------------------------------------------------------
// ---------------------------------------------------------
// ---------------------------------------------------------
// ---------------------------------------------------------
clear
use data_choice.dta
drop if moral!=4

//B.1 Comparisons of reported boths in parts I,II and III 
ranksum report1, by(team) porder //Part I individuals vs. groups
mannwhitneyu
ranksum report2 if rep==1, by(team) porder //Part II individuals vs. groups
mannwhitneyu
ranksum report3, by(team) porder //Part III individuals vs. groups
mannwhitneyu

//B.2 Classify individuals and groups into types.
egen neutral_number = mean(report1)
gen type = 0 if rep==1
replace type = 1 if report2>neutral_number & rep==1
label define type 0 "pro-social" 1 "Selfish"
label values type type
tab type team, chi2 column

preserve 
expand 2, gen(n1)
expand 2, gen(n2)
egen part = group(n1 n2 )
drop if part > 3
drop n1 n2
gen report = report1 if part ==1 
replace report = report2 if part == 2 & rep==1
replace report = report3 if part == 3
drop if report>=.
gen x = 1
gsort part team
by part team: egen total = sum(x)
collapse (sum) x (mean) total, by(part team report)
gen freq = x / total
format freq %9.2f
twoway ///
(bar freq report if team==0, by(part)) ///
(bar freq report if team==1, by(part)) ///
, xlabe(0(1)5) ylabel(0.0(0.1)0.4)
graph play grec_Figure7
graph export "Figure7.pdf", replace
restore 


//B.3 Comparisons of type.
tab type team, chi2
ranksum report2 if type==1 & rep==1, by(team) porder
mannwhitneyu


// ---------------------------------------------------------
// ---------------------------------------------------------
// ---------------------------------------------------------
// ---------------------------------------------------------
// ---------------------------------------------------------
// C. Post-study probability estimates (Sectiion 5)
// ---------------------------------------------------------
// ---------------------------------------------------------
// ---------------------------------------------------------
// ---------------------------------------------------------
// ---------------------------------------------------------
// ---------------------------------------------------------
preserve
clear 
set obs  101
gen pi = 0
format pi %9.2f
gen first = 1 if pi[_n-1]>=.
replace pi = pi[_n-1]+0.01 if first!=1
gen alpha = 0.05
expand 2 , gen(n1)
expand 2 , gen(n2)
expand 2 , gen(n3)
expand 2 , gen(n4)

egen y = group(n1 n2 n3 n4)
drop if pi>1

gen k = 1 if y==1
replace k =2 if y==2
replace k = 5 if y==3
replace k = 10 if y==4
replace k = 15 if y==5
replace k = 20 if y==6
replace k = 30 if y==7
replace k = 40 if y==8
replace k = 50 if y==9
drop if y>9
drop n1 n2 n3 n4 y first

expand 2, gen(n1)
expand 2, gen(n2)
egen y = group(n1 n2)
gen beta = 0.2 if y==1
replace beta = 0.4 if y==2
replace beta = 0.6 if y==3
drop if y>3
drop n1 n2 

gen num = (1-beta^k)*pi 
gen den = (1-beta^k)*pi  + (1 - (1-alpha)^k)*(1-pi)
gen PSP = num/den
drop num den
format PSP %9.2f

drop if k>5

twoway ///
(connected PSP pi if y==1,by(k)) ///
(connected PSP pi if y==2,by(k)) ///
(connected PSP pi if y==3,by(k)) ///
,xlabel(0(0.2)1) legend(order(1 "Power = 0.8" 2 "Power = 0.6" 3 "Power = 0.4")) ylabel(0(0.2)1)
graph play grec_Figure8
graph export "Figure8.pdf", replace
restore

set graphics on


