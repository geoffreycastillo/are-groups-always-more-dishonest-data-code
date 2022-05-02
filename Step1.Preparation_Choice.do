clear
capture cd "/Users/lawrencechoo/Dropbox/Projects/Group Dishonesty/Stata/Lawrence/" //<---Insert your file path here!!!!!

// -----------------------------------------------------
// A. Load Die_roll data
// -----------------------------------------------------
import excel using raw.xlsx, firstrow sheet("raw_die")


//A.1 Drop unused variables 
drop  xx FinalProfit ShowUpFee ShowUpFeeInvested MoneyAdded MoneyToPay MoneyEarned  profit1r diff1_0 diff1_1 diff1_2 diff1_3 diff1_4 diff1_5 profit1g sure1  dur1start guess1_0 guess1_1 guess1_2 guess1_3 guess1_4 guess1_5 dur1roll dur1erklaer dur1schaetz dur1erklaerschaetz dur1sicher profit1r_charity profit2r sameN dur2start dur2chat dur2wantleave dur2roll leave leave_counter profit2r_charity profit3r guess3_0 guess3_1 guess3_2 guess3_3 guess3_4 guess3_5 diff3_0 diff3_1 diff3_2 diff3_3 diff3_4 diff3_5 profit3g sure3 los1 partg partr los2 profit_g profit_r dur3start dur3roll dur3erklaer dur3schaetz dur3erklaerschaetz dur3sicher profit_r_charity client Nationality Semester Follow KnowCharity GiveCharity volunteer Behaviour GeneralFeedback1 nocharityfeedback session

//A.2 Rename variables 
rename DiceNum1 see1
rename DiceNum2 see2
rename DiceNum3 see3
rename roll1 report1
rename roll2 report2
rename roll3 report3
replace see1=0 if see1==6
replace see2=0 if see2==6
replace see3=0 if see3==6
replace report1=0 if report1==6
replace report2=0 if report2==6
replace report3=0 if report3==6

save temp_1, replace 

// -----------------------------------------------------
// B. Load Dictator individual data
// -----------------------------------------------------
clear
import excel using raw.xlsx, firstrow sheet("raw_dictator_ind")


//B.1 Drop unused variables 
drop xx subjects Period ClientNumber LastClientNumber Profit TotalProfit Participate TimeINeedMoreTimePart1DecisionOK q1a_2 q1b_2 q2a_2 q2b_2 TimeSubmitAnswersPart2Instructio TimeINeedMoreTimePart2Instructio TimeContinuePart2Instructions2OK AJ thoughts TimeContinuePart2ThoughtsOK TimeINeedMoreTimePart2ThoughtsOK TimeSubmitPart2DecisionOK TimeINeedMoreTimePart2DecisionOK q1a_3 q1b_3 q2a_3 q2b_3 TimeSubmitAnswersPart3Instructio TimeINeedMoreTimePart3Instructio TimeSubmitPart3DecisionOK TimeINeedMoreTimePart3DecisionOK subject_points charity_points To_Pay_Charity TimeConfirmEndOK TimeINeedMoreTimeEndOK total_to_pay_charity paymentpart Countdown auxFinished LeaveStage TimeIHaveReadTheInstructionsAndV TimeINeedMoreTimeOverallInstruct q1a_1 q1b_1 q2a_1 q2b_1 TimeSubmitAnswersPart1Instructio TimeINeedMoreTimePart1Instructio TimeSubmitPart1DecisionOK 
rename choice1 report1
rename choice2 report2
rename choice3 report3
gen moral = 4
gen team = 0

append using temp_1.dta

save temp_2.dta, replace 

// -----------------------------------------------------
// C. Load Dictator Group data
// -----------------------------------------------------
clear
import excel using raw.xlsx, firstrow sheet("raw_dictator_group")

//C.1 Drop unused variables 
drop xx subjects Period ClientNumber LastClientNumber Profit TotalProfit Participate groupchoice samegroup leave leave_counter paymentpart number_members Countdown auxFinished LeaveStage TimeIHaveReadTheInstructionsAndV TimeINeedMoreTimeOverallInstruct q1a_1 q1b_1 q2a_1 q2b_1 TimeSubmitAnswersPart1Instructio TimeINeedMoreTimePart1Instructio TimeSubmitPart1DecisionOK TimeINeedMoreTimePart1DecisionOK play box q1a_2 q1b_2 q1c_2 q2a_2 q2b_2 q2c_2 q3a_2 q3b_2 q3c_2 TimeExamplesPart2Instructions1OK TimeSubmitAnswersPart2Instructio TimeBackPart2Instructions1OK TimeControlQuestionsPart2Instruc TimeINeedMoreTimePart2Instructio q5 TimeSubmitAnswerPart2Instruction BA TimeLeaveTheChatPart2ChatOK TimeBackPart2ChatOK TimeINeedMoreTimePart2ChatOK TimeSubmitPart2DecisionOK TimeINeedMoreTimePart2DecisionOK q1a_3 q1b_3 q2a_3 q2b_3 TimeSubmitAnswersPart3Instructio TimeINeedMoreTimePart3Instructio TimeSubmitPart3DecisionOK TimeINeedMoreTimePart3DecisionOK member1_choice member2_choice member3_choice subject_points charity_points To_Pay_Charity TimeConfirmEndOK TimeINeedMoreTimeEndOK total_to_pay_charity
rename choice1 report1
rename choice2 report2
rename choice3 report3
gen moral = 4
gen team = 1

append using temp_2.dta

// -----------------------------------------------------
// D. Clean up
// -----------------------------------------------------

//D.1 Label team and individual
label define team 0 "Ind" 1 "Group"
label value team team 
label define moral 1 "Base" 2 "Charity" 3 "CharityR" 4 "Dictator"
label value moral moral

//D.2 Generate treatment variable 
drop treatment
egen treat=group(moral team)
label define treat 1 "I-Base" 2 "G-Base" 3 "I-Charity" 4 "G-Charity" 5 "I-CharityR" 6 "G-CharityR" 7 "I-Dictator" 8 "G-Dictator"
label value treat treat

//D.3 Generate ID variable
egen id = group(date_time Subject)

//D.4 Generate matching group variables
egen matchgroup = group(date_time Group) if team==1
replace matchgroup = 1000+id if team==0

//D.5 Find the group representative
gsort matchgroup
by matchgroup: egen x = min(id)
gen rep = 1 if x==id
drop x

//D.6 Identify problem subjects in the dictator treatment
gen prob = 0
replace prob = 1 if (report1<0 | report2<0 | report3<0)
by matchgroup: egen x = sum(prob)
drop if x>0
drop x prob

//D.7 Did all group members report the same number
by matchgroup: egen x = mean(report2)
gen y = 0
replace y = 1 if report2!=x
by matchgroup: egen z = sum(y)
gen samereport = 1
replace samereport = 0 if z==3
drop x y z
label define samereport 1 "Team reported same number" 0 "Team reported different number"
label value samereport samereport

//D.8 variable for overreporting
gen over1 = (report1>see1) if moral<4
gen over2 = (report2>see2) if rep==1 & samereport==1 & moral<4
gen over3 = (report3>see3) if moral<4

order treat moral team matchgroup id rep IDinGroup see1 report1  over1 see2 report2 over2 samereport see3 report3 over3
label variable treat "Experiment treatment" 
label variable moral "Who is harmed" 
label variable team "Team or individual in part II"
label variable matchgroup "Matching group number"
label variable id "Unique subject ID"
label variable rep "Group representative for part II"
label variable IDinGroup "Group member number for part II"
label variable see1 "points observed in part I"
label variable see2 "points observed in part II"
label variable see3 "points observed in part III" 
label variable report1 "points reported in part I" 
label variable report2 "points reported in part II" 
label variable report3 "points reported in part III" 
label variable over1 "over-report in part I" 
label variable over2 "over-report in part II" 
label variable over3 "over-report in part III" 
label variable samereport "Whether the team reported the same number in part II" 

gen male = (Gender=="Male") if moral<=3
label define male 0 "Female" 1 "Male" 
label value male male
drop Gender

label variable date_time "Date and time of experimental session"
label variable Subject "Subject ID in the session"
label variable Group "Subject Group ID in the session" 
label variable Age "Subjects' Age'" 
label variable male "Gender"

save data_choice.dta,replace
erase temp_1.dta
erase temp_2.dta


//*********************************************************************************************
//CHAT DATA
//*********************************************************************************************

//Step 1. Load the subject level data 
use data_choice.dta
drop  see1 report1 see3 report3 over3
drop if team==0
drop team rep
gsort moral matchgroup
by moral matchgroup: egen total_over1 = sum(over1) if moral<=3
save temp_1.dta, replace


//Step 2. Load the chat data
clear 
import excel using raw.xlsx, firstrow sheet("chat_prepared")
drop die_report die_see treatment BASE CHARITY CHARITYR DICTATOR 
rename subject Subject
rename group Group
rename datetime date_time
merge m:m date_time Group Subject using temp_1.dta
gsort moral date_time Group chat_sequence 
drop Subject treat  member Age male
erase temp_1.dta
order date_time Group matchgroup id  moral IDinGroup chat time chat_sequence see2 report2 samereport  report_argue_1 argument_type_1 report_argue_2 argument_type_2 
drop treatment_coded over1 over2 total_over1 

//Step 3. Label the variables in the Chat data_choice

label variable Group "Group ID in session"
rename report_argue_1 RA1_Code
rename report_argue_2 RA2_Code
rename argument_type_1 RA1_Cat
rename argument_type_2 RA2_Cat

label variable RA1_Code "Coded number by Research assistant 1"
label variable RA2_Code "Coded number by Research assistant 2"
label variable RA1_Cat "Coded category by Research assistant 1"
label variable RA2_Cat "Coded category by Research assistant 2"

label define RA1_Cat 1 "prosocial" 2 "selfish" 3 "honest" 4 "dishonest"
label values RA1_Cat RA1_Cat

label define RA2_Cat 1 "prosocial" 2 "selfish" 3 "honest" 4 "dishonest"
label  values RA2_Cat RA2_Cat

gen drop_in_session = 0 if moral==4
replace drop_in_session = 1 if _merge==1 
drop _merge 

label define drop_in_session 0 "No Group member dropped when online" 1 "Group member dropped when online"
label  values drop_in_session drop_in_session

replace RA1_Code = 0.00 if RA1_Code==6.00 // Rescale  argument in terms of points for the team as opposed to die roll number
replace RA2_Code = 0.00 if RA2_Code==6.00 // Rescale  argument in terms of points for the team as opposed to die roll number

save chat_data.dta,replace


