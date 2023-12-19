import excel "D:\Forskning og undervisning\Parliamentary Power and Coalition Participation\Copy of parlgov with other data_and_commitee_v3.xlsx", sheet("cabinet") firstrow


*Labelling country ID*
labmask country_id, values (country_name)

sum Index_01scaled_commitee_no Wehner_Index if country_name!="Switzerland"

* Overview of distribution of parliamentary committee power*
graph hbar Index_01scaled_commitee_no, over (country_id, sort(Index_01scaled_commitee_no)descending)  bargap(50) bar(1, color(black)) graphregion(color(white))legend (off) ytitle("Committee power index"), if Index_01scaled_commitee_no > 5.381227 & country_name!="Switzerland"

graph hbar Index_01scaled_commitee_no, over (country_id, sort(Index_01scaled_commitee_no)descending)  bargap(50) bar(1, color(black)) graphregion(color(white))legend (off) ytitle("Committee power index"), if Index_01scaled_commitee_no <= 5.381227


graph hbar Index_01scaled_commitee_no, over (country_id, sort(Index_01scaled_commitee_no)descending)  bargap(100) bar(1, color(black)) graphregion(color(white))legend (off) ytitle("Committee power index"), if  country_name!="Switzerland"






* Overview of distribution of parliamentary budgetary power*
graph hbar Wehner_Index , over (country_id, sort(Wehner_Index ) descending) bar(1, color(black)) graphregion(color(white))legend (off) ytitle("Parliamentary budgetary power"), if  Wehner_Index  > 52.48329 & country_name!="Switzerland"

graph hbar Wehner_Index , over (country_id, sort(Wehner_Index ) descending) bar(1, color(black)) graphregion(color(white))legend (off) ytitle("Parliamentary budgetary power") , if Wehner_Index <= 52.48329


graph hbar Wehner_Index , over (country_id, sort(Wehner_Index ) descending) bar(1, color(black)) graphregion(color(white))legend (off) ytitle("Parliamentary budgetary power"), if  country_name!="Switzerland"



*Generation of variables which measure idelogical distance/closeness to prime minister's party 
bysort cabinet_id  : egen primeminister_rile= mean(rile) if prime_minister==1 



bysort cabinet_id : egen primeminister_rile2= mean(primeminister_rile) 


generate distance_primeminister_rile= abs(rile-primeminister_rile2)

generate closeness_primeminister_rile= 100 - distance_primeminister_rile


*Generation of prime minister seat share variable* 
bysort cabinet_id  : egen primeminister_seats= mean(seats) if prime_minister==1 

bysort cabinet_id : egen primeminister_seats2= mean(primeminister_seats) 


generate primeminister_seat_share= primeminister_seats2/election_seats_total

generate single_party_majority=0 
replace single_party_majority=1 if primeminister_seat_share>0.5
replace single_party_majority=. if primeminister_seat_share==. 

generate single_party_majority2= 0
replace single_party_majority2=1 if gov_type==1
replace single_party_majority2=. if gov_type==.


* Minority government*
generate minority_gov=0
replace minority_gov=1 if gov_type==4 | gov_type==5 
replace minority_gov=. if gov_type==. 


*Generate a standard deviation of (rile) ideology*
bysort cabinet_id : egen rile_sd= sd(rile) 

*Generation of a seat-share adjusted standard deviation of (rile) ideology
generate seat_share= seats/election_seats_total

generate seat_share_adjusted_rile= seat_share*rile

bysort cabinet_id : egen seat_share_adjusted_rile_sd= sd(seat_share_adjusted_rile) 


* Generation of seat-share adjusted distance to prime minister party*
generate close_prime_rile_adjusted = seat_share*closeness_primeminister_rile


*Generation of semi-presidentialism dummy*
generate semipresident=0
replace semipresident=1 if pres==1 | pres==2 | pres==3 
replace semipresident=. if pres==. 

replace semipresident=0 if country_name=="Israel"

replace semipresident=0 if country_name=="Turkey" & election_year<2007

replace semipresident=1 if country_name=="Turkey" & election_year>2006


*Setting bicameralism before 1990 to missing*
replace bicameralism=. if election_year<1990

*Collapsing to the Cabinet level*

collapse days_since_election election_year formation_year caretaker  Wehner_Index  bicameralism primeminister_seat_share  close_prime_rile_adjusted  seat_share_adjusted_rile_sd single_party_majority primeminister_seats2 closeness_primeminister_rile year  rile_sd election_id country_id NoofCommittees Correspondence CompelMinister CompelCC Initiaite EarlyStage Plenaryvoteasamended Timetablecontrol Index_of_Committee_Power Index_without_no Index_01scaled_commitee_no  investiturepositiveparliamenta semipresident  minority_gov rae_ele rae_leg effpar_ele effpar_leg realgdpgr pres inflation debt receipts outlays unemp postfisc_gini pop65  , by (cabinet_id)

*Dropping caretaker governments*
drop if caretaker==1

*Making duplication variable for each election*
sort election_id days_since_election

by election_id :  gen dup = cond(_N==1,0,_n)

* Cox proportional hazard models: 1990-*

replace days_since_election = .01 if days_since_election==0
stset days_since_election 


*Correlation between the two indexes*
corr Index_01scaled_commitee_no Wehner_Index

* Missing investiture data point*
list country_id election_year if days_since_election!=. &  investiturepositiveparliamenta==.  &  election_year>1989   & dup<2 & Index_01scaled_commitee_no!=.

replace investiturepositiveparliamenta=0 if country_id==41 & election_year==2019 

* Descriptive statistics*
sum days_since_election  Index_01scaled_commitee_no Wehner_Index investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident if  election_year>1989   & dup<2 & Index_01scaled_commitee_no!=.

* List of countries*
sort country_id 
list country_id election_year if days_since_election!=. &  Index_01scaled_commitee_no!=.  &  election_year>1989   & dup<2 


* Scatter plots*
 twoway (scatter days_since_election   Index_01scaled_commitee_no , mcolor(gs7)  ) (lfit days_since_election   Index_01scaled_commitee_no  , lcolor(gs0) ), legend (off) ytitle (Cabinet formation time (days)) xtitle (Committee power index) graphregion(color(white))


 twoway (scatter days_since_election   Wehner_Index   , mcolor(gs7)  ) (lfit days_since_election   Wehner_Index    , lcolor(gs0) ), legend (off) ytitle (Cabinet formation time (days)) xtitle (Parliamentary budgetary power) graphregion(color(white))

*Analysis 
stcox  Index_01scaled_commitee_no , efron nohr   cluster ( country_id), if  election_year>1989   & dup<2
stcox  Index_01scaled_commitee_no  i.election_year  , efron nohr schoenfeld(sc*) cluster ( country_id), if election_year>1989  & dup<2
stphtest, detail
 drop sc*
stcox  Index_01scaled_commitee_no single_party_majority i.election_year  , efron nohr  cluster ( country_id), if election_year>1989   & dup<2
stcox  Index_01scaled_commitee_no investiturepositiveparliamenta  i.election_year , efron nohr  cluster ( country_id), if election_year>1989   & dup<2
  stcox  Index_01scaled_commitee_no  effpar_leg   i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2
 stcox  Index_01scaled_commitee_no  seat_share_adjusted_rile_sd    i.election_year , efron nohr  cluster ( country_id), if election_year>1989   & dup<2
  stcox  Index_01scaled_commitee_no   close_prime_rile_adjusted   i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2
    stcox  Index_01scaled_commitee_no   semipresident i.election_year   , efron nohr  cluster ( country_id), if election_year>1989   & dup<2
stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2
  
 stcox  Index_01scaled_commitee_no investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg semipresident    i.election_year  , efron nohr schoenfeld(sc*) cluster ( country_id), if election_year>1989  & dup<2
stphtest, detail
drop sc*
 margins, at(Index_01scaled_commitee_no = (2(1)8))
marginsplot, level(90)xtitle (Parliamentary Committee Power) graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") 


generate high_committee_power=0
replace high_committee_power=1 if Index_01scaled_commitee_no> 4.926227
replace high_committee_power=. if Index_01scaled_commitee_no==. 

sts graph, by(high_committee_power ) graphregion(color(white)) scheme(s2mono)  , if election_year>1989  & dup<2
sts graph, by(high_committee_power ) graphregion(color(white)) scheme(s2mono) title("") legend(lab(1 "Low committee power") lab(2 "High committe power")) , if election_year>1989  & dup<2


stcox   Wehner_Index   , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2
stcox   Wehner_Index  i.election_year  , efron nohr schoenfeld(sc*)  cluster ( country_id), if election_year>1989  & dup<2
stphtest, detail
 drop sc*
stcox   Wehner_Index single_party_majority i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2
stcox   Wehner_Index investiturepositiveparliamenta i.election_year  , efron nohr  cluster ( country_id), if election_year>1989   & dup<2
 stcox  Wehner_Index  effpar_leg    i.election_year , efron nohr   cluster ( country_id), if  election_year>1989  & dup<2
 stcox   Wehner_Index seat_share_adjusted_rile_sd   i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2
 stcox  Wehner_Index   close_prime_rile_adjusted   i.election_year  , efron nohr  cluster ( country_id), if election_year>1989   & dup<2
  stcox  Wehner_Index   semipresident   i.election_year  , efron nohr  cluster ( country_id), if election_year>1989   & dup<2
 stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr schoenfeld(sc*)  cluster ( country_id), if election_year>1989  & dup<2
 stphtest, detail
 drop sc*
 margins, at(Wehner_Index = (20(5)75))
marginsplot, level(90)xtitle (Parliamentary Budgetary Power (Wehner Index)) graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") 

generate high_budget_power=0
replace high_budget_power=1 if Wehner_Index>51.106
replace high_budget_power=. if Wehner_Index==. 

sts graph, by(high_budget_power )graphregion(color(white)) scheme(s2mono) , if election_year>1989  & dup<2

sts graph, by(high_budget_power )graphregion(color(white)) scheme(s2mono) title("") legend(lab(1 "Low budgetary power") lab(2 "High budgetary power")),  if election_year>1989  & dup<2



* Controlling for bicamerialism*
stcox   Index_01scaled_commitee_no  bicameralism  i.election_year  , efron nohr schoenfeld(sc*)  cluster ( country_id), if election_year>1989  & dup<2
 drop sc*

stcox  Index_01scaled_commitee_no  bicameralism  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2

stcox   Wehner_Index bicameralism  i.election_year  , efron nohr schoenfeld(sc*)  cluster ( country_id), if election_year>1989  & dup<2
 drop sc*
stcox Wehner_Index  bicameralism investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2


*Both together*
stcox  Index_01scaled_commitee  Wehner_Index   , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2
stcox  Index_01scaled_commitee Wehner_Index  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989   & dup<2
stcox Index_01scaled_commitee  Wehner_Index single_party_majority i.election_year , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2
stcox  Index_01scaled_commitee Wehner_Index investiturepositiveparliamenta i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989 & dup<2
 stcox Index_01scaled_commitee Wehner_Index  effpar_leg   i.election_year   , efron nohr  cluster ( country_id), if  election_year>1989 & dup<2
 stcox  Index_01scaled_commitee Wehner_Index seat_share_adjusted_rile_sd   i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2
 stcox Index_01scaled_commitee Wehner_Index   close_prime_rile_adjusted   i.election_year  , efron nohr  cluster ( country_id), if election_year>1989 & dup<2
  stcox Index_01scaled_commitee Wehner_Index   semipresident   i.election_year  , efron nohr  cluster ( country_id), if election_year>1989 &  dup<2
stcox  Index_01scaled_commitee_no Wehner_Index investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2



*Final sample in all estimations. 
stcox  Index_01scaled_commitee_no  i.election_year  , efron nohr cluster ( country_id), if election_year>1989  & dup<2 & investiturepositiveparliamenta!=. & single_party_majority!=. & close_prime_rile_adjusted!=. & seat_share_adjusted_rile_sd!=. & effpar_leg!=. &  semipresident!=. 
stcox  Index_01scaled_commitee_no single_party_majority i.election_year  , efron nohr  cluster ( country_id), if election_year>1989   & dup<2 & investiturepositiveparliamenta!=. & single_party_majority!=. & close_prime_rile_adjusted!=. & seat_share_adjusted_rile_sd!=. & effpar_leg!=. &  semipresident!=. 
stcox  Index_01scaled_commitee_no investiturepositiveparliamenta  i.election_year , efron nohr  cluster ( country_id), if election_year>1989   & dup<2 & investiturepositiveparliamenta!=. & single_party_majority!=. & close_prime_rile_adjusted!=. & seat_share_adjusted_rile_sd!=. & effpar_leg!=. &  semipresident!=. 
  stcox  Index_01scaled_commitee_no  effpar_leg   i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2 & investiturepositiveparliamenta!=. & single_party_majority!=. & close_prime_rile_adjusted!=. & seat_share_adjusted_rile_sd!=. & effpar_leg!=. &  semipresident!=. 
 stcox  Index_01scaled_commitee_no  seat_share_adjusted_rile_sd    i.election_year , efron nohr  cluster ( country_id), if election_year>1989   & dup<2 & investiturepositiveparliamenta!=. & single_party_majority!=. & close_prime_rile_adjusted!=. & seat_share_adjusted_rile_sd!=. & effpar_leg!=. &  semipresident!=. 
  stcox  Index_01scaled_commitee_no   close_prime_rile_adjusted   i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & investiturepositiveparliamenta!=. & single_party_majority!=. & close_prime_rile_adjusted!=. & seat_share_adjusted_rile_sd!=. & effpar_leg!=. &  semipresident!=. 
    stcox  Index_01scaled_commitee_no   semipresident i.election_year   , efron nohr  cluster ( country_id), if election_year>1989   & dup<2 & investiturepositiveparliamenta!=. & single_party_majority!=. & close_prime_rile_adjusted!=. & seat_share_adjusted_rile_sd!=. & effpar_leg!=. &  semipresident!=.  
stcox  Index_01scaled_commitee_no investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg semipresident    i.election_year  , efron nohr cluster ( country_id), if election_year>1989  & dup<2 & investiturepositiveparliamenta!=. & single_party_majority!=. & close_prime_rile_adjusted!=. & seat_share_adjusted_rile_sd!=. & effpar_leg!=. &  semipresident!=. 



stcox  Wehner_Index   i.election_year  , efron nohr cluster ( country_id), if election_year>1989  & dup<2 & investiturepositiveparliamenta!=. & single_party_majority!=. & close_prime_rile_adjusted!=. & seat_share_adjusted_rile_sd!=. & effpar_leg!=. &  semipresident!=. 
stcox  Wehner_Index  single_party_majority i.election_year  , efron nohr  cluster ( country_id), if election_year>1989   & dup<2 & investiturepositiveparliamenta!=. & single_party_majority!=. & close_prime_rile_adjusted!=. & seat_share_adjusted_rile_sd!=. & effpar_leg!=. &  semipresident!=. 
stcox  Wehner_Index  investiturepositiveparliamenta  i.election_year , efron nohr  cluster ( country_id), if election_year>1989   & dup<2 & investiturepositiveparliamenta!=. & single_party_majority!=. & close_prime_rile_adjusted!=. & seat_share_adjusted_rile_sd!=. & effpar_leg!=. &  semipresident!=. 
  stcox  Wehner_Index   effpar_leg   i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2 & investiturepositiveparliamenta!=. & single_party_majority!=. & close_prime_rile_adjusted!=. & seat_share_adjusted_rile_sd!=. & effpar_leg!=. &  semipresident!=. 
 stcox  Wehner_Index   seat_share_adjusted_rile_sd    i.election_year , efron nohr  cluster ( country_id), if election_year>1989   & dup<2 & investiturepositiveparliamenta!=. & single_party_majority!=. & close_prime_rile_adjusted!=. & seat_share_adjusted_rile_sd!=. & effpar_leg!=. &  semipresident!=. 
  stcox  Wehner_Index   close_prime_rile_adjusted   i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & investiturepositiveparliamenta!=. & single_party_majority!=. & close_prime_rile_adjusted!=. & seat_share_adjusted_rile_sd!=. & effpar_leg!=. &  semipresident!=. 
    stcox  Wehner_Index   semipresident i.election_year   , efron nohr  cluster ( country_id), if election_year>1989   & dup<2 & investiturepositiveparliamenta!=. & single_party_majority!=. & close_prime_rile_adjusted!=. & seat_share_adjusted_rile_sd!=. & effpar_leg!=. &  semipresident!=.  
stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg semipresident    i.election_year  , efron nohr cluster ( country_id), if election_year>1989  & dup<2 & investiturepositiveparliamenta!=. & single_party_majority!=. & close_prime_rile_adjusted!=. & seat_share_adjusted_rile_sd!=. & effpar_leg!=. &  semipresident!=. 



*First 100 days*
stcox  Index_01scaled_commitee_no , efron nohr   cluster ( country_id), if  election_year>1989   & days_since_election<100.01 
stcox  Index_01scaled_commitee_no  i.election_year  , efron nohr schoenfeld(sc*) cluster ( country_id), if election_year>1989   & days_since_election<100.01 
stphtest, detail
 drop sc*
stcox  Index_01scaled_commitee_no single_party_majority i.election_year  , efron nohr  cluster ( country_id), if election_year>1989    & days_since_election<100.01 
stcox  Index_01scaled_commitee_no investiturepositiveparliamenta  i.election_year , efron nohr  cluster ( country_id), if election_year>1989    & days_since_election<100.01 
  stcox  Index_01scaled_commitee_no  effpar_leg   i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & days_since_election<100.01 
 stcox  Index_01scaled_commitee_no  seat_share_adjusted_rile_sd    i.election_year , efron nohr  cluster ( country_id), if election_year>1989    & days_since_election<100.01 
  stcox  Index_01scaled_commitee_no   close_prime_rile_adjusted   i.election_year  , efron nohr  cluster ( country_id), if election_year>1989   & days_since_election<100.01 
    stcox  Index_01scaled_commitee_no   semipresident i.election_year   , efron nohr  cluster ( country_id), if election_year>1989    & days_since_election<100.01 
stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989   & days_since_election<100.01 
  


stcox   Wehner_Index   , efron nohr  cluster ( country_id), if  election_year>1989   & days_since_election<100.01 
stcox   Wehner_Index  i.election_year  , efron nohr schoenfeld(sc*)  cluster ( country_id), if election_year>1989   & days_since_election<100.01 
stphtest, detail
 drop sc*
stcox   Wehner_Index single_party_majority i.election_year  , efron nohr  cluster ( country_id), if election_year>1989   & days_since_election<100.01 
stcox   Wehner_Index investiturepositiveparliamenta i.election_year  , efron nohr  cluster ( country_id), if election_year>1989    & days_since_election<100.01 
 stcox  Wehner_Index  effpar_leg    i.election_year , efron nohr   cluster ( country_id), if  election_year>1989   & days_since_election<100.01 
 stcox   Wehner_Index seat_share_adjusted_rile_sd   i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989   & days_since_election<100.01 
 stcox  Wehner_Index   close_prime_rile_adjusted   i.election_year  , efron nohr  cluster ( country_id), if election_year>1989    & days_since_election<100.01 
  stcox  Wehner_Index   semipresident   i.election_year  , efron nohr  cluster ( country_id), if election_year>1989    & days_since_election<100.01 
 stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr schoenfeld(sc*)  cluster ( country_id), if election_year>1989   & days_since_election<100.01 
 stphtest, detail
 drop sc*



* 2000-*

stcox  Index_01scaled_commitee_no , efron nohr  cluster ( country_id), if election_year>1999   & dup<2
stcox  Index_01scaled_commitee_no  i.election_year , efron nohr  cluster ( country_id), if election_year>1999    & dup<2
stcox  Index_01scaled_commitee_no single_party_majority i.election_year , efron nohr  cluster ( country_id), if election_year>1999    & dup<2
stcox  Index_01scaled_commitee_no investiturepositiveparliamenta i.election_year  , efron nohr  cluster ( country_id), if election_year>1999  & dup<2
  stcox  Index_01scaled_commitee_no  effpar_leg   i.election_year  , efron nohr  cluster ( country_id), if election_year>1999   & dup<2
 stcox  Index_01scaled_commitee_no  seat_share_adjusted_rile_sd  i.election_year  , efron nohr  cluster ( country_id), if election_year>1999    & dup<2
  stcox  Index_01scaled_commitee_no   close_prime_rile_adjusted   i.election_year  , efron nohr  cluster ( country_id), if election_year>1999   & dup<2
    stcox  Index_01scaled_commitee_no   semipresident  i.election_year , efron nohr  cluster ( country_id), if election_year>1999    & dup<2
stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if election_year>1999   & dup<2

 stcox  Index_01scaled_commitee_no investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr schoenfeld(sc*) cluster ( country_id), if election_year>1999   & dup<2
stphtest, detail
drop sc*
 margins, at(Index_01scaled_commitee_no = (2(1)8))
marginsplot, level(90)xtitle (Parliamentary Committee Power) graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") 



stcox   Wehner_Index   , efron nohr  cluster ( country_id), if election_year>1999    & dup<2
stcox   Wehner_Index  i.election_year , efron nohr  cluster ( country_id), if  election_year>1999  & dup<2
stcox   Wehner_Index single_party_majority i.election_year  , efron nohr  cluster ( country_id), if election_year>1999   & dup<2
stcox   Wehner_Index investiturepositiveparliamenta i.election_year  , efron nohr  cluster ( country_id), if election_year>1999   & dup<2
 stcox  Wehner_Index  effpar_leg    i.election_year , efron nohr   cluster ( country_id), if election_year>1999   & dup<2
 stcox   Wehner_Index seat_share_adjusted_rile_sd   i.election_year  , efron nohr  cluster ( country_id), if election_year>1999   & dup<2
 stcox  Wehner_Index   close_prime_rile_adjusted   i.election_year  , efron nohr  cluster ( country_id), if election_year>1999  & dup<2
  stcox  Wehner_Index   semipresident   i.election_year  , efron nohr  cluster ( country_id), if  election_year>1999   & dup<2
 stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr schoenfeld(sc*)  cluster ( country_id), if election_year>1999   & dup<2
 stphtest, detail
 drop sc*
 margins, at(Wehner_Index = (20(5)75))
marginsplot, level(90)xtitle (Parliamentary Budgetary Power (Wehner Index)) graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") 


*Excluding instances with a singe party majority party*


stcox  Index_01scaled_commitee_no , efron nohr  cluster ( country_id), if election_year>1999   & dup<2 & single_party_majority!=1
stcox  Index_01scaled_commitee_no  i.election_year , efron nohr  cluster ( country_id), if election_year>1999    & dup<2 & single_party_majority!=1
stcox  Index_01scaled_commitee_no investiturepositiveparliamenta i.election_year  , efron nohr  cluster ( country_id), if election_year>1999  & dup<2 & single_party_majority!=1
  stcox  Index_01scaled_commitee_no  effpar_leg   i.election_year  , efron nohr  cluster ( country_id), if election_year>1999   & dup<2 & single_party_majority!=1
 stcox  Index_01scaled_commitee_no  seat_share_adjusted_rile_sd  i.election_year  , efron nohr  cluster ( country_id), if election_year>1999    & dup<2 & single_party_majority!=1
  stcox  Index_01scaled_commitee_no   close_prime_rile_adjusted   i.election_year  , efron nohr  cluster ( country_id), if election_year>1999   & dup<2 & single_party_majority!=1
    stcox  Index_01scaled_commitee_no   semipresident  i.election_year , efron nohr  cluster ( country_id), if election_year>1999    & dup<2 & single_party_majority!=1
stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if election_year>1999   & dup<2 & single_party_majority!=1
 stcox  Index_01scaled_commitee_no investiturepositiveparliamenta  close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr schoenfeld(sc*) cluster ( country_id), if election_year>1999   & dup<2 & single_party_majority!=1
stphtest, detail
drop sc*
 margins, at(Index_01scaled_commitee_no = (2(1)8))
marginsplot, level(90)xtitle (Parliamentary Committee Power) graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") 



stcox   Wehner_Index   , efron nohr  cluster ( country_id), if election_year>1999    & dup<2 & single_party_majority!=1
stcox   Wehner_Index  i.election_year , efron nohr  cluster ( country_id), if  election_year>1999  & dup<2 & single_party_majority!=1
stcox   Wehner_Index investiturepositiveparliamenta i.election_year  , efron nohr  cluster ( country_id), if election_year>1999   & dup<2 & single_party_majority!=1
 stcox  Wehner_Index  effpar_leg    i.election_year , efron nohr   cluster ( country_id), if election_year>1999   & dup<2 & single_party_majority!=1
 stcox   Wehner_Index seat_share_adjusted_rile_sd   i.election_year  , efron nohr  cluster ( country_id), if election_year>1999   & dup<2 & single_party_majority!=1
 stcox  Wehner_Index   close_prime_rile_adjusted   i.election_year  , efron nohr  cluster ( country_id), if election_year>1999  & dup<2 & single_party_majority!=1
  stcox  Wehner_Index   semipresident   i.election_year  , efron nohr  cluster ( country_id), if  election_year>1999   & dup<2 & single_party_majority!=1
 stcox  Wehner_Index  investiturepositiveparliamenta  close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr schoenfeld(sc*)  cluster ( country_id), if election_year>1999   & dup<2 & single_party_majority!=1
 stphtest, detail
 drop sc*
 margins, at(Wehner_Index = (20(5)75))
marginsplot, level(90)xtitle (Parliamentary Budgetary Power (Wehner Index)) graphregion(color(white))legend (off) scheme(s2mono) recastci(rline) recast(line) title("") 





* Controlling for minority government* 

 stcox  Index_01scaled_commitee_no investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg semipresident  minority_gov   i.election_year  , efron nohr schoenfeld(sc*) cluster ( country_id), if election_year>1989  & dup<2
  stphtest, detail
 drop sc*

 
  stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident minority_gov   i.election_year  , efron nohr schoenfeld(sc*)  cluster ( country_id), if election_year>1989  & dup<2
 stphtest, detail
 drop sc*



* Excluding countries* 

*Excluding Australia
stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=33
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  ,  efron nohr cluster ( country_id), if election_year>1989  & dup<2 & country_id!=33
  
   
   
 *Excluding Austria
 stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=59
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  ,  efron nohr cluster ( country_id), if election_year>1989  & dup<2 & country_id!=59
   
   
 
 
 *Excluding Belgium
 stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=64
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  ,  efron nohr cluster ( country_id), if election_year>1989  & dup<2 & country_id!=64
   
   
 
 
 *Exlcuding Bulgaria*
 stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=10
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr cluster ( country_id), if election_year>1989  & dup<2 & country_id!=10
   
   
 
 
 *Excluding Canada
 stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=29
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=29
   
   
 
 
 *Excluding Croatia*
 stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=62
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr   cluster ( country_id), if election_year>1989  & dup<2 & country_id!=62
   
   
 
 *Excluding Cyprus
 stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=51
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=51
   
   
 
 *Excluding Czech Republic
 stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=68
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=68
   
   
 
 *Excluding Denmark
 stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=21
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=21
   
   
 
 *Excluding Estonia
 stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=75
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr   cluster ( country_id), if election_year>1989  & dup<2 & country_id!=75
   
   
 
 *Excluding Finland
 stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=67
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=67
   
   
 
 *Excluding France
 stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=43
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=43
   
   
 
 *Excluding Germany
 stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=54
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=54
   
   
 
 *Excluding Greece
  stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=41
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=41
   
   
 
 *Excluding Hungary
  stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=39
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=39
   
   
 
 *Excluding Iceland
  stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=56
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=56
   
   
 
 *Excluding Ireland
  stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=37
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=37
   
   
 
 
 *Exluding Israel
  stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=34
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=34
   
   
 
 *Excluding Italy
  stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=26
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=26
   
   
 
 *Excluding Japan
  stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=5
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=5
   
   
 
 
 *Excluding Latvia
  stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=55
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=55
   
   
 
 
 *Excluding Lithuania
  stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=15
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=15
   
   
 
 *Excluding Luxembourg
  stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=7
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=7
   
   
 
 *Excluding Malta
  stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=72
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=72
   
   
 
 *Excluding Netherlands
  stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=8
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=8
   
   
 
 *Excluding New Zealand
  stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=11
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=11
   
   
 
 
 *Excluding Norway
  stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=9
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=9
   
   
 
 *Excluding Poland
  stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=74
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=74
   
   
 
 *Excluding Portugal
  stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=63
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=63
   
   
 
 *Excluding Romania
  stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=23
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=23
   
   
 
 *Excluding Sweden
  stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=35
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=35
   
   
 
 *Excluding Switzerland 
  stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=40
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=40
   
   
 
 * Excluding Slovakia
  stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=1
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=1
   
   
 
 *Excluding Slovenia
  stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=60
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=60
   
   
 
 *Excluding Spain
  stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=27
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=27
   
   
 
 
 * Excluding Turkey
  stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=20
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=20
   
   
 
 * Excluding UK 
 stcox  Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  , efron nohr  cluster ( country_id), if  election_year>1989  & dup<2  & country_id!=44
   stcox  Wehner_Index  investiturepositiveparliamenta single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident  i.election_year  , efron nohr  cluster ( country_id), if election_year>1989  & dup<2 & country_id!=44
   
   




*Average time: OLS:
reg  days_since_election Index_01scaled_commitee_no  i.election_year  ,  cluster ( country_id), if election_year>1989  & dup<2
reg  days_since_election Index_01scaled_commitee_no single_party_majority i.election_year  ,   cluster ( country_id), if election_year>1989   & dup<2
reg  days_since_election Index_01scaled_commitee_no investiturepositiveparliamenta  i.election_year ,  cluster ( country_id), if election_year>1989   & dup<2
reg  days_since_election Index_01scaled_commitee_no  effpar_leg   i.election_year  ,   cluster ( country_id), if  election_year>1989  & dup<2
reg  days_since_election Index_01scaled_commitee_no  seat_share_adjusted_rile_sd    i.election_year ,   cluster ( country_id), if election_year>1989   & dup<2
reg  days_since_election Index_01scaled_commitee_no   close_prime_rile_adjusted   i.election_year  ,   cluster ( country_id), if election_year>1989  & dup<2
reg  days_since_election Index_01scaled_commitee_no   semipresident i.election_year   ,   cluster ( country_id), if election_year>1989   & dup<2
reg  days_since_election Index_01scaled_commitee_no  investiturepositiveparliamenta  single_party_majority close_prime_rile_adjusted seat_share_adjusted_rile_sd effpar_leg  semipresident i.election_year  ,  cluster ( country_id), if  election_year>1989  & dup<2



reg  days_since_election Wehner_Index i.election_year  ,  cluster ( country_id), if election_year>1989  & dup<2
reg  days_since_election Wehner_Index single_party_majority i.election_year  ,   cluster ( country_id), if election_year>1989   & dup<2
reg  days_since_election Wehner_Index investiturepositiveparliamenta  i.election_year ,  cluster ( country_id), if election_year>1989   & dup<2
reg  days_since_election Wehner_Index  effpar_leg   i.election_year  ,   cluster ( country_id), if  election_year>1989  & dup<2
reg  days_since_election Wehner_Index  seat_share_adjusted_rile_sd    i.election_year ,   cluster ( country_id), if election_year>1989   & dup<2
reg  days_since_election  Wehner_Index  close_prime_rile_adjusted   i.election_year  ,   cluster ( country_id), if election_year>1989  & dup<2
reg  days_since_election Wehner_Index   semipresident i.election_year   ,   cluster ( country_id), if election_year>1989   & dup<2
reg  days_since_election  Wehner_Index  single_party_majority investiturepositiveparliamenta effpar_leg   seat_share_adjusted_rile_sd close_prime_rile_adjusted  semipresident i.election_year  ,  cluster ( country_id), if  election_year>1989  & dup<2




