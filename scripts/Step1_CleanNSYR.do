clear
capture log close
set more off

* This do-file cleans the variables and appends the NSYR survey files

global datadir ///
	"../data/"
ssc install xtpatternvar
	
*******************************************************************************
* CLEAN ***********************************************************************
*******************************************************************************

* Wave 1

	use "$datadir/NSYRW1_raw.dta", clear
	rename *, lower
	
	keep ids agecats /*
		*/ faith1 godclose prayansr miracle comitgod doubts1 doubts2 /*
		*/ spiritua aftrlife angels demons miracles god judgeday godview okayconv /*
		*/ tworel1 tworel2 viewrel congmust okaypick lrnrel1 lrnrel2 /*
		*/ y187f_01 y187f_02 y187f_03 y187f_04 y187f_05 /*
		*/ y187h_01 y187h_02 y187h_03 y187h_04 y187h_05 /*
		*/ frnds1 frnds2 sschl i_gender bntwhite i_religion bnregso bndadsmc /*
		*/ pincome1 bnmomsmc pimprel bnplmarr attend1 ythgrp2
	
	* missing observations
	recode doubts1 doubts2 (5=.) 
	recode doubts1 doubts2 lrnrel1 lrnrel2 (999=5)
	recode tworel1 tworel2 frnds1 frnds2 attend1 (999=0)
	recode god (2=3) (777=2)
	recode judgeday (2=0) (777=0)
	mvdecode _all, mv(666/999)
	
	* religious beliefs
	replace faith1=6-faith1
	replace spiritua=4-spiritua
	replace doubts1=5-doubts1
	replace doubts2=5-doubts2
	replace lrnrel1=5-lrnrel1
	replace lrnrel2=5-lrnrel2
	gen learnrel=lrnrel1+lrnrel2
	replace aftrlife=4-aftrlife
	replace angels=4-angels
	replace demons=4-demons
	replace miracles=4-miracles
	replace god=4-god
	recode okayconv congmust (2=0) 
	recode godview (2/4=0)
	replace viewrel=4-viewrel
	gen doubts=doubts1+doubts2
	gen tworel=tworel1+tworel2
	recode okaypick tworel (1=0) (2=1)
	
	* respondent characteristics
	gen frnds=frnds1+frnds2
		foreach var of varlist y187h_01 y187h_02 y187h_03 y187h_04 y187h_05 {
		replace `var'=1-`var'
	}
	egen friend1=rowmax(y187h_01 y187f_01)
	egen friend2=rowmax(y187h_02 y187f_02)
	egen friend3=rowmax(y187h_03 y187f_03)
	egen friend4=rowmax(y187h_04 y187f_04)
	egen friend5=rowmax(y187h_05 y187f_05)
	gen frrelbelief=friend1+friend2+friend3+friend4+friend5
	gen peers=frrelbelief/frnds
	replace peers=0 if peers < 1
	recode pincome1 (1=0) (2=1)
	recode pimprel (2/6=0)
	egen famcollege=rowmax(bndadsmc bnmomsmc)
	gen trauma = 0
	recode sschl (1=0) (2/7=1)
	gen noaffil=1 if i_religion==7
	replace noaffil=0 if i_religion!=7
	recode i_religion (7=5) (8=6)

	drop doubts1 doubts2 tworel1 tworel2 bndadsmc bnmomsmc /*
		*/ lrnrel1 lrnrel2 frrelbelief frnds* friend* /*
		*/ y187f_01 y187f_02 y187f_03 y187f_04 y187f_05 /*
		*/ y187h_01 y187h_02 y187h_03 y187h_04 y187h_05
	rename (faith1 tworel pimprel i_gender bntwhite bnregso /*
		*/ bnplmarr pincome attend1 ythgrp2 i_religion) /*
		*/ (faith onlyone famfaith gender rwhites south /*
		*/ married income attend youthgroup relaffil)
	tempfile NSYRtemp1
	save `NSYRtemp1'
	clear

* Wave 2

	use "$datadir/NSYRW2_raw.dta", clear
	rename *, lower
	
	keep ids agecats /*
		*/ faith1 godclose prayansr miracle comitgod doubts1 doubts2 /*
		*/ spiritua aftrlife angels demons miracles god judgeday godview okayconv /*
		*/ tworel1 tworel2 viewrel congmust okaypick lrnrel1 lrnrel2 /*
		*/ i_religion frnds frrelblf sfrrelbl trauma schtypsp attend1 ythgrp2 sschl

	* missing observations
	recode doubts1 doubts2 (5=.) 
	recode doubts1 doubts2 lrnrel1 lrnrel2 (999=5)	
	recode tworel1 tworel2 frrelblf sfrrelbl attend1 ythgrp2 (999=0)
	mvdecode _all, mv(666/999)
	
	* religious beliefs
	replace faith1=6-faith1
	replace spiritua=4-spiritua
	replace doubts1=5-doubts1
	replace doubts2=5-doubts2
	replace lrnrel1=5-lrnrel1
	replace lrnrel2=5-lrnrel2
	gen learnrel=lrnrel1+lrnrel2
	recode god (2=4)
	recode god (3=2) (4=3)
	replace god=4-god
	replace aftrlife=4-aftrlife
	replace angels=4-angels
	replace demons=4-demons
	replace miracles=4-miracles
	recode okayconv congmust (2=0) 
	recode godview (2/4=0)
	replace viewrel=4-viewrel
	gen doubts=doubts1+doubts2
	gen tworel=tworel1+tworel2
	recode okaypick tworel (1=0) (2=1)
	
	* respondent characteristics
	gen frrelbelief=frrelblf+sfrrelb
	gen peers=frrelbelief/frnds
	replace peers=0 if peers<1
	gen	college=1 if schtypsp==5
	replace college=0 if schtypsp!=5
	recode sschl (1=0) (2/7=1)
	gen noaffil=1 if i_religion==5
	replace noaffil=0 if i_religion!=5
	recode i_religion (9=3) (3=4)
	
	drop doubts1 doubts2 tworel1 tworel2 lrnrel1 lrnrel2 /*
		*/ frnds frrelblf sfrrelbl frrelbelief schtypsp
	rename (faith1 tworel attend1 ythgrp2 i_religion) /*
		*/ (faith onlyone attend youthgroup relaffil)
	tempfile NSYRtemp2
	save `NSYRtemp2'
	clear
	
* Wave 3

	use "$datadir/NSYRW3_raw.dta", clear
	rename *, lower
	keep if inwave3==1

	keep ids agecats /*
		*/ faith1 godclose prayansr miracle comitgod doubts1 doubts2 spirtual /*
		*/ aftrlife angels demons miracles god judgeday godview okayconv tworel1 /*
		*/ tworel2 viewrel congmust okaypick lrnrel1 lrnrel2 i_religion /*
		*/ numfrien frrelblf sfrrelbl trauma cu_attco finhrd attend1 ythgr2_2 sschl

	* missing observations
	recode doubts1 doubts2 (5=.) 
	recode doubts1 doubts2 lrnrel1 lrnrel2 (999=5)
	recode tworel1 tworel2 frrelblf sfrrelbl attend1 (999=0)
	mvdecode _all, mv(666/999)
	
	* religious beliefs
	replace faith1=6-faith1
	replace spirtual=4-spirtual
	replace doubts1=5-doubts1
	replace doubts2=5-doubts2
	replace lrnrel1=5-lrnrel1
	replace lrnrel2=5-lrnrel2
	gen learnrel=lrnrel1+lrnrel2
	recode learnrel (0=.)
	recode god (2=4)
	recode god (3=2) (4=3)
	replace god=4-god
	replace aftrlife=4-aftrlife
	replace angels=4-angels
	replace demons=4-demons
	replace miracles=4-miracles
	recode okayconv congmust (2=0) 
	recode godview (2/4=0)
	replace viewrel=4-viewrel
	gen doubts=doubts1+doubts2
	drop if doubts==0
	gen tworel=tworel1+tworel2
	recode okaypick tworel (1=0) (2=1)
	
	* respondent characteristics
	gen frrelbelief=frrelblf+sfrrelb
	gen peers=frrelbelief/numfrien
	replace peers=0 if peers<1
	recode sschl (1=0) (2/7=1)
	gen noaffil=1 if i_religion==8
	replace noaffil=0 if i_religion!=8
	recode i_religion (5=4) (8=5) (9=6)
	
	drop doubts1 doubts2 tworel1 tworel2 lrnrel1 lrnrel2 /*
		*/ numfrien frrelblf sfrrelbl frrelbelief
	rename (faith1 tworel spirtual cu_attco attend1 ythgr2_2 i_religion) /*
		*/ (faith onlyone spiritua college attend youthgroup relaffil)	
	tempfile NSYRtemp3
	save `NSYRtemp3'
	clear
	
* Wave 4

	use "$datadir/NSYRW4_raw.dta", clear
	rename *, lower
	rename *_w4 *
	
	keep ids agecats /*
		*/ faith1 godclose prayansr miracle comitgod doubts1 doubts2 doubts3 /*
		*/ spiritual afterlife angels demons miracles god judgeday godview okayconv /*
		*/ tworell viewrel congmust1-congmust7 okaypick lrnrel1 /*
		*/ tradrel friends friends2 friend5_2 friend4_2 friend3_2 friend2_2 /*
		*/ friend1_2 lifetrauma edatt finhrdshp attend1 ythgrpcoll sschl
	
	* missing observations
	recode doubts1 doubts2 doubts3 (-99=5)
	recode congmust1-congmust7 (-99=.)
	recode friends2 friend5_2 friend4_2 friend3_2 friend2_2 friend1_2 (-99=0)
	recode ythgrpcoll (-99=0)
	recode attend1 (-99=7)
	mvdecode _all, mv(-99)
	
	* religious beliefs
	replace faith1=6-faith1
	recode godclose (7=.)
	replace godclose=7-godclose
	recode prayansr miracle comitgod (2=0)
	replace spiritual=4-spiritual
	replace doubts1=5-doubts1
	replace doubts2=5-doubts2
	replace doubts3=5-doubts3
	egen doubts=rowmax(doubts1 doubts2 doubts3)
	recode doubts (0=.)
	recode judgeday (2=0)
	recode god (0=4) (1=5) (2=6)
	recode god (4=1) (6=2) (5=3)
	replace afterlife=4-afterlife
	replace angels=4-angels
	replace demons=4-demons
	replace miracles=4-miracles
	replace tworell=1-tworell
	replace viewrel=4-viewrel
	egen congmust = rowmax(congmust1-congmust7)
	replace okaypick=1-okaypick
	recode godview (2/4=0)
	replace lrnrel1=5-lrnrel1
	
	* response characteristics
	replace friends = friends+friends2
	egen frrelbelief = rowtotal(friend5_2 friend4_2 friend3_2 friend2_2 friend1_2)
	gen peers=frrelbelief/friends
	replace peers=0 if peer<1
	gen college=1 if edatt==4 | edatt==5
	replace college=0 if edatt<4
	recode agecats (29/32=28)
	replace attend1=7-attend1
	recode sschl (7=0) (1/6=1)
	gen noaffil=1 if tradrel==8
	replace noaffil=0 if tradrel!=8
	recode tradrel (4=3) (5=4) (6/7=6) (8=5) (9/13=6)
	recode ythgrpcoll (2=1)

	drop doubts1 doubts2 doubts3 congmust1-congmust7 friends friends2 /*
		*/ friend5_2 friend4_2 friend3_2 friend2_2 friend1_2 frrelbelief edatt
	rename (faith1 tworell spiritual afterlife lrnrel1 lifetrauma /*
		*/ finhrdshp attend1 ythgrpcoll tradrel) /*
		*/ (faith onlyone spiritua aftrlife learnrel trauma /*
		*/ finhrd attend youthgroup relaffil)
	tempfile NSYRtemp4
	save `NSYRtemp4'
	clear
	
*******************************************************************************
* APPEND & RECODE *************************************************************
*******************************************************************************	

* Append the Data & Recode Variables

	use `NSYRtemp1'
	sort ids
	qui append using `NSYRtemp2' `NSYRtemp3' `NSYRtemp4', gen(waves)
	replace waves=waves+1
	xtset ids waves
	
	foreach var of varlist gender rwhites famcollege famfaith south married income {
		bysort ids (waves): replace `var' = `var'[1] if missing(`var')
	}
	
	recode attend (2/6=1)
	bysort ids: egen lifetrauma = total(trauma)
	replace lifetrauma = 1 if lifetrauma>0 & lifetrauma!=.
		drop trauma
	bysort ids: egen evercollege = sum(college)
	replace evercollege = 1 if evercollege>0 & evercollege!=.
		drop college
	bysort ids: egen hardships = sum(finhrd)
	replace hardships = 1 if hardships>0 & hardships!=.
	replace hardships = 0 if income==1
		drop finhrd
	bysort ids: egen youthgr = sum(youthgroup)
	replace youthgr = 1 if youthgr>0 & youthgr!=.
		drop youthgroup
	bysort ids: egen reltraining = sum(sschl)
	replace reltraining = 1 if reltraining>0 & reltraining!=.
		drop sschl
	gen drelaffil = d.relaffil
	recode drelaffil (-5/-1=1) (1/5=1)
	bysort ids: egen conversion = sum(drelaffil)
	replace conversion = 1 if conversion>0 & conversion!=.
		drop drelaffil relaffil

* Strip All Labels, and Describe the Missing Data and Panel Structure

	_strip_labels _all
	foreach var of varlist _all {
		label var `var' ""
	}
	mdesc if waves==1
	mdesc if waves==2
	mdesc if waves==3
	mdesc if waves==4		
	xtdescribe
	drop if agecats==.
	
	save "$datadir/NSYR_ALL.dta", replace
	
*******************************************************************************
* BELIEF CHANGES **************************************************************
*******************************************************************************

* Construct Change Scores

	keep ids agecats waves faith prayansr miracle comitgod attend aftrlife /*
		*/ angels demons miracles god godclose judgeday godview /*
		*/ spiritua okayconv viewrel congmust okaypick learnrel doubts onlyone

	xtpatternvar, gen(pattern)
	drop if pattern == "...1"
	drop if pattern == "1..."
	drop if pattern == "1..1"
	drop if pattern == "1.1."
		drop pattern
	
	ds ids agecats waves, not
	foreach var of varlist `r(varlist)' {
		bysort ids (waves): gen d_`var' = `var' - `var'[1]
		replace d_`var' = abs(d_`var')
		recode d_`var' (1/10=1)
	}
	egen totalchanges = rowtotal(d_*)
	drop if waves==1
	keep ids agecats waves totalchanges
	
	save "$datadir/NSYR_varchange.dta", replace

clear
