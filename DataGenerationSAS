
libname ft15 'X:\MB\Fuldtællinger\2015' access = readonly;
libname ft14 'X:\MB\Fuldtællinger\2014' access = readonly;
libname ft09 'X:\MB\Fuldtællinger\2009' access = readonly;
libname ft08 'X:\MB\Fuldtællinger\2008' access = readonly;
libname ft07 'X:\MB\Fuldtællinger\2007' access = readonly;
libname ft06 'X:\MB\Fuldtællinger\2006' access = readonly;
libname ft05 'X:\MB\Fuldtællinger\2005' access = readonly;
libname ft04 'X:\MB\Fuldtællinger\2004' access = readonly;
libname ft03 'X:\MB\Fuldtællinger\2003' access = readonly;
libname ft02 'X:\MB\Fuldtællinger\2002' access = readonly;
libname ft01 'X:\MB\Fuldtællinger\2001' access = readonly;

libname andla 'D:\FM\andla\Data';
libname fmt 'D:\DOKUMENTATION\SAS formater i Danmarks Statistik\FORMATKATALOG' access=readonly;
libname fmt2 'D:\FM\PROJEKT\Disruption\Fyringshistorik\Programmer\Formater';
options fmtsearch=(fmt.uddannelser fmt.times_personstatistik));

proc format;
value $audd	(multilabel)
					'10 Grundskole' , '20 Almengymnasiale uddannelser' , '25 Erhvervsgymnasiale uddannelser' = 'Ufaglærte'
					'35 Erhvervsfaglige praktik- og hovedforløb' = 'Faglærte'
					'40 Korte videregående uddannelser' = 'KVU'
					'50 Mellemlange videregående uddannelser' , '60 Bachelor' = 'MVU'
					'65 Lange videregående uddannelser' , '70 Forskeruddannelser' = 'LVU';	

run;

/*****************************************/
/********** Erhvervserfaring og løn ******/
/* konstruer sæt til markov transitioner */
/*****************************************/
/*******
/* 2001
/******
/* Erfaring fra 59 til 2001*/
data exp01;
set ft01.idap (keep=erhver pstill senafar senstar id_nr aldernov erhver79 akasaf);
array change _numeric_;
	do over change;
		if change=. then change=0;
	end;
if aldernov > 018;
if aldernov < 070;
if pstill ^= 93;
erhvår = erhver / 1000;
*/age = substr(aldernov,2,3);
age = input(aldernov,best12.);
exp_tot = erhvår + erhver79;
year = 2001;
laststart = senstar;
lastend = senafar;
lastyearins = akasaf;
drop erhver aldernov  senstar senafar pstill erhver79 erhvår akasaf;
run;

data arb01; set ft01.akm (keep = id_nr beskst13);
typejob = beskst13;
drop beskst13;
run;

/*Alder og årstal 2001*/
data ay01;
set ft01.bef (keep= id_nr alder);
age = alder;
year = 2001;
if alder > 18;
if alder <70;
drop alder;
run;

*/Uddannelse;
data udd01; set ft01.udda (keep=id_nr hfaudd);
edu_full=put(hfaudd,AUDD2015_L1L5_KT.);
edu_segment = put(edu_full,audd.);
drop hfaudd;
run;
data ind01;
set ft01.ind (keep= id_nr formrest_ny05 loenmv_13 brutto dagpenge_kontant_13 sluskat QSPLINDK fradrag);
UnemB  = dagpenge_kontant_13;
Tax    = sluskat;
wage = loenmv_13;
Wealth = formrest_ny05;
taxinc = QSPLINDK ;
deductions = fradrag;
grossinc = brutto;
drop dagpenge_kontant_13 brutto loenmv_13 formrest_ny05 sluskat QSPLINDK fradrag;
run;


proc sql;
create table dag01 as
select distinct ID_nr, 
		FORSIKRINGSKATEGORI_KODE as ui_code,
		akasse 		as ui_ins,
		sum(dpialt) as UI,
		ledars 		as termins 	
from ft01.dur
group by ID_nr
order by ID_nr;
quit;

data dag01; set dag01;
term_ins = input(termins,8.);
drop termins;
run;


Proc Sort data = dag01;
by ID_nr;
run;

proc sort data = ind01;
by ID_nr;
run;
proc sort data = udd01;
by ID_nr;
run;
proc sort data = arb01;
by ID_nr;
run;
Proc Sort data = ay01;
by ID_nr;
run;
Proc Sort data = exp01;
by ID_nr;
run;

data final01;
Merge exp01(IN=EX) ay01(IN=AY) udd01(in=UDD)  arb01(in=arb) ind01(in = IND) dag01;
By ID_nr;
if EX and AY and arb and UDD and IND;
Run;

/********/
/* 2002 */
/********/
/* Erfaring 2002*/
proc sql;
create table exp02 as
select distinct ID_nr, /*FULDDEL, 
		sum(monthsjob) as tot_month_job,;
		sum(SFTJ) as tot_fort_job,*/
		sum(TIMNORM) as tot_norm,
		sum(TIMPRAE) as tot_time_job
from ft02.lon
group by ID_nr
order by ID_nr;
quit;

data exp02;
set exp02;
if tot_time_job > 1740 then experience = 1; 
else if 1740 > tot_time_job > 1500 then experience =(750 + (tot_time_job - 1500)*((1000-750)/(1740-1500))) / 1000; /*indekserer deltid*/
else if 1501 > tot_time_job then experience  = tot_time_job * 0.5 /1000;
run; 

*/Alder og årstal 2002;
data ay02;
set ft02.bef (keep= id_nr alder);
age = alder;
year = 2002;
if alder > 17;
if alder <71;
drop alder;
run;

proc sql;
create table dag02 as
select distinct ID_nr, 
		FORSIKRINGSKATEGORI_KODE as ui_code,
		akasse 		as ui_ins,
		sum(dpialt) as UI,
		ledars 		as termins 	
from ft02.dur
group by ID_nr
order by ID_nr;
quit;

data dag02; set dag02;
term_ins = input(termins,8.);
drop termins;
run;

*/ Løn dagpenge formue og skat/;
data ind02;
set ft02.ind (keep= id_nr formrest_ny05 loenmv_13 brutto dagpenge_kontant_13 loenskpl sluskat QSPLINDK fradrag);
UnemB  = dagpenge_kontant_13;
Tax    = sluskat;
wage = loenmv_13;
Wealth = formrest_ny05;
taxinc = QSPLINDK ;
deductions = fradrag;
grossinc = brutto;
drop dagpenge_kontant_13 brutto formrest_ny05 loenskpl sluskat QSPLINDK fradrag;
run;

*/ Arbejdsmarkedsforhold ;
data arb02; set ft02.akm (keep = id_nr beskst13);
typejob = beskst13;
drop beskst13;
run;

*/Uddannelse;
data udd02; set ft02.udda (keep=id_nr hfaudd);
edu_full=put(hfaudd,AUDD2015_L1L5_KT.);
edu_segment = put(edu_full,audd.);
drop hfaudd;
run;

data ida2;
set ft02.idap (keep= senstar senafar id_nr akasaf );
lastyearins = akasaf;
laststart = senstar;
lastend = senafar;
drop akasaf senstar senafar;
run;

Proc Sort data = ida2;
by ID_nr;
run;
Proc Sort data = udd02;
by ID_nr;
run;
/* Merge*/
Proc Sort data = exp02;
	by ID_nr;
run;
Proc Sort data = ay02;
	by ID_nr;
run;
Proc Sort data = ind02;
	by ID_nr;
run;
Proc Sort data = arb02;
	by ID_nr;
run;
Proc Sort data = dag02;
	by ID_nr;
run;

data final02;
	Merge exp02(IN=EX) ay02 (IN=AY) ind02(IN=IND) arb02(IN = ARB) udd02 (in=udd) ida2(in=ida) dag02; 
	by ID_nr;
	if EX and AY and IND and ARB and udd and ida;
Run;

/********/
/* 2003 */
/********/
/* Erfaring 2003*/
proc sql;
create table exp03 as
select distinct ID_nr, /*FULDDEL, 
		sum(monthsjob) as tot_month_job,;
		sum(SFTJ) as tot_fort_job,*/
		sum(TIMNORM) as tot_norm,
		sum(TIMPRAE) as tot_time_job
from ft03.lon
group by ID_nr
order by ID_nr;
quit;

data exp03;
set exp03;
if tot_time_job > 1740 then experience = 1; 
else if 1740 > tot_time_job > 1500 then experience =(750 + (tot_time_job - 1500)*((1000-750)/(1740-1500))) / 1000; /*indekserer deltid*/
else if 1501 > tot_time_job then experience  = tot_time_job * 0.5 /1000;
run; 

*/Alder og årstal 2003;
data ay03;
set ft03.bef (keep= id_nr alder);
age = alder;
year = 2003;
if alder > 17;
if alder <71;
drop alder;
run;

*/ Løn dagpenge formue og skat/;
data ind03;
set ft03.ind (keep= id_nr formrest_ny05 brutto loenmv_13 dagpenge_kontant_13 loenskpl sluskat QSPLINDK fradrag);
UnemB  = dagpenge_kontant_13;
Tax    = sluskat;
wage = loenmv_13;
Wealth = formrest_ny05;
taxinc = QSPLINDK ;
deductions = fradrag;
grossinc = brutto;
drop dagpenge_kontant_13 brutto formrest_ny05 loenskpl sluskat QSPLINDK fradrag;
run;

*/ Arbejdsmarkedsforhold ;
data arb03; set ft03.akm (keep = id_nr beskst13);
typejob = beskst13;
drop beskst13;
run;

*/Uddannelse;
data udd03; set ft03.udda (keep=id_nr hfaudd);
edu_full=put(hfaudd,AUDD2015_L1L5_KT.);
edu_segment = put(edu_full,audd.);
drop hfaudd;
run;

data ida3;
set ft03.idap (keep= senstar senafar id_nr akasaf );
lastyearins = akasaf;
laststart = senstar;
lastend = senafar;
drop akasaf senstar senafar;
run;

proc sql;
create table dag03 as
select distinct ID_nr, 
		FORSIKRINGSKATEGORI_KODE as ui_code,
		akasse 		as ui_ins,
		sum(dpialt) as UI,
		ledars 		as termins 	
from ft03.dur
group by ID_nr
order by ID_nr;
quit;

data dag03; set dag03;
term_ins = input(termins,8.);
drop termins;
run;


Proc Sort data = dag03;
by ID_nr;
run;

Proc Sort data = ida3;
by ID_nr;
run;
Proc Sort data = udd03;
by ID_nr;
run;
/* Merge*/
Proc Sort data = exp03;
	by ID_nr;
run;
Proc Sort data = ay03;
	by ID_nr;
run;
Proc Sort data = ind03;
	by ID_nr;
run;
Proc Sort data = arb03;
	by ID_nr;
run;

data final03;
	Merge exp03(IN=EX) ay03 (IN=AY) ind03(IN=IND) arb03(IN = ARB) udd03 (in=udd) ida3(in=ida) dag03; 
	by ID_nr;
	if EX and AY and IND and ARB and udd and ida;
Run;

/********/
/* 2004 */
/********/
/* Erfaring 2004*/
proc sql;
create table exp04 as
select distinct ID_nr, /*FULDDEL, 
		sum(monthsjob) as tot_month_job,;
		sum(SFTJ) as tot_fort_job,*/
		sum(TIMNORM) as tot_norm,
		sum(TIMPRAE) as tot_time_job
from ft04.lon
group by ID_nr
order by ID_nr;
quit;

data exp04;
set exp04;
if tot_time_job > 1740 then experience = 1; 
else if 1740 > tot_time_job > 1500 then experience =(750 + (tot_time_job - 1500)*((1000-750)/(1740-1500))) / 1000; /*indekserer deltid*/
else if 1501 > tot_time_job then experience  = tot_time_job * 0.5 /1000;
run; 

*/Alder og årstal 2004;
data ay04;
set ft04.bef (keep= id_nr alder);
age = alder;
year = 2004;
if alder > 17;
if alder <71;
drop alder;
run;

*/ Løn dagpenge formue og skat/;
data ind04;
set ft04.ind (keep= id_nr formrest_ny05 brutto loenmv_13 dagpenge_kontant_13 loenskpl sluskat QSPLINDK fradrag);
UnemB  = dagpenge_kontant_13;
Tax    = sluskat;
wage = loenmv_13;
Wealth = formrest_ny05;
taxinc = QSPLINDK ;
deductions = fradrag;
grossinc = brutto;
drop dagpenge_kontant_13 brutto formrest_ny05 loenskpl sluskat QSPLINDK fradrag;
run;


*/ Arbejdsmarkedsforhold ;
data arb04; set ft04.akm (keep = id_nr beskst13);
typejob = beskst13;
drop beskst13;
run;

*/Uddannelse;
data udd04; set ft04.udda (keep=id_nr hfaudd);
edu_full=put(hfaudd,AUDD2015_L1L5_KT.);
edu_segment = put(edu_full,audd.);
drop hfaudd;
run;

data ida4;
set ft04.idap (keep= senstar senafar id_nr akasaf );
lastyearins = akasaf;
laststart = senstar;
lastend = senafar;
drop akasaf senstar senafar;
run;

proc sql;
create table dag04 as
select distinct ID_nr, 
		FORSIKRINGSKATEGORI_KODE as ui_code,
		akasse 		as ui_ins,
		sum(dpialt) as UI,
		ledars 		as termins 	
from ft04.dur
group by ID_nr
order by ID_nr;
quit;

data dag04; set dag04;
term_ins = input(termins,8.);
drop termins;
run;


Proc Sort data = dag04;
by ID_nr;
run;
Proc Sort data = ida4;
by ID_nr;
run;
Proc Sort data = udd04;
by ID_nr;
run;
/* Merge*/
Proc Sort data = exp04;
	by ID_nr;
run;
Proc Sort data = ay04;
	by ID_nr;
run;
Proc Sort data = ind04;
	by ID_nr;
run;
Proc Sort data = arb04;
	by ID_nr;
run;

data final04;
	Merge exp04(IN=EX) ay04 (IN=AY) ind04(IN=IND) arb04(IN = ARB) udd04 (in=udd) ida4(in=ida); 
	by ID_nr;
	if EX and AY and IND and ARB and udd and ida;
Run;

/********/
/* 2005 */
/********/
/* Erfaring 2005*/
proc sql;
create table exp05 as
select distinct ID_nr, /*FULDDEL, 
		sum(monthsjob) as tot_month_job,;
		sum(SFTJ) as tot_fort_job,*/
		sum(TIMNORM) as tot_norm,
		sum(TIMPRAE) as tot_time_job
from ft05.lon
group by ID_nr
order by ID_nr;
quit;

data exp05;
set exp05;
if tot_time_job > 1740 then experience = 1; 
else if 1740 > tot_time_job > 1500 then experience =(750 + (tot_time_job - 1500)*((1000-750)/(1740-1500))) / 1000; /*indekserer deltid*/
else if 1501 > tot_time_job then experience  = tot_time_job * 0.5 /1000;
run; 

*/Alder og årstal 2005;
data ay05;
set ft05.bef (keep= id_nr alder);
age = alder;
year = 2005;
if alder > 18;
if alder <70;
drop alder;
run;

*/ Løn dagpenge formue og skat;
data ind05;
set ft05.ind (keep= id_nr brutto formrest_ny05 loenmv_13 dagpenge_kontant_13 loenskpl sluskat QSPLINDK fradrag);
UnemB = dagpenge_kontant_13;
Tax = sluskat;
wage = loenmv_13;
wealth = formrest_ny05;
taxinc = QSPLINDK ;
deductions = fradrag;
grossinc = brutto;
drop dagpenge_kontant_13 brutto formrest_ny05 loenskpl sluskat QSPLINDK fradrag;
run;


*/ Arbejdsmarkedsforhold ;
data arb05; set ft05.akm (keep = id_nr beskst13);
typejob = beskst13;
run;

*/Uddannelse;
data udd05; set ft05.udda (keep=id_nr hfaudd);
edu_full=put(hfaudd,AUDD2015_L1L5_KT.);
edu_segment = put(edu_full,audd.);
drop hfaudd;
run;

data ida5;
set ft05.idap (keep= senstar senafar id_nr akasaf );
lastyearins = akasaf;
laststart = senstar;
lastend = senafar;
drop akasaf senstar senafar;
run;

proc sql;
create table dag05 as
select distinct ID_nr, 
		FORSIKRINGSKATEGORI_KODE as ui_code,
		akasse 		as ui_ins,
		sum(dpialt) as UI,
		ledars 		as termins 	
from ft05.dur
group by ID_nr
order by ID_nr;
quit;

data dag05; set dag05;
term_ins = input(termins,8.);
drop termins;
run;



Proc Sort data = dag05;
by ID_nr;
run;

Proc Sort data = ida5;
by ID_nr;
run;

Proc Sort data = udd05;
by ID_nr;
run;

/* Merge*/
Proc Sort data = exp05;
by ID_nr;
run;
Proc Sort data = ay05;
by ID_nr;
run;
Proc Sort data = ind05;
by ID_nr;
run;
Proc Sort data = arb05;
by ID_nr;
run;

data final05;
Merge exp05(IN=EX) ay05 (IN=AY) ind05 (IN=IND) arb05(IN=ARB) udd05(in = udd) ida5(in=ida) dag05;
By ID_nr;
if EX and AY and IND and ARB and udd and ida;
Run;

/********/
/* 2006 */
/********/
/* Erfaring 2006*/
proc sql;
create table exp06 as
select distinct ID_nr, /*FULDDEL, 
		sum(monthsjob) as tot_month_job,;
		sum(SFTJ) as tot_fort_job,*/
		sum(TIMNORM) as tot_norm,
		sum(TIMPRAE) as tot_time_job
from ft06.lon
group by ID_nr
order by ID_nr;
quit;

data exp06;
set exp06;
if tot_time_job > 1740 then experience = 1; 
else if 1740 > tot_time_job > 1500 then experience =(750 + (tot_time_job - 1500)*((1000-750)/(1740-1500))) / 1000; /*indekserer deltid*/
else if 1501 > tot_time_job then experience  = tot_time_job * 0.5 /1000;
run; 

*/Alder og årstal 2006;
data ay06;
set ft06.bef (keep= id_nr alder);
age = alder;
year = 2006;
if alder > 17;
if alder <71;
drop alder;
run;

*/ Løn dagpenge formue og skat;
data ind06;
set ft06.ind (keep= id_nr brutto loenmv_13 formrest_ny05 dagpenge_kontant_13 loenskpl sluskat QSPLINDK fradrag);
UnemB = dagpenge_kontant_13;
Tax = sluskat;
wage = loenmv_13;
wealth = formrest_ny05;
taxinc = QSPLINDK ;
deductions = fradrag;
grossinc = brutto;
drop dagpenge_kontant_13 brutto formrest_ny05 loenskpl sluskat QSPLINDK fradrag;
run;

*/ Arbejdsmarkedsforhold ;
data arb06; set ft06.akm (keep = id_nr beskst13);
typejob = beskst13;
run;

*/Uddannelse;
data udd06; set ft06.udda (keep=id_nr hfaudd);
edu_full=put(hfaudd,AUDD2015_L1L5_KT.);
edu_segment = put(edu_full,audd.);
drop hfaudd;
run;
Proc Sort data = udd06;
by ID_nr;
run;

data ida6;
set ft06.idap (keep= senstar senafar id_nr akasaf );
lastyearins = akasaf;
laststart = senstar;
lastend = senafar;
drop akasaf senstar senafar;
run;

proc sql;
create table dag06 as
select distinct ID_nr, 
		FORSIKRINGSKATEGORI_KODE as ui_code,
		akasse 		as ui_ins,
		sum(dpialt) as UI,
		ledars 		as termins 	
from ft06.dur
group by ID_nr
order by ID_nr;
quit;

data dag06; set dag06;
term_ins = input(termins,8.);
drop termins;
run;



Proc Sort data = dag06;
by ID_nr;
run;

Proc Sort data = ida6;
by ID_nr;
run;

/* Merge*/
Proc Sort data = exp06;
by ID_nr;
run;
Proc Sort data = ay06;
by ID_nr;
run;
Proc Sort data = ind06;
by ID_nr;
run;
Proc Sort data = arb06;
	by ID_nr;
run;

data final06;
Merge exp06(IN=EX) ay06 (IN=AY) ind06 (IN=IND) arb06(IN=ARB) udd06 (in=udd) ida6(in=ida) dag06;
By ID_nr;
if EX and AY and IND and ARB and udd and ida;
Run;

/********/
/* 2007 */
/********/
/* Erfaring 2007*/
proc sql;
create table exp07 as
select distinct ID_nr, /*FULDDEL, 
		sum(monthsjob) as tot_month_job,;
		sum(SFTJ) as tot_fort_job,*/
		sum(TIMNORM) as tot_norm,
		sum(TIMPRAE) as tot_time_job
from ft07.lon
group by ID_nr
order by ID_nr;
quit;
data exp07;
set exp07;
if tot_time_job > 1740 then experience = 1; 
else if 1740 > tot_time_job > 1500 then experience =(750 + (tot_time_job - 1500)*((1000-750)/(1740-1500))) / 1000; /*indekserer deltid*/
else if 1501 > tot_time_job then experience  = tot_time_job * 0.5 /1000;
run; 

*/Alder og årstal 2007;
data ay07;
set ft07.bef (keep= id_nr alder);
age = alder;
year = 2007;
if alder > 17;
if alder <71;
drop alder;
run;

*/ Løn dagpenge formue og skat;
data ind07;
set ft07.ind (keep= id_nr brutto formrest_ny05 loenmv_13 dagpenge_kontant_13 loenskpl sluskat QSPLINDK fradrag);
UnemB = dagpenge_kontant_13;
Tax = sluskat;
wage = loenmv_13;
wealth = formrest_ny05;
taxinc = QSPLINDK ;
deductions = fradrag;
grossinc = brutto;
drop dagpenge_kontant_13 brutto formrest_ny05 loenskpl sluskat QSPLINDK fradrag;
run;


*/ Arbejdsmarkedsforhold ;
data arb07; set ft07.akm (keep = id_nr beskst13);
typejob = beskst13;
drop beskst13;
run;

*/Uddannelse;
data udd07; set ft07.udda (keep=id_nr hfaudd);
edu_full=put(hfaudd,AUDD2015_L1L5_KT.);
edu_segment = put(edu_full,audd.);
drop hfaudd;
run;

data ida7;
set ft07.idap (keep= senstar senafar id_nr akasaf );
lastyearins = akasaf;
laststart = senstar;
lastend = senafar;
drop akasaf senstar senafar;
run;

proc sql;
create table dag07 as
select distinct ID_nr, 
		FORSIKRINGSKATEGORI_KODE as ui_code,
		akasse 		as ui_ins,
		sum(dpialt) as UI,
		ledars 		as termins 	
from ft07.dur
group by ID_nr
order by ID_nr;
quit;

data dag07; set dag07;
term_ins = input(termins,8.);
drop termins;
run;


Proc Sort data = dag07;
by ID_nr;
run;

Proc Sort data = ida7;
by ID_nr;
run;

Proc Sort data = udd07;
by ID_nr;
run;

/* Merge*/
Proc Sort data = exp07;
by ID_nr;
run;
Proc Sort data = ay07;
by ID_nr;
run;
Proc Sort data = ind07;
by ID_nr;
run;
Proc Sort data = arb07;
	by ID_nr;
run;

data final07;
Merge exp07(IN=EX) ay07 (IN=AY) ind07 (IN = IND) arb07 (IN = ARB) udd07 (in=udd) ida7(in=ida) dag07;
By ID_nr;
if EX and AY and IND and ARB and udd and ida;
Run;

/********/
/* 2008 */
/********/
/* Erfaring 2008*/
proc sql;
create table exp08 as
select distinct ID_nr, /*FULDDEL, 
		sum(monthsjob) as tot_month_job,;
		sum(SFTJ) as tot_fort_job,*/
		sum(TIMNORM) as tot_norm,
		sum(TIMPRAE) as tot_time_job
from ft08.lon
group by ID_nr
order by ID_nr;
quit;
data exp08;
set exp08;
if tot_time_job > 1740 then experience = 1; 
else if 1740 > tot_time_job > 1500 then experience =(750 + (tot_time_job - 1500)*((1000-750)/(1740-1500))) / 1000; /*indekserer deltid*/
else if 1501 > tot_time_job then experience  = tot_time_job * 0.5 /1000;
run; 

*/Alder og årstal 2008;
data ay08;
set ft08.bef (keep= id_nr alder);
age = alder;
year = 2008;
if alder > 17;
if alder <71;
drop alder;
run;

*/ Løn dagpenge formue og skat;
data ind08;
set ft08.ind (keep= id_nr brutto formrest_ny05 dagpenge_kontant_13 loenskpl loenmv_13 sluskat QSPLINDK fradrag);
UnemB = dagpenge_kontant_13;
Tax = sluskat;
wage = loenmv_13;
wealth = formrest_ny05;
taxinc = QSPLINDK ;
deductions = fradrag;
grossinc = brutto;
drop dagpenge_kontant_13 brutto formrest_ny05 loenskpl sluskat QSPLINDK fradrag;
run;

*/ Uddannelse ; 

data udd08; set ft08.udda (keep=id_nr hfaudd);
edu_full=put(hfaudd,AUDD2015_L1L5_KT.);
edu_segment = put(edu_full,audd.);
drop hfaudd;
run;
Proc Sort data = udd08;
by ID_nr;
run;



*/ Arbejdsmarkedsforhold ;
data arb08; set ft08.akm (keep = id_nr beskst13);
	typejob = beskst13;
	drop beskst13;
run;

data ida8;
set ft08.idap (keep= senstar senafar id_nr akasaf );
lastyearins = akasaf;
laststart = senstar;
lastend = senafar;
drop akasaf senstar senafar;
run;

proc sql;
create table dag08 as
select distinct ID_nr, 
		FORSIKRINGSKATEGORI_KODE as ui_code,
		akasse 		as ui_ins,
		sum(dpialt) as UI,
		ledars 		as termins 	
from ft08.dur
group by ID_nr
order by ID_nr;
quit;

data dag08; set dag08;
term_ins = input(termins,8.);
drop termins;
run;


Proc Sort data = dag08;
by ID_nr;
run;

Proc Sort data = ida8;
by ID_nr;
run;

/* Merge*/
Proc Sort data = exp08;
by ID_nr;
run;
Proc Sort data = ay08;
by ID_nr;
run;
Proc Sort data = ind08;
by ID_nr;
run;
Proc Sort data = arb08;
	by ID_nr;
run;
Proc Sort data = udd08;
by ID_nr;
run;

data final08;
Merge exp08(IN=EX) ay08 (IN=AY) ind08 (IN = IND) arb08 (IN=ARB) udd08 (IN=UDD) ida8(in=ida) dag08;
By ID_nr;
if EX and ida and AY and IND and UDD and ARB;
Run;

/********/
/* 2009 */
/********/
/* Erfaring 2009*/
proc sql;
create table exp09 as
select distinct ID_nr, /*FULDDEL, 
		sum(monthsjob) as tot_month_job,;
		sum(SFTJ) as tot_fort_job,*/
		sum(TIMNORM) as tot_norm,
		sum(TIMPRAE) as tot_time_job
from ft09.lon
group by ID_nr
order by ID_nr;
quit;
data exp09;
set exp09;
if tot_time_job > 1740 then experience = 1; 
else if 1740 > tot_time_job > 1500 then experience =(750 + (tot_time_job - 1500)*((1000-750)/(1740-1500))) / 1000; /*indekserer deltid*/
else if 1501 > tot_time_job then experience  = tot_time_job * 0.5 /1000;
run; 

*/Alder og årstal 2009;
data ay09;
set ft09.bef (keep= id_nr alder);
age = alder;
year = 2009;
if alder > 17;
if alder <71;
drop alder;
run;

*/ Løn dagpenge formue og skat;
data ind09;
set ft09.ind (keep= id_nr brutto formrest_ny05 dagpenge_kontant_13 loenskpl loenmv_13 sluskat QSPLINDK fradrag);
UnemB = dagpenge_kontant_13;
Tax = sluskat;
wage = loenmv_13;
wealth = formrest_ny05;
taxinc = QSPLINDK ;
deductions = fradrag;
grossinc = brutto;
drop dagpenge_kontant_13 brutto formrest_ny05 loenskpl sluskat QSPLINDK fradrag;
run;

*/ Uddannelse ; 

data udd09; set ft09.udda (keep=id_nr hfaudd);
edu_full=put(hfaudd,AUDD2015_L1L5_KT.);
edu_segment = put(edu_full,audd.);
drop hfaudd;
run;
Proc Sort data = udd;
by ID_nr;
run;

*/ Arbejdsmarkedsforhold ;
data arb09; set ft09.akm (keep = id_nr beskst13);
	typejob = beskst13;
	drop beskst13;
run;

data ida9;
set ft09.idap (keep= senstar senafar id_nr akasaf );
lastyearins = akasaf;
laststart = senstar;
lastend = senafar;
drop akasaf senstar senafar;
run;
proc sql;
create table dag09 as
select distinct ID_nr, 
		FORSIKRINGSKATEGORI_KODE as ui_code,
		akasse 		as ui_ins,
		sum(dpialt) as UI,
		ledars 		as termins 	
from ft09dur
group by ID_nr
order by ID_nr;
quit;

data dag09; set dag09
term_ins = input(termins,8.);
drop termins;
run;

Proc Sort data = dag09;
by ID_nr;
run;

Proc Sort data = ida9;
by ID_nr;
run;

/* Merge*/
Proc Sort data = exp09;
by ID_nr;
run;
Proc Sort data = ay09;
by ID_nr;
run;
Proc Sort data = ind09;
by ID_nr;
run;
Proc Sort data = arb09;
	by ID_nr;
run;
Proc Sort data = udd09;
by ID_nr;
run;

data final09;
Merge exp09(IN=EX) ay09 (IN=AY) ind09 (IN = IND) arb09 (IN=ARB) udd09 (IN=UDD) ida9(in=ida) dag09;
By ID_nr;
if EX and ida and AY and IND and UDD and ARB;
Run;

data final;
set 
final01
final02
final03
final04
final05
final06
final07
final08
final09;
run;

Proc Sort data = final;
by ID_nr;
run;


* Out;
* data andla.data; *set final;
* run;

%ds2csv (
	data = final,
	runmode = b,
	csvfile = D:\FM\andla\Data\datafinal.csv
	);

proc sql;
create table pens14 as
select distinct ID_nr, 
		sum(PENSDEPOTBLB) as tot_pens
from ft14.pensform
group by ID_nr
order by ID_nr;
quit;

data exp14;
set ft14.idap (keep=erhver id_nr aldernov erhver79 akasse_id akasaf);
array change _numeric_;
	do over change;
		if change=. then change=0;
	end;
if aldernov > 063;
if aldernov < 068;
if pstill ^= 93;
insurance = akasse_id;
lastyearins = akasaf;
erhvår = erhver / 1000;
age = input(aldernov,best12.);
exp_tot = erhvår + erhver79;
year = 2014;
drop erhver akasse_id erhver79 erhvår akasaf aldernov;
run;

proc sort data = exp14;
by ID_nr;
run;

data ind14;
set ft14.ind (keep= id_nr formrest_ny05);
wealth = formrest_ny05;
drop formrest_ny05;
run;

proc sort data = ind14;
by ID_nr;
run;

proc sql;
create table pens15 as
select distinct ID_nr, 
		sum(PENSDEPOTBLB) as tot_pens
from ft15.pensform
group by ID_nr
order by ID_nr;
quit;


data exp15;
set ft15.idap (keep=erhver id_nr aldernov erhver79 akasse_id akasaf);
array change _numeric_;
	do over change;
		if change=. then change=0;
	end;
if aldernov > 063;
if aldernov < 068;
if pstill ^= 93;
lastyearins = akasaf;
insurance = akasse_id;
erhvår = erhver / 1000;
age = input(aldernov,best12.);
exp_tot = erhvår + erhver79;
year = 2015;
drop erhver akasse_id erhver79 erhvår akasaf aldernov;
run;


proc sort data = exp15;
by ID_nr;
run;


data ind15;
set ft15.ind (keep= id_nr formrest_ny05);
wealth = formrest_ny05;
drop formrest_ny05;
run;

proc sort data = ind15;
by ID_nr;
run;


data pens014;
Merge exp14(IN=EX) pens14(in=pen) ind14(in=IND);
By ID_nr;
if EX and pen and IND;
Run;

data pens015;
Merge exp15(IN=EX) ind15(in =IND) pens15(in=pen);
By ID_nr;
if EX and pen and ind15;
Run;


data pens;
set 
pens014
pens015;
run;


%ds2csv (
	data = pens,
	runmode = b,
	csvfile = D:\FM\andla\Data\datapens.csv
	);
