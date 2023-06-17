PEW RESEARCH CENTER FOR THE PEOPLE & THE PRESS
MID-OCTOBER POLITICAL SURVEY
October 16-19, 2008
N=3,016

This dataset includes cell phone interviews conducted using an RDD sample of cell phone numbers. 
Cell phone interviews include households that are cell-only as well as those that also contain a landline phone. 

The dataset contains three weight variables, which can be used to compare the combined landline and 
cell phone sample with the landline sample by itself and with the landline sample combined with the cell-only households.

Weight Name: WEIGHT
Description: This is the weight for the combined sample of all landline and
cell phone interviews. This weight is used for all analysis in the 9-18 and 9-24 reports.

Weight Name: LLWEIGHT
Description: This weight is applied to the landline RDD sample only. Interviews
conducted by cellphone are not given a weight and are excluded
from analysis when this weight is used.

Weight Name: COWEIGHT
Description: This is the weight for the Landline RDD sample and the 
cell-only cases combined. Cases from the Cell Phone RDD sample that 
reported having a landline phone are not given a weight and are excluded from
analysis when this weight is used.

Two additional weights related to likely voters (lvwt67 and unlvwt67) are described below.

*********************************************************************************************

The racecmb variable was computed using the following syntax:

recode race1 (1=1) (2=2) (3=3) (4=5) (9=9) into racecmb.
if race2>0 racecmb=4.
var label racecmb "Combining Race".
value label RACEcmb
1 "White"
2 "Black or African-American"
3 "Asian or Asian-American"
4 "Mixed Race"
5 "Or some other race"
9 "Don't know/Refused (VOL.)".

 
*********************************************************************************************
The SWING variable was computed using the following syntax:
Swing Voter Syntax 

if (q3=1) and (q6>1) swing=1.
if (q3=2) and (q5>1) swing=2.
if (q3a=1 or (q3=1 and q6=1)) swing=3.
if (q3a=2 or (q3=2 and q5=1)) swing=4.
if (q3a>2) swing=5.
var lab swing '2008 Vote Preference'.
val lab swing 1 'MCCAIN �no chance Obama'
              2 'OBAMA - no chance McCain'
              3 'MCCAIN - Lean McCain Chance Obama'
              4 'OBAMA - Lean Obama Chance McCain'
              5 'UNDECIDED'.


*********************************************************************************************

The horserace variable (Q3horse) was computed using the following syntax:

Q3Horse

If (q3=1 or q3a=1) q3horse=1
If (q3=2 or q3a=2) q3horse=2
If q3a=3 q3horse=3
If q3a=9 q3horse=9
Val lab q3horse �Horserace�
		  1 'McCain/lean McCain'
              2 'Obama/lean Obama'
              3 'Other-refused to lean'
              5 'DK-refused to lean�.

*********************************************************************************************

The BATTLE variable was computed using the following syntax:


recode state (1,2,4,5,13,16,20,21,22,28,30,31,38,40,45,46,47,48,49,56,54=1)
(6,9,10,11,15,17,23,24,25,34,36,41,44,50,53=2) (8,12,18,19,26,27,29,32,33,35,37,39,42,55,51=3) into battle.
var label battle 'Red, Blue & Battle'.
val label battle 1'Republican states' 2 'Democratic states' 3 'Battleground states'.

*********************************************************************************************

The likely voter scale is computed using the following variables:

****** LVS variables ***

recode thought (1 thru 2=1) (else=0) into lvs1.
recode oftvote (1 thru 2=1) (else=0) into lvs2.
recode pvote04a (1=1) (else=0) into lvs3.
recode precinct (1=1) (else=0) into lvs4.
recode scale10 (9 thru 10=1) (else=0) into lvs5.
recode q2 (1 thru 2=1) (else=0) into lvs6.
recode planto2 (1=1) (else=0) into lvs7.
compute lvstemp=lvs1+lvs2+lvs3+lvs4+lvs5+lvs6+lvs7.

LVS is then adjusted, as follows:

COMPUTE lvs=lvstemp.
if racecmb=2 lvs=lvstemp+.5.
if age<20 lvs=lvstemp+1.5.
if age ge 20 and age le 25 lvs=lvstemp+1.
if age ge 26 and age < 30 lvs=lvstemp+.5.
if regicert > 1 lvs=0.
if regist > 1 lvs=0.
if planto1=2 lvs=0.
if planto1=3 lvs=7.
if planto3=3 lvs=7.
if lvs > 7 lvs=7.
exe.
weight by weight.
fre lvs.
var label lvs '7-point likely voter scale'.

*LIKELY VOTER WEIGHT*

A weight for likely Voters is computed based on the likely voter scale 
(predicting 62% turnout--using a 67% cutoff for the scale).

**This weight (lvswt67) should be used when analyzing likely voters**.

if (lvs<5) lvswt67=0.
if (lvs=5) lvswt67=weight*.1091.
if (lvs>5) lvswt67=weight.
weight by lvswt67.
var label lvswt67 'weight for likely voter scale 67% turnout cutoff'.

*UNLIKELY VOTER WEIGHT*

This unlikely voter weight (unlvwt67) is only used in comparison with likely voters.
(this includes all those not considered likely voters--including non registered).

if (lvs>5) unlvwt67=0.
if (lvs=5) unlvwt67=weight*.8909.
if (lvs<5) unlvwt67=weight.
weight by unlvwt67.
var label unlvwt67 'weight for unlikely voters'.


For additional information on Pew's likely voter scale, see:

http://people-press.org/methodology/election/#1



