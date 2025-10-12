/*Dataset: Cake (from PROC MEANS examples)

Create a permanent library named ck.
Use the dataset "cake" found in the PROC MEANS examples.
How many chocolate cakes were presented during the competition?
Find the highest taste score for each flavor and save it in a new dataset.
Add the highest taste score for each cake flavor to the original cake dataset.
Create a numeric variable (0 or 1) in the new dataset to identify the winner for each flavor based on taste score.
Transpose the variables presentscore and tastescore (make sure the identifier used is unique).*/

libname b 'C:\Users\eleonora.bonafe2\Desktop\sas';

data b.cake;
   input LastName $ 1-12 Age 13-14 PresentScore 16-17
         TasteScore 19-20 Flavor $ 23-32 Layers 34 ;
   datalines;
Orlando     27 93 80  Vanilla    1
Ramey       32 84 72  Rum        2
Goldston    46 68 75  Vanilla    1
Roe         38 79 73  Vanilla    2
Larsen      23 77 84  Chocolate  .
Davis       51 86 91  Spice      3
Strickland  19 82 79  Chocolate  1
Nguyen      57 77 84  Vanilla    .
Hildenbrand 33 81 83  Chocolate  1
Byron       62 72 87  Vanilla    2
Sanders     26 56 79  Chocolate  1
Jaeger      43 66 74             1
Davis       28 69 75  Chocolate  2
Conrad      69 85 94  Vanilla    1
Walters     55 67 72  Chocolate  2
Rossburger  28 78 81  Spice      2
Matthew     42 81 92  Chocolate  2
Becker      36 62 83  Spice      2
Anderson    27 87 85  Chocolate  1
Merritt     62 73 84  Chocolate  1
;
run;
/*average age of competition*/
proc means data=c.cake; var age;
run;
/*How many chocolate cakes were presented during the competition?*/
proc freq data=c.cake;
where flavor="Chocolate";
table flavor;
run;

/*Find the highest taste and present score for each flavor 
and save it in a new dataset.*/
proc sort data=c.cake; by flavor;run;
proc means data=c.cake;
by flavor;
var tastescore presentscore; output out=c ;
run;

proc means data=c.cake;
by flavor;
var tastescore presentscore; output out=c1 max= ;
run;
/*Add the highest taste score for each 
cake flavor to the original cake dataset.*/

data c; set c;
where _STAT_ ="MAX";
RENAME TASTESCORE=TASTECORE_FL;
RENAME PRESENTSCORE=PRESENTSCORE_FL;
RUN;
PROC sort data=c; by flavor;run;
proc sort data=c.cake; by flavor; run;

data c.cake1 (drop= _type_ _freq_ _stat_); merge c.cake c;
by flavor;
run;
