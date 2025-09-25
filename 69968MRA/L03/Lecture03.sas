data wghtclub; 
   input idno 1-4 name $ 6-24 team $ strtwght endwght; 
   loss=strtwght-endwght; 
   cards; 
1023 David Shaw         red    189 165 
1049 Amelia Serrano     yellow 145 124 
1219 Alan Nance         red    210 192 
1246 Ravi Sinha         yellow 194 177 
1078 Ashley McKnight    red    127 118 
1221 Jim Brown          yellow 220   . 
1095 Susan Stewart      blue   135 127 
1157 Rose Collins       green  155 141 
1331 Jason Schock       blue   187 172 
1067 Kanoko Nagasaka    green  135 122 
1251 Richard Rose       blue   181 166 
1333 Li-Hwa Lee         green  141 129 
1192 Charlene Armstrong yellow 152 139 
1352 Bette Long         green  156 137 
1262 Yao Chen           blue   196 180 
1087 Kim Blackburn      red    148 135 
1124 Adrienne Fink      green  156 142 
1197 Lynne Overby       red    138 125 
1133 John VanMeter      blue   180 167 
1036 Becky Redding      green  135 123 
1057 Margie Vanhoy      yellow 146 132 
1328 Hisashi Ito        red    155 142 
1243 Deanna Hicks       blue   134 122 
1177 Holly Choate       red    141 130 
1259 Raoul Sanchez      green  189 172 
1017 Jennifer Brooks    blue   138 127 
1099 Asha Garg          yellow 148 132 
1329 Larry Goss         yellow 188 174 
; 
run; 

/*We need to have own physical library*/

libname c 'C:\Users\chiaki.nishihara\Desktop\SAS';

data c.wghtclub;
set wghtclub;
run;

/*proc contents in order to discover the variables store in your dataset*/

proc contents data=c.wghtclub; run;

/*show only who belong to the red team*/
proc print data=c.wghtclub;
where team="red";
run;

/*data set for each team*/
proc freq data=c.wghtclub;
table team;
run;

/*out= is indicating where you are saving your SAS file*/
proc freq data=c.wghtclub;
table team;
run;

proc freq data=c.wghtclub;
table team*loss /all out=a;
run;

data c.wghtclub1; set c.wghtclub;
max_wght= max(of strtwght endwght);
min_wght= min(of strtwght endwght);
mean_wght= mean(of strtwght endwght);
run;

proc sort data=c.wghtclub1;
by team;
run;

/*Check each team, average and var of loss. Then summarize your dataset*/
proc summary data=c.wghtclub1;
by team;
var loss; output out=b mean=mean_team;
run;

proc means data=c.wghtclub1;
by team;
var loss;
run;

proc sort data=b;
by team;
run;

proc sort data=c.wghtclub1;
by team;
run;

data c.wghtclub2; merge c.wghtclub1 b;
by team;
run;

data b (keep= team mean_team); set b;
run;
