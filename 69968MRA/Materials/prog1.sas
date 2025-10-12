*path for students;
libname c 'C:\Camillo_clabe';
*path for furio;
libname c '\\137.204.248.62\MaterialeDocente\Camillo_clabe';

proc cluster data=c.unico_1 method=single outtree=c.tree;
var d10_:;
id id;
run;
proc tree data=c.tree;
id id;
run;
quit;
proc cluster data=c.unico_1 method=ward outtree=c.tree_1;
var d10_:;
id id;
run;
proc tree data=c.tree_1;
id id;
run;

proc tree data=c.tree_1 noprint out=c.cluster nclusters=3;
id id;
run;

proc sort data=c.cluster; by id; run;
data c.unico_2; merge c.unico_1 c.cluster;
by id;
run;

proc freq data=c.unico_2;
table cluster;
run;

proc freq data=c.unico_2 ;
table cluster*sex / expected all;
*where cluster ne 3;
run;

proc means data=c.unico_2;
var d10_1-d10_8;
run;
proc means data=c.unico_2;
var d10_1-d10_8;
class cluster;
run;
