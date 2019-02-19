/*folder must exist locally*/
LIBNAME Project "/folders/myfolders/Project";

%inc "/folders/myfolders//Project/clustergroups.sas";

/*SET UP DATA*/
/*removed met rate since it correlates highly with rate, t and a content since they correlate with g and c, and telomore length since it is only available for very few observations*/
PROC IMPORT OUT=Project.ANIMALALT(KEEP=Kingdom Phylum Class Order Family Genus 
		Species Common_name Adult_weight__g_ Maximum_longevity__yrs_ Sample_size 
		Data_quality Temperature__K_ G_per_1_kb C_per_1_kb) 
		datafile='/folders/myfolders/Project/AnAge.xlsx' DBMS=XLSX REPLACE;
	SHEET='Combined';
	GETNAMES=YES;
	
RUN;

DATA Project.ANIMALALT1 (RENAME=(Temperature__K_=T G_per_1_kb=G_CON 
		C_per_1_kb=C_CON) );
	set Project.ANIMALALT;
        
	/*Delete observations with questionable data quality*/
	if Data_quality="questionable" then
		delete;

	/*Transform variables*/
	Log_AWeight=log(Adult_weight__g_);
	Log_Max_longevity=log(Maximum_longevity__yrs_);

	/*exclude observations for which any variable is missing*/
	if nmiss(of _numeric_) + cmiss(of _character_) > 0 then
		delete;

	/*Drop raw variables no longer needed*/
	DROP Adult_weight__g_ Maximum_longevity__yrs_;

	/*Change labels to shorter ones*/
	label Temperature__K_='Temp' G_per_1_kb='G_CON' C_per_1_kb='C_CON' 
		T_per_1_kb='T_CON' A_per_1_kb='A_CON' Log_AWeight='LN_WT' 
		Log_Max_longevity='LN_MX_L';
RUN;


/*standardize the data*/
proc standard data=Project.ANIMALALT1 out=Project.ANIMALALT1_std mean=0 std=1;
	var T G_CON C_CON Log_AWeight Log_Max_longevity;
	
run;

/*visualize data*/
proc sgscatter data= Project.ANIMALALT1;
	matrix _numeric_ / diagonal=(kernel histogram) ellipse;
run;
	
proc princomp data=Project.ANIMALALT1 out=ProPC;
	var T G_CON C_CON Log_AWeight Log_Max_longevity;
run;

ods graphics on;

proc sgplot data=WORK.ProPC;
	SCATTER y=prin2 x=prin1 / DATALABEL=Common_name;

	/*SCATTER y= prin2 x= prin1 / markerchar=Class group=Class;*/
	/*SCATTER y= prin2 x= prin1 / markerchar=Order group=Order;*/
	xaxis label="z1";
	yaxis label="z2";
run;

quit;

/*Hierarchical clustering*/
/*  Use Single Linkage   */
/*horizontal option: not used plots=dendrogram(vertical height=ncl)*/
proc cluster data=Project.ANIMALALT1 method=single outtree=ProTreeSing;
	var T G_CON C_CON Log_AWeight Log_Max_longevity;
	id Common_name;
run;

/*  Use Complete Linkage   */
proc cluster data=Project.ANIMALALT1 method=complete outtree=ProTreeComp;
	var T G_CON C_CON Log_AWeight Log_Max_longevity;
	id Common_name;
run;

/*  Use Average Linkage   */
proc cluster data=Project.ANIMALALT1 method=average outtree=ProTreeAvg;
	var T G_CON C_CON Log_AWeight Log_Max_longevity;
	id Common_name;
run;
/*  Use Centroid Linkage   */
proc cluster data=Project.ANIMALALT1 method=centroid outtree=ProTree;
	var T G_CON C_CON Log_AWeight Log_Max_longevity;
	id Common_name;
run;
/*  Use Ward Linkage   */
proc cluster data=Project.ANIMALALT1 method=Ward outtree=ProTreeWard;
	var T G_CON C_CON Log_AWeight Log_Max_longevity;
	id Common_name;
run;



proc tree data=ProTree nclusters=5 out=newdata noprint;
	id Common_name;
	copy T G_CON C_CON Log_AWeight Log_Max_longevity;
run;

proc sort data=newdata;
	by cluster;
run;

proc print data=newdata;
	var Common_name cluster;
run;

proc means data=newdata;
	by cluster;
	output out=Seeds mean=T G_CON C_CON Log_AWeight Log_Max_longevity;
	var T G_CON C_CON Log_AWeight Log_Max_longevity;
run;

proc fastclus data=Project.ANIMALALT1 maxc=5 maxiter=50 seed=Seeds out=Clus_out;
	var T G_CON C_CON Log_AWeight Log_Max_longevity;
	id Common_name;
run;

proc sort data=Clus_out;
	by cluster distance;
run;

proc print data=Clus_out;
	var Common_name cluster distance;
run;

proc candisc data=Clus_out noprint out=ProCan(keep=Common_name cluster Can1 
		Can2);
	class cluster;
	var T G_CON C_CON Log_AWeight Log_Max_longevity;
run;

proc sgplot data=ProCan;
	SCATTER y=Can2 x=Can1 / DATALABEL=Common_name;

	/*SCATTER y= prin2 x= prin1 / markerchar=Class group=Class;*/
	/*SCATTER y= prin2 x= prin1 / markerchar=Order group=Order;*/
	xaxis label="z1";
	yaxis label="z2";
run;

quit;