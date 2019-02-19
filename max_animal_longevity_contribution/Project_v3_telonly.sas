	/*folder must exist locally*/
	LIBNAME Project "/folders/myfolders/Project"; 
	
	PROC IMPORT OUT= Project.ANIMAL_NOMTDNA(KEEP= Kingdom Phylum Class Order Family Genus Species Common_name Adult_weight__g_ Maximum_longevity__yrs_ Sample_size Data_quality Temperature__K_ Telomere_Length__KB_ Metabolic_rate__W_ G_per_1_kb T_per_1_kb C_per_1_kb A_per_1_kb) 
	 
	  datafile = '/folders/myfolders/Project/AnAge.xlsx'
	  DBMS=XLSX REPLACE ;
	  SHEET='Combined';
	  GETNAMES=YES;	  
	  
	RUN; 	
	
	DATA Project.ANIMAL_NOMTDNA1 (RENAME=(Temperature__K_=T Telomere_Length__KB_=Tel G_per_1_kb=G_CON C_per_1_kb=C_CON A_per_1_kb=A_CON T_per_1_kb=T_CON) );
	    set Project.ANIMAL_NOMTDNA;
	    
	    /*Delete observations with questionable data quality*/
	    if Data_quality = "questionable" then delete;   
	    
	    /*Transform variables*/
	    Log_AWeight = log(Adult_weight__g_);
	    Log_MRate = log(Metabolic_rate__W_);
	    Log_Max_longevity = log(Maximum_longevity__yrs_);
	    
	    /*exclude observations for which any variable is missing*/
	    if nmiss(of _numeric_) + cmiss(of _character_) > 0 then delete;
	    
	    /*Drop raw variables no longer needed*/
	    DROP Adult_weight__g_ /*Metabolic_rate__W_ Maximum_longevity__yrs_*/;   
	    	    
	    /*Change labels to shorter ones*/
	   label Telomere_Length__KB_='Tel' Temperature__K_='Temp' G_per_1_kb='G_CON' C_per_1_kb='C_CON' T_per_1_kb='T_CON' A_per_1_kb = 'A_CON' Log_AWeight='LN_WT' Log_MRate='LN_MRate' Log_Max_longevity='LN_MX_L';
	   
	RUN;
	
	/*standardize the data*/
	proc standard data=Project.ANIMAL_NOMTDNA1 out=Project.ANIMAL_NOMTDNA1_std mean=0 std=1;
	var T Tel G_CON C_CON T_CON A_CON  Log_AWeight Log_MRate Log_Max_longevity;
	run;
	
	
	proc cluster data=Project.ANIMAL_NOMTDNA1_std outtree=Project.tree_ANIMAL_NOMTDNA1 method=average nonorm;
	var  T Tel /*G_CON C_CON T_CON A_CON  Log_AWeight Log_MRate Log_Max_longevity*/;
	id Common_name;
	copy Kingdom Phylum Class Order Family Genus Species;
	run;
	
	proc tree data=Project.ANIMAL_NOMTDNA1_std NCLUSTERS=2 out=Project.tree_ANIMAL_NOMTDNA1_tree;
	id Common_name;	
	copy Kingdom Phylum Class Order Family Genus Species;
	run;
	
	proc fastclus data=univesity_std radius=1.5 maxc=3 replace=none maxiter=10 out=Clus_out;
	var X1-X6;
	id University;
	run;

	proc sort data=Clus_out_c ;
	by cluster distance;
	run;
	
	proc candisc data=Clus_out_c noprint out=ProCan_c(keep=University cluster Can1 Can2);
	class cluster;
	var X1-X6;
	run;
	
	
	proc sgplot data=ProCan_c;  
	  title 'FIRST TWO DISCRIMINANT FUNCTIONS';
	  SCATTER y= Can2 x= Can1 / DATALABEL= university group=cluster;  
	  xaxis label="z1";
	  yaxis label="z2";
	run;quit;
	
	
	
	