This is the replication archive for Simon Jackman and Bradley Spahn's paper in Political Analysis, "Why does the ANES Overestimate Voter turnout?"  If you download all of these files into a single directory and set that directory as your working directory in R, they should run without an issue.

Each file (except for useSampledExport.R, which is called by simonValidate.R) can be run independently. Collectively the files reproduce the tables and figure in the paper, as well as several in-text calculations. Results that are derived arithmetically from other results are not are usually not calculated explicitly in the files. To match the order they appear in the paper, the files should be run in the following order:

1) MatchTypeAnalysis.R (produced table 1; < 1 minute to run) 
2) simonValidate.R (produces table 2 & 3; < 1 minute to run)
3) noncomplier_graph_matchingmethod.R (produces figure 1; around 1 minute to run))
4) compute_itt.R (produces table 4; around 1 minute to run)

Note that for table 4, the returned p values are multiplied by 100 and should be adjusted manually so that they are on the 0 to 1 scale.

Table 5 is just a summary of in-paper results, and is thus not re-produced in this replication archive.

The following packages are required:
doParallel
dplyr
foreach
gdata
ggplot2
iterators
knitr
MASS
Matching
reshape2
scales
sqldf
stringr
xtable

The data appears in .RData, .csv and .txt files. The file att.RData contains permutations of the data as described in the paper and is produced by a randomized within-household selection process that relies on individually-identified data. Due to privacy concerns, this randomization was conducted and the resulting data anonymized and exported. However, the analysis of this randomization is produced in compute_itt.R.

These files were verified to run by Bradley Spahn on April 16, 2018.  His session info was as follows:

R version 3.2.3 (2015-12-10)
Platform: x86_64-apple-darwin13.4.0 (64-bit)
Running under: OS X 10.12.6 (unknown)

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] xtable_1.8-2      stringr_1.2.0     sqldf_0.4-10      RSQLite_1.0.0    
 [5] DBI_0.6           gsubfn_0.6-6      proto_0.3-10      scales_0.4.1     
 [9] reshape2_1.4.2    knitr_1.16        ggplot2_2.2.1     gdata_2.17.0     
[13] dplyr_0.5.0.9000  doParallel_1.0.10 iterators_1.0.8   foreach_1.4.3    
[17] Matching_4.9-2    MASS_7.3-45      

loaded via a namespace (and not attached):
 [1] Rcpp_0.12.12     magrittr_1.5     munsell_0.4.3    colorspace_1.2-6 R6_2.1.2        
 [6] rlang_0.0.0.9002 plyr_1.8.4       tcltk_3.2.3      tools_3.2.3      grid_3.2.3      
[11] gtable_0.2.0     gtools_3.5.0     lazyeval_0.2.0   assertthat_0.1   tibble_1.2      
[16] codetools_0.2-14 glue_0.0.0.9000  stringi_1.1.5    chron_2.3-47   