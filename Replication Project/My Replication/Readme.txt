Readme for “Corporate Board Quotas and Gender Equality Policies in the Workplace”

Audrey Latura, Visiting Fellow, Department of Government, Harvard University, ORCID: 0000-0003-4869-1077

Ana Catalano Weeks, Lecturer (Assistant Professor) in Comparative Politics, University of Bath, ORCID: 0000-0003-1394-8223

Replication code 
1. AJPS_Latura_Weeks_Replication_Code.R
This code will read in the data files listed below and reproduce the analysis. 

Codebook 
1. AJPS_Latura_Weeks_Codebook.PDF
This codebook contains a list of all variables in the analysis datasets with variable descriptions, values, and value labels for all values. 

Data files
1. data_final_25Jan2022.RData – dataset used for main quantitative analysis
2. company_level_20companies_merged.RData – dataset used for Table A11 in Supplemental Information Appendix
3. Data_for_matching_country_level.csv – dataset used for statistical matching, Table A1 in Supplemental Information Appendix
4. snoq.csv – dataset used for Figure A1 in Supplemental Information Appendix
Software
Results produced using (base) R version 4.1.2. The following packages are required to replicate figures and tables in the paper: 

multiwayvcov
lmtest
ggplot2
dplyr
stargazer
MatchIt
aod
clubSandwich
reshape2
