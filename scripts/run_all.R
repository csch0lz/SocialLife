#run all scripts

# Run all

#DATA CLEANING
rm(list = ls())
print('CLEANING STUDY 1A')
source('scripts/clean_norming_data_study1a.R')
rm(list = ls())
print('CLEANING STUDY 1B')
source('scripts/clean_norming_data_study1b.R')
rm(list = ls())
print('CLEANING STUDY 2')
source('scripts/clean_study2.R')
rm(list = ls())

# RELIABILITY INDEX
print('COMPUTING RELATABILITY CORRELATION')
source('scripts/compute_corr.R')
rm(list=ls())

# TABLES
print('MAKING BINGE TABLE')
source('scripts/make_bingeTable.R')
rm(list = ls())
print('MAKING CRAVING TABLE')
source('scripts/make_cravingTable.R')
rm(list = ls())
print('MAKING DESCRIPTIVES TABLE')
source('scripts/make_descriptivesTable.R')
print('ALL DONE')
print('MAKING Emo Avg TABLE')
source('scripts/make_emoAvgTable.R')
rm(list=ls())
print('MAKING EMO Fam TABLE')
source('scripts/make_emofamTable.R')
rm(list=ls())
print('MAKING Fam Avg TABLE')
source('scripts/make_famAvgTableR.R')
rm(list=ls())

# Manipulation checks
print('MAKING MC table Study 1a')
source('scripts/make_MC_table_study1a.R')
rm(list=ls())
print('MAKING MC table Source')
source('scripts/make_mcTableSource.R')
rm(list=ls())

print('MAKING MC TABLE VALENCE')
source('scripts/make_MC_table_study1a.R')
rm(list = ls())
print('MAKING MC TABLE SOURCE')
source('scripts/make_mcTableSource.R')
rm(list=ls())



