train = read.csv('/Users/cholmes/Desktop/training.csv')
test = read.csv('/Users/cholmes/Desktop/test.csv')

#Experimental Imputation Method
df_impute = train

grouped_2_cols = select(train, c('PRI_jet_leading_pt', 'PRI_jet_leading_eta','PRI_jet_leading_phi'))
grouped_4_cols = select(train, c('DER_deltaeta_jet_jet','DER_mass_jet_jet','DER_prodeta_jet_jet','DER_lep_eta_centrality','PRI_jet_subleading_pt','PRI_jet_subleading_eta','PRI_jet_subleading_phi'))

for (i in 2:ncol(grouped_2)) {
  change = as.numeric(((grouped_2[4,i] - grouped_2[3,i]) + (grouped_2[3,i] - grouped_2[2,i]))/2)
  change_jet0 = grouped_2[2,i] - change
  df_impute[grouped_2_cols[i-1]][is.na(df_impute[grouped_2_cols[i-1]])] = change_jet0
}

impute_0 = filter(df_impute, PRI_jet_num == 0)
impute_1 = filter(df_impute, PRI_jet_num == 1)
impute_23 = filter(df_impute, PRI_jet_num >= 2)

for (i in 2:ncol(grouped_4)) {
  change = as.numeric(grouped_4[4,i] - grouped_4[3,i])
  change_jet1 = grouped_4[3,i] - change
  change_jet0 = change_jet1 - change
  
  impute_0[grouped_4_cols[i-1]][is.na(impute_0[grouped_4_cols[i-1]])] = change_jet0
  impute_1[grouped_4_cols[i-1]][is.na(impute_1[grouped_4_cols[i-1]])] = change_jet1
  
  
}
exp_impute_train = rbind(impute_0, impute_1, impute_23)








