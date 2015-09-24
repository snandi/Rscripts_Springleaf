## xgboost, 80 iter, 65% train, nrounds 140, max.depth 8
nohup R CMD BATCH --no-save '--args N_TrainIter=80 Percent_Train=0.65 SubmissionNumberStart=38 Nrounds=140 Max.Depth=8' RS6_XGBoost_noNA.R RS6_XGBoost_iter80_pct65_nround140.Rout &
