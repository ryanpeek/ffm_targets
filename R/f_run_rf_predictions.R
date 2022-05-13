# run predictions using random forest model
# based on T. Grantham's CEFF flow modeling
# just packaged that code into a function for more reproducibility

# Make Predictions From RF Model ------------------------------------------

# requires rf model, and data/csv to make predictions from
f_run_rf_predictions <- function(metric, rf, comid_data, outdir){

  library(dplyr)
  options(dplyr.summarise.inform = FALSE) # turn off messages
  library(fs)

  # Magnitude Metrics for scaling
  metrics_mag <- c("FA_Mag","Wet_BFL_Mag_50","Wet_BFL_Mag_10","SP_Mag","DS_Mag_90","DS_Mag_50", "Peak_2","Peak_5","Peak_10")

  # predict RF to each COMID
  preds <- predict(rf, comid_data, predict.all=TRUE)
  # save median, 10th, 25th, 75th, & 90th percentiles

  # make predictions
  predp10<-apply(preds$individual,1,function(x) quantile(x,probs=0.1))
  predp25<-apply(preds$individual,1,function(x) quantile(x,probs=0.25))
  predp50<-apply(preds$individual,1,median)
  predp75<-apply(preds$individual,1,function(x) quantile(x,probs=0.75))
  predp90<-apply(preds$individual,1,function(x) quantile(x,probs=0.9))

  ffm_preds <- data.frame(
    metric = metric,
    comid = comid_data$COMID,
    wy = comid_data$WaYr,
    area = comid_data$DRAIN_SQKM,
    p10 = predp10, p25 = predp25, p50 = predp50,
    p75 = predp75, p90 = predp90)

  if(metric %in% metrics_mag){
    print("magnitude metric identified...scaling by drainage area.")
    # scale predictions to cfs
    ffm_preds$p10 <- ffm_preds$area * ffm_preds$p10
    ffm_preds$p25 <- ffm_preds$area * ffm_preds$p25
    ffm_preds$p50 <- ffm_preds$area * ffm_preds$p50
    ffm_preds$p75 <- ffm_preds$area * ffm_preds$p75
    ffm_preds$p90 <- ffm_preds$area * ffm_preds$p90
  }

  # summarize by median across all years
  ffm_preds_median <- ffm_preds %>%
    group_by(comid, metric, area) %>%
    summarize(across(.cols = c(p10, p25, p50, p75, p90), median, na.rm=TRUE))
  # write to temp directory to storing output
  print(glue("Writing out {metric}..."))
  fs::dir_create(glue("data_output/{outdir}"))
  readr::write_csv(ffm_preds_median,
         file = fs::path(glue("data_output/{outdir}/{metric}_{unique(ffm_preds_median$comid)}.csv")))

}
