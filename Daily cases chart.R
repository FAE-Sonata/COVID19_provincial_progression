## SET YOUR DIRECTORY ##
libraries_needed<-c("openxlsx", "data.table", "magrittr", "stringr", "readr")
lapply(libraries_needed,require,character.only=TRUE)
## see example (without recoveries) https://en.wikipedia.org/wiki/Template:COVID-19_pandemic_data/United_States/Virginia_medical_cases_chart
## outputs lines of code in the format
## YYYY-MM-DD;[cumulative deaths];[cumulative recovered];[cumulative confirmed]
# Utilities ---------------------------------------------------------------
positive_pct<-function(rounded_pct) {
  return(paste0("+", rounded_pct, "%"))
}

negative_pct<-function(rounded_pct) {
  return(paste0("-", rounded_pct, "%"))
}

round_pct<-function(pct)  {
  if(is.na(pct) | !is.numeric(pct))
    return(NA)
  else if(pct >= 1)
    return(positive_pct(round(pct, 1)))
  else  {
    if(pct >= 0.01)
      return(positive_pct(round(pct, 2)))
    else if(pct > 0)
      return(paste0("<", positive_pct(0.01)))
    else if(pct == 0)
      return("{{=}}")
    else if(pct > -0.01)
      return(paste0(">", negative_pct(0.01)))
    else if(pct > -1)
      return(negative_pct(-round(pct, 2)))
    else
      return(negative_pct(-round(pct, 1)))
  }
}

# main function -----------------------------------------------------------
chart_output<-function(province)  {
  dt_province<-read.xlsx("Mainland CN daily cases by province.xlsx",
                         sheet=province,
                         detectDates=T) %>%
    as.data.table()
  
  original_ncols<-ncol(dt_province)
  dt_province[,check:=Total.symptomatic >= Recovered + Total.death]
  stopifnot(all(dt_province$check))
  
  # accounts for corrections (that aren't actually new statistics)
  dt_province[,`:=`(lag_cases=shift(Total.symptomatic),
                    lag_recoveries=shift(Recovered),
                    lag_deaths=shift(Total.death))]
  dt_province[,`:=`(change_cases=Total.symptomatic - lag_cases,
                    change_recoveries=Recovered - lag_recoveries,
                    change_deaths=Total.death - lag_deaths)]
  
  idx_actual_change<-grepl("change_", names(dt_province)) %>% which()
  dt_actual_change<-dt_province[,c(idx_actual_change), with=F]
  ## determine whether code needs to be truncated, e.g. if the last update
  ## (change in recoveries, cumulative cases, or deaths) is on 9 May 2020
  ## (e.g. 2020-05-09;13;887;1000), and the next update does not occur until
  ## 16 May (e.g. 2020-05-16;13;912;1000), the rows in between will be:
  ## ;13;887;1000
  has_daily_change<-apply(dt_actual_change, MARGIN=1, function(x) any(x != 0))
  has_daily_change[1]<-T
  
  dt_province[,has_daily_change:=has_daily_change]
  dt_province[,code_changes_only:=ifelse(has_daily_change,
                                         paste(Date,
                                               Total.death,
                                               Recovered,
                                               Total.symptomatic,
                                               sep=";"),
                                         "")]
  dt_province[,code_changes_only:=str_replace_all(code_changes_only,
                                                  ";0",
                                                  ";")]
  dt_province[,non_date_code:=str_replace(shift(code_changes_only),
                                          paste("[1-9]{1}[0-9]{3}",
                                                "[0-1]{1}[0-9]{1}",
                                                "[0-3]{1}[0-9]{1}",
                                                sep="\\-"),
                                          "")]
  dt_province[,code_final:=ifelse(has_daily_change,
                                  code_changes_only,
                                  non_date_code)]
  output_lines<-Filter(function(x) x!="", dt_province$code_final)
  write_lines(output_lines, paste0(province, " template output.txt"))
  # consider setnafill() for the raw daily increases
}