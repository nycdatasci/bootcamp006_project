render_data <- function(DT,cat,Cond,texas,Cond1) {
  if (Cond1==1900) {
    Cond1=as.character(as.numeric(Cond)+1)
  }
  return_data = NULL
  data_cat=NULL
  data_cat_prev=NULL
  for (i in 1:nrow(DT)) {
    return_data$full[i]=levels(DT$full)[DT$full[i]]
    return_data$state[i]=levels(DT$state)[DT$state[i]]
    if (cat=="All Sources"){
      data_cat = DT$NG_Gross_Withdrawals_All[[i]][Cond]
      data_cat_prev = DT$NG_Gross_Withdrawals_All[[i]][Cond1]
    }
    if (cat=="Shale"){
      data_cat = DT$NG_Gross_Withdrawals_From_Shale[[i]][Cond]
      data_cat_prev = DT$NG_Gross_Withdrawals_From_Shale[[i]][Cond1]
    }
    if (cat=="Coal Beds"){
      data_cat = DT$NG_Gross_Withdrawals_From_Coal_Beds[[i]][Cond]
      data_cat_prev = DT$NG_Gross_Withdrawals_From_Coal_Beds[[i]][Cond1]
    }
    if (cat=="Oil Wells"){
      data_cat = DT$NG_Gross_Withdrawals_From_Oil_Wells[[i]][Cond]
      data_cat_prev = DT$NG_Gross_Withdrawals_From_Oil_Wells[[i]][Cond1]
    }
    if (cat=="Gas Wells"){
      data_cat = DT$NG_Gross_Withdrawals_From_Gas_Wells[[i]][Cond]
      data_cat_prev = DT$NG_Gross_Withdrawals_From_Gas_Wells[[i]][Cond1]
    }
    if (cat=="Consumption"){
      data_cat = DT$Total_Consumption[[i]][Cond]
      data_cat_prev = DT$Total_Consumption[[i]][Cond1]
    }
    return_data$sum[i]=sum(data_cat)
    return_data$diff[i]=sum(data_cat_prev)-sum(data_cat)
  }
  return_data$sum[which(return_data$sum==0)]=NA
  return_data$diff[which(return_data$diff==0)]=NA
  if (texas==TRUE) {
    return_data$sum[return_data$state=='TX']=NA
    return_data$diff[return_data$state=='TX']=NA
  }
  return(return_data)
}

sortdata <- function(D,retnum) {
  D = D[with(D, order(D$sum,decreasing=TRUE)), ]
  return(D[1:retnum,])
}
sortdiffdata <- function(D,retnum) {
  D = D[with(D, order(D$diff,decreasing=TRUE)), ]
  
  D1 = D[with(D, order(D$diff,decreasing=FALSE)), ]
  D = D[1:retnum,]
  D1 = D1[retnum:1,]
  return(rbind(D,D1))
}

chart_data <- function(DT,cat,cond,state) {
  data_cat=NULL
  for (i in 1:nrow(DT)) {
    if (state==DT$state[i]) {
      if (cat=="All Sources"){
        data_cat = DT$NG_Gross_Withdrawals_All[[i]][cond]
      }
      if (cat=="Shale"){
        data_cat = DT$NG_Gross_Withdrawals_From_Shale[[i]][cond]
      }
      if (cat=="Coal Beds"){
        data_cat = DT$NG_Gross_Withdrawals_From_Coal_Beds[[i]][cond]
      }
      if (cat=="Oil Wells"){
        data_cat = DT$NG_Gross_Withdrawals_From_Oil_Wells[[i]][cond]
      }
      if (cat=="Gas Wells"){
        data_cat = DT$NG_Gross_Withdrawals_From_Gas_Wells[[i]][cond]
      }
      if (cat=="Consumption"){
        data_cat = DT$Total_Consumption[[i]][cond]
      }
      return(data_cat)
    }
  }
}

