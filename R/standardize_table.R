#' Rate comparison after adjusting for a confounder
#'
#' @param df A dataframe with three categorical variables (chr or factor)
#' @param response character vector indicating the name of the response variable
#' @param explanatory character vector indicating the name of the explanatory variable
#' @param confounder character vector indicating the name of the confounding variable
#' @param mix a two-entry numerical vector representing the percentages of each level of the confounder used for standardization
#'
#' @return a list with entries "unstd" for the unstandardized table, and "std" for the standardized table using the weights from `mix`
#' @export
#'
#' @examples
#' whickham<-read.table("https://s3.amazonaws.com/pbreheny-data-sets/whickham.txt",
#'     sep = "\t",header=TRUE)
#' whickham$over65 <- ifelse(whickham$Age=="65-","Yes","No")
#' table1<-standardize_table(
#'     df = whickham,
#'     response = "Survival",
#'     explanatory = "Smoking",
#'     confounder = "over65",
#'     mix = c(.7, .3)
#' )
#' table1$unstd
#' table1$std
standardize_table<-function(df,
                            response,
                            explanatory,
                            confounder,
                            mix = c(.5, .5)){
    mix_table<-rbind(mix,mix)
    # print(mix_table)

    newdf<-split(df, df[confounder])

    rates1<- prop.table(table(newdf[[1]][[explanatory]],
                              newdf[[1]][[response]]),
                        margin =1)[,1]
    rates2<- prop.table(table(newdf[[2]][[explanatory]],
                              newdf[[2]][[response]]),
                        margin =1)[,1]
    all_rates<-cbind(rates1,rates2)
    # print(all_rates)

    conf_table <-prop.table(
        table(df[[explanatory]], df[[confounder]]),
        margin = 1
    )
    # print(conf_table)
    unstd_rates<-diag(all_rates%*%t(conf_table))
    std_rates<-diag(all_rates %*% t(mix_table))

    return_un<-cbind(all_rates[,1],conf_table[,1],
                all_rates[,2],conf_table[,2],
                unstd_rates)
    return_st<-cbind(all_rates[,1],mix_table[,1],
                all_rates[,2],mix_table[,2],
                std_rates)

    list(unstd = return_un,
         std = return_st)

}
