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
