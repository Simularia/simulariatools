#' Plot stability class
#' 
#' @param mydata A data frame containing cs field
#' @param type type determines how the data are split and then plotted. The default is "season". Any other string will give an hourly plot.
#'
#' @import ggplot2
#' @import scales
#' 
#' @export
#' 
plotStabClass <- function(mydata, type="season") {
    
    require("ggplot2")
    require("scales")
    
    mydata$cs[mydata$cs == 1] <- "A"
    mydata$cs[mydata$cs == 2] <- "B"
    mydata$cs[mydata$cs == 3] <- "C"
    mydata$cs[mydata$cs == 4] <- "D"
    mydata$cs[mydata$cs == 5] <- "E"
    mydata$cs[mydata$cs == 6] <- "F"
    
    if (type == "season") {
        mydata$quarter <- quarters(mydata$date)
        mydata$quarter[mydata$quarter == "Q1"] <- "Inverno"
        mydata$quarter[mydata$quarter == "Q2"] <- "Primavera"
        mydata$quarter[mydata$quarter == "Q3"] <- "Estate"
        mydata$quarter[mydata$quarter == "Q4"] <- "Autunno"
        mydata$quarter <- factor(mydata$quarter, levels=c("Inverno", "Primavera", "Estate", "Autunno"))
        v <- ggplot(data=mydata, aes(x=quarter)) + 
            geom_bar(aes(fill=cs), position="fill")
    } else {
        mydata$hour <- factor(as.numeric(format(t$date, format="%H")))
        v <- ggplot(data=mydata, aes(x=hour)) +
            geom_bar(aes(fill=cs), position="fill")
    }
    v + scale_y_continuous(labels=percent, breaks=seq(0,1,0.1)) +
        scale_fill_brewer(palette="Spectral") +
        labs(x="", y="Percentuale (%)") + 
        guides(fill=guide_legend(label.position="bottom", label.hjust=0.5, title=NULL, direction="horizontal")) +
        theme_bw(base_family="Helvetica") + 
        theme(legend.position="bottom")
    #         theme_bw(base_family="Helvetica") + theme(legend.position="bottom", legend.title=element_blank(), legend.text=element_text())
   
    return(v)
}