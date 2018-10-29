##############################################
## make data set for individual-level analysis
##
## simon jackman
## july 2014
###############################################
library(stringr)

###############################################
## this is a household level attribute 
sdata <- read.delim(file="sampexp_nodupes.txt",
                    sep="\t")

##sdata <- read.delim(file="../data/sampexp_20160314.txt",
##                    sep="\t")

##drop <- sdata$col0_abtid=="14337" & sdata$vendorid != "LALTN2176182"
##sdata <- sdata[!drop,]

sdata$latereg <- sdata$regdate > 20120908

## responder hh (completed interview)
responder.hh <- sdata$dispo=="Complete"
ok <- responder.hh & sdata$namematch==1

anes_sample <- read.csv(file="anes_sample.csv",
                        stringsAsFactors=FALSE)

theVars <- c("col0_abtid",
             names(sdata)[!(names(sdata) %in% names(anes_sample))])

foo <- merge(anes_sample,
             sdata[ok,theVars],
             by.x="COL0_ABTID",
             by.y="col0_abtid",
             all.x=TRUE)

foo$col0_abtid.1 <- NULL
foo$e2012new <- NULL

foo$haveName <- foo$n1!="" | foo$n2!="" | foo$n3!=""
foo$namematch[foo$dispo=="Complete" & is.na(foo$namematch)] <- 0

## extra Brad code
name <- paste(anes_sample$n1, anes_sample$n2, anes_sample$n3)
ok <- anes_sample$complete & str_length(name)<12 ##& str_length(name)>3
table(str_length(name))
name[ok]
