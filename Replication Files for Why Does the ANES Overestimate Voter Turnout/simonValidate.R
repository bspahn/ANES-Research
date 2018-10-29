################################################
## vote validation tables
## 
## simon jackman
## july 2014
#############################################
source("useSampledExport.R")

## match rate, completes
table(foo$dispo,foo$namematch,exclude=NULL)
sum(foo$namematch[foo$dispo=="Complete"],na.rm=TRUE)

## pick up Brad's registration altstreet_match
## TRUE means 

## validated registration factor
foo$vr <- factor(foo$registration,
                 levels=c(1,NA,0),
                 exclude=NULL,
                 labels=c("Registered","Unknown","Unregistered"))

## late registrations move to unknown
foo$vr2 <- foo$vr
foo$vr2[foo$vr=="Registered" & foo$regdate>20121106] <- "Unknown"

## move people who say they are registered someplace else, but we can't find them
## at the registration address supplied by R (we find them some place else)
foo$vr2[foo$prevote_reg=="2. Registered at a different address" & foo$altstreet_match==FALSE] <- "Unknown"

## validated vote factor
foo$vv <- factor(foo$e2012g,
                 levels=c(0,NA,1),
                 exclude=NULL,
                 labels=c("Not voted","Unknown","Voted"),
                 ordered=TRUE)

foo$vv2 <- foo$vv
foo$vv2[foo$prevote_reg=="2. Registered at a different address" &  foo$altstreet_match==FALSE] <- "Unknown"

################################
## do we have a name?
table(foo$dispo,foo$haveName)

denom <- foo$dispo=="Complete" & foo$haveName
sum(foo$namematch[denom],na.rm=TRUE)/sum(denom,na.rm=TRUE)

###############################
## validate registration self-report
r1 <- xtabs(~finalRegist + vr,
            data=foo,
            exclude=NULL,
            subset=complete)

r2 <- xtabs(weight_ftf ~ finalRegist + vr,
            data=foo,
            exclude=NULL,
            subset=complete)

r1.2 <- xtabs(~finalRegist + vr2,
            data=foo,
            exclude=NULL,
            subset=complete)

r2.2 <- xtabs(weight_ftf ~ finalRegist + vr2,
            data=foo,
            exclude=NULL,
            subset=complete)

## validated registration rate, unweighted & weighted
vrr <- sum(r1[,1])/sum(r1[,-2])
vrr.w <- sum(r2[,1])/sum(r2[,-2])


##############################
## validate turnout self-report
t1 <- xtabs(~turnout + vv,
            data=foo,
            exclude=NULL,
            subset=complete)

t1.out <- prop.table(t1,1)*100

t2 <- xtabs(weight_ftf ~ turnout + vv,
            data=foo,
            exclude=NULL,
            subset=complete)

tabWriter <- function(t2,fname="table2.tex"){
  
  t2.out <- prop.table(t2,1)*100
  t2.out <- cbind(prop.table(xtabs(weight_ftf~turnout,data=foo))*100,
                  t2.out)
  colnames(t2.out)[1] <- "All"
  
  require(xtable)
  print(xtable(t2.out,
               digits=1,
               align=c("rrrrr")),
        hline.after=NULL,
        floating=FALSE,
        include.colnames = FALSE,
        add.to.row=list(pos=list(-1,0,nrow(t2.out)),
                        command=c("& & \\multicolumn{3}{c}{Validated Turnout} \\\\\n",
                                  paste("Self report~~~~~~~ & All & Not voted & Unknown & Voted \\\\",
                                        "\\cline{3-5} & & \\\\[-10pt]\n",sep="\n"),
                                  "\\cline{3-5} & & \\\\[-10pt]\n")),
        file=fname)
  
  cat("\n\n\n",file=fname,append=TRUE)
  cat("\\bigskip",file=fname,append=TRUE)
  cat("\n\n\n",file=fname,append=TRUE)
  
  t2.outb <- prop.table(t2,2)*100
  t2.outb <- rbind(t2.outb,
                   prop.table(xtabs(weight_ftf~vv,data=foo))*100)
  rownames(t2.outb)[dim(t2.outb)[1]] <- "All"
  print(xtable(t2.outb,
               digits=1,
               align="rrrr"),
        floating=FALSE,
        hline.after=NULL,
        include.colnames = FALSE,
        add.to.row=list(pos=list(-1,0,nrow(t2.outb)-1),
                        command=c("& \\multicolumn{3}{c}{Validated Turnout} \\\\\n",
                                  paste("Self report~~~~~~~ & Not voted & Unknown & Voted \\\\",
                                        "\\cline{2-4} & & \\\\[-10pt]\n",sep="\n"),
                                  "\\cline{2-4} & & \\\\[-10pt]\n")),
        file=fname,
        append=TRUE)
  
  tmp <- readLines(con=fname)
  tmp <- gsub(pattern="\\\\\\\\ ",replacement="\\\\\\\\[2pt\\]",tmp)
  writeLines(tmp,con=fname)
}

########################################################
## version 1, weighted, no altstreet_match reassignments
t2 <- xtabs(weight_ftf ~ turnout + vv,
            data=foo,
            exclude=NULL,
            subset=complete)
tabWriter(t2)

########################################################
## version 2, weighted, with altstreet_match reassignments
fname <- "table3.tex"
t2 <- xtabs(weight_ftf ~ turnout + vv2,
            data=foo,
            exclude=NULL,
            subset=complete)
tabWriter(t2,fname=fname)


## validated turnout rate, unweighted & weighted
vtr <- sum(t1[,3])/sum(t1[,-2])
vtr.w <- sum(t2[,3])/sum(t2[,-2])

require(dplyr)
#### Numbers comparable to BKL 
informative.turnout.response <- c("Not voted","Not voted, unregistered", "Voted")
foo %>%
  dplyr::filter(registration==1 & namematch==1 & turnout %in% informative.turnout.response) %>%
  group_by(turnout, e2012g) %>% 
  summarise(tot.wt = sum(weight_ftf)) %>% 
  ungroup %>% 
  mutate(pct = round(100 * tot.wt / sum(tot.wt),1)) 

foo %>%
  dplyr::filter(registration==1 & namematch==1 & turnout %in% informative.turnout.response) %>%
  group_by(turnout, e2012g) %>% 
  summarise(tot.wt = sum(weight_ftf)) %>% 
  ungroup %>% 
  mutate(pct = 100 * tot.wt / sum(tot.wt)) %>% 
  group_by(consistent = (turnout == "Voted" & e2012g == 1) | (grepl("Not",turnout) & e2012g == 0)) %>%
  summarise(pct = round(sum(pct),1))
