

require(doParallel)
require(foreach)
require(iterators)
require(MASS)
require(dplyr)
require(xtable)
require(knitr)



anes_sample <- read.csv("anes_sample.csv")


## Make sampexp have no ll records, at least for some of these analyses
sampexp <- read.delim("sampexp_nocat.txt")

sampexp <- sampexp[!is.na(sampexp$oversamp),]

sampexp$complier <- ifelse(sampexp$dispo=="Complete", 1, 0)

namematch.abtid <- unique(sampexp$col0_abtid[sampexp$namematch==1 & sampexp$dispo=="Complete"])


bad.dispo <- c("Address does not exist", "Out of sample area", "Address does not have a permanently occupied household", "No adult citizen", "OS only, no Hispanic/AA")


#### This is for the match rate footnote
sampexp %>% filter(!dispo %in% bad.dispo) %>% group_by(col0_abtid, dispo) %>% dplyr::summarise(matched = any(!is.na(vendorid))) %>% ungroup %>% group_by(complete = dispo == "Complete") %>% dplyr::summarise(mean(matched))


vh.cols <-  c("e2012g", "e2010g", "e2008g", "e2006g")
vh.labs <-  c("2012  Turnout", "2010  Turnout", "2008  Turnout", "2006  Turnout")

regvh.cols <-  c("registration","e2012g", "e2010g", "e2008g", "e2006g")
regvh.labs <-  c("2012 Registration","2012  Turnout", "2010  Turnout", "2008  Turnout", "2006  Turnout")


bad.dispo <- c("Address does not exist", "Out of sample area", "Address does not have a permanently occupied household", "No adult citizen", "OS only, no Hispanic/AA")

## people.per.hh

sampexp %>% 
  filter(!dispo %in% bad.dispo)  %>%
  group_by(col0_abtid, complier) %>% 
  dplyr::summarise(number.of.people = sum(!is.na(vendorid)), 
                                          e2012g = mean(e2012g), 
                                          e2010g = mean(e2010g),
                                          e2008g = mean(e2008g),
                                          e2006g = mean(e2006g)) %>%
  ungroup %>%
  group_by(complier) %>%
  dplyr::summarise(avg.n = mean(number.of.people))

hh <- sampexp %>% 
  filter(!dispo %in% bad.dispo)  %>%
  filter(source == "ts") %>% 
  group_by(col0_abtid, complier, dispo) %>% 
  dplyr::summarise(number.of.people = sum(!is.na(vendorid)), 
                   e2012g = coalesce(mean(e2012g),0), 
                   e2010g = coalesce(mean(e2010g),0), 
                   e2008g = coalesce(mean(e2008g),0), 
                   e2006g = coalesce(mean(e2006g),0)) %>%
  filter(number.of.people < 4 & number.of.people > 0) %>% 
  ungroup 

require(Matching)
matches <- list()
for(i in vh.cols){
  print(i)
  matches[[i]] <- Match(Y = hh[[i]], Tr = hh$complier, X = hh$number.of.people)
}

lapply(matches, summary)

hh.reg <- sampexp %>% 
  filter(!dispo %in% bad.dispo & registration == 1)  %>%
  group_by(col0_abtid, complier) %>% 
  dplyr::summarise(number.of.people = sum(!is.na(vendorid)), 
                   e2012g = coalesce(mean(e2012g),0), 
                   e2010g = coalesce(mean(e2010g),0), 
                   e2008g = coalesce(mean(e2008g),0), 
                   e2006g = coalesce(mean(e2006g),0)) %>%
  filter(number.of.people < 4 & number.of.people > 0) %>% 
  ungroup 

for(i in vh.cols){
  print(i)
  matches[[paste0(i,".reg")]] <- Match(Y = hh.reg[[i]], Tr = hh.reg$complier, X = hh.reg$number.of.people)
}

lapply(matches, summary)



## make.vh.comparisons is going to be the workhorse function making the crosstabs

make.vh.comparison <- function(dat, group1, group2, colname, rowlabel, lab.group1 = "Group1", lab.group2 = "Group2"){
  
  
  mainsamp <- t.test(dat[group1 & dat$oversamp == 0,colname], 
                     dat[group2 & dat$oversamp == 0,colname] , na.action = na.omit)
  
  oversamp <- t.test(dat[group1 & dat$oversamp >= 1,colname], 
                     dat[group2 & dat$oversamp >= 1,colname], na.action = na.omit)
  
  group1.avg <- c(mainsamp$estimate[1], oversamp$estimate[1])
  group2.avg <- c(mainsamp$estimate[2], oversamp$estimate[2])
  Difference <- group1.avg - group2.avg
  SE       <- c((mainsamp$conf.int[2] - mainsamp$conf.int[1])/3.92,
                (oversamp$conf.int[2] - oversamp$conf.int[1])/3.92)
  p <- c(mainsamp$p.value, oversamp$p.value)
  
  
  rowtype <- c("Main Sample","Full Sample")
  
  out <- data.frame(rowlabel,
                    rowtype,
                    Group1 = round(group1.avg, 3), 
                    Group2 = round(group2.avg, 3), 
                    Difference = round(group2.avg - group1.avg, 3),
                    p = round(p,3),
                    SE
                    #,row.names = paste0(rowlabel," - ",rowtype) 
  )
  colnames(out)[3:4] <- c(lab.group1, lab.group2)
  out
}



## All Voters


require(xtable)
require(reshape2)
dat.all <-  sampexp %>% 
  filter(!is.na(vendorid)) %>% 
  filter(!dispo %in% bad.dispo) %>% 
  group_by(col0_abtid, complier,dispo, oversamp) %>%
  summarise(e2012g = mean(e2012g, na.rm=T), e2010g = mean(e2010g, na.rm=T), e2008g = mean(e2008g, na.rm=T), e2006g = mean(e2006g, na.rm=T), n = length(vendorid), registration = mean(registration)) %>% 
  as.data.frame


all.tbl <- foreach( i = 1:5, .combine= rbind) %do% 
  make.vh.comparison(dat = dat.all, 
                     group1 = dat.all$complier == 0, 
                     group2 = dat.all$complier == 1  , 
                     colname = regvh.cols[i], 
                     rowlabel = regvh.labs [i],
                     lab.group1 = "Non-Complier HH",
                     lab.group2 = "Complier HH"
  )

xtable(all.tbl, caption = "Average rates of registration and turnout for complier and non-complier households.", label = "tab:complier_noncomplier_allvoters")
kable(all.tbl, digits = 3 )

all.tbl$Comparison <- "All Listed Persons"

# Moving to the 2012 election, the difference in turnout rates are 3.8 percentage points larger for the main sample cases in 2012 than in 2008 (9.1\% vs 5.3\%, respectively, see the lower left panel of Figure 2). 
all.tbl$Difference[3] 
all.tbl$Difference[7]



dat.lm <- melt(data = dat.all, id.vars = c("oversamp","col0_abtid","complier"), measure.vars = c("e2012g", "e2008g"))
summary(lm(value~ complier * variable, data = dat.lm, subset = oversamp==0 ))
summary(lm(value~ complier * variable, data = dat.lm, subset = oversamp==1 ))



## The next crosstab tells us that these differences in turnout rates pre-treatment due to registration. HH w/registered voters more like to comply and registered voters more likely to vote


## Proportion unregistered 


dat.regrate <-  sampexp %>% 
  filter(!is.na(vendorid)) %>% 
  filter(!dispo %in% bad.dispo) %>% 
  group_by(col0_abtid, complier,dispo,oversamp) %>%
  summarise(e2012g = mean(e2012g, na.rm=T), e2010g = mean(e2010g, na.rm=T), e2008g = mean(e2008g, na.rm=T), e2006g = mean(e2006g, na.rm=T), reg = mean(registration, na.rm=T)) %>% 
  as.data.frame



make.vh.comparison(dat = dat.regrate, 
                   group1 = dat.regrate$complier == 0, 
                   group2 = dat.regrate$complier == 1, 
                   colname = "reg", 
                   rowlabel = "Registration",
                   lab.group1 = "Non-Complier HH",
                   lab.group2 = "Complier HH")




## Subset to Registered Voters



require(xtable)
dat.reg <-  sampexp %>% 
  filter(!is.na(vendorid)) %>% 
  filter(!dispo %in% bad.dispo) %>% 
  filter(registration==1) %>% 
  group_by(col0_abtid, complier,dispo,oversamp) %>%
  summarise(e2012g = mean(e2012g, na.rm=T), e2010g = mean(e2010g, na.rm=T), e2008g = mean(e2008g, na.rm=T), e2006g = mean(e2006g, na.rm=T)) %>% 
  as.data.frame

regonly.tbl <- foreach( i = 1:4, .combine= rbind) %do% 
  make.vh.comparison(dat = dat.reg, 
                     group1 = dat.reg$complier == 0, 
                     group2 = dat.reg$complier == 1, 
                     colname = vh.cols[i], 
                     rowlabel = vh.labs [i],
                     lab.group1 = "Non-Complier HH",
                     lab.group2 = "Complier HH"
  )


xtable(regonly.tbl, digits = 3, label = "tab:turnout_regonly", caption = "Average rates of registration and turnout for complier and non-complier households.")
kable(regonly.tbl, digits =3)

# Non-response bias basically works by separating registered people (who comply) from unregistereds (who do not)




#However, even if we set the registration status of people who registered after September 1, 2012 to unregistered, the same pattern of no significant differences in the pre-treatment elections, but large and significant differences in 2012 holds up.


sampexp.tmp <- sampexp
sampexp.tmp$registration <- ifelse(sampexp.tmp$regdate < 20120901 | is.na(sampexp.tmp$regdate) , sampexp.tmp$registration, 0)


dat.reg.2 <-  sampexp.tmp %>% 
  filter(!is.na(vendorid)) %>% 
  filter(!dispo %in% bad.dispo) %>% 
  filter(registration==1) %>% 
  group_by(col0_abtid, complier,dispo,oversamp) %>%
  summarise(e2012g = mean(e2012g, na.rm=T), e2010g = mean(e2010g, na.rm=T), e2008g = mean(e2008g, na.rm=T), e2006g = mean(e2006g, na.rm=T)) %>% 
  as.data.frame

regonly.2.tbl <- foreach( i = 1:4, .combine= rbind) %do% 
  make.vh.comparison(dat = dat.reg.2, 
                     group1 = dat.reg.2$complier == 0, 
                     group2 = dat.reg.2$complier == 1, 
                     colname = vh.cols[i], 
                     rowlabel = vh.labs [i],
                     lab.group1 = "Non-Complier HH",
                     lab.group2 = "Complier HH"
  )


#xtable(regonly.tbl, digits = 3)
kable(regonly.2.tbl, digits =3)

regonly.2.tbl$Comparison <- "Registered Only"

# Non-response bias basically works by separating registered people (who comply)
# from unregistereds (who do not)




# These differences suggest that the non-response bias of the ANES is driven by
# a failure to encourage households with unregistered people to participate in
# the survey.  Among registered people, past turnout seems balanced between
# complier and non-complier households.

## Complier person vs non-complier in complier household 


require(stringr)
all.to.report <- rbind(regonly.2.tbl, all.tbl)
all.to.report$rowlabel <- str_replace(all.to.report$rowlabel," ", '\n')
#all.to.report$Comparison <- str_replace(all.to.report$Comparison," ", '\n')
all.to.report$rowtype   <- str_replace(all.to.report$rowtype," ", '\n')

require(ggplot2)
require(scales)

p <- ggplot(all.to.report, aes(x = rowlabel, 
                          y = Difference, 
                          ymin = Difference - 2 * SE, 
                          ymax = Difference + 2 * SE)) + 
  geom_errorbar( width = 0, size = 2, colour = "grey") +
  geom_point(fill = "white", colour = "black", shape = 15, size = 2) + 
  facet_grid(rowtype ~ Comparison, scales = "free_x") +
  scale_x_discrete("") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous("Nonresponse Bias", labels = percent) +
  theme_bw() +
  theme(axis.text=element_text(size=7), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(fill = "white", size = 1 ),
        strip.text.y = element_text(angle = 0)) 


p
ggsave("compliertests.pdf", plot = p, width = 7, height = 5, units = "in")
