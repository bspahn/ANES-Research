

require(doParallel)
require(foreach)
require(iterators)
require(MASS)
require(dplyr)
require(xtable)
require(knitr)



anes_sample <- read.csv("anes_sample.csv")


## Make sampexp have no ll records, at least for some of these analyses
sampexp <- read.delim("sampexp_nocatll.txt",  na.strings = c("NA",""))

sampexp <- sampexp[!is.na(sampexp$oversamp),]

sampexp$complier <- ifelse(sampexp$dispo=="Complete", 1, 0)

namematch.abtid <- unique(sampexp$col0_abtid[sampexp$namematch==1 & sampexp$dispo=="Complete"])


bad.dispo <- c("Address does not exist", "Out of sample area", "Address does not have a permanently occupied household", "No adult citizen", "OS only, no Hispanic/AA")


#### This is for the match rate footnote
sampexp %>% filter(!dispo %in% bad.dispo) %>% group_by(col0_abtid, dispo) %>% dplyr::summarise(matched = any(!is.na(vendorid))) %>% ungroup %>% group_by(complete = dispo == "Complete") %>% dplyr::summarise(mean(matched))


vh.cols <-  c("e2012g", "e2010g", "e2008g", "e2006g")
vh.labs <-  c("2012  Turnout", "2010  Turnout", "2008  Turnout", "2006  Turnout")

regvh.cols <-  c("registration","e2012g", "e2010g", "e2008g", "e2006g")
regvh.labs <-  c("2012\nRegistration","2012\nTurnout", "2010\nTurnout", "2008\nTurnout", "2006\nTurnout")


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

hh <- list()
hh$"All Listed Persons" <- sampexp %>% 
  filter(!dispo %in% bad.dispo)  %>%
  group_by(col0_abtid, complier, oversamp) %>% 
  dplyr::summarise(number.of.people = sum(!is.na(vendorid)), 
                   e2012g = coalesce(mean(e2012g),0), 
                   e2010g = coalesce(mean(e2010g),0), 
                   e2008g = coalesce(mean(e2008g),0), 
                   e2006g = coalesce(mean(e2006g),0),
                   registration = coalesce(mean(registration),0)  ) %>%
  filter(number.of.people < 4) %>% 
  ungroup 


hh$"Registered Only" <- sampexp %>% 
  filter(!dispo %in% bad.dispo & registration == 1)  %>%
  group_by(col0_abtid, complier, oversamp) %>% 
  dplyr::summarise(number.of.people = sum(!is.na(vendorid)), 
                   e2012g = coalesce(mean(e2012g),0), 
                   e2010g = coalesce(mean(e2010g),0), 
                   e2008g = coalesce(mean(e2008g),0), 
                   e2006g = coalesce(mean(e2006g),0),
                   registration = coalesce(mean(registration),0)  ) %>%
  filter(number.of.people < 4 & number.of.people > 0) %>% 
  ungroup 


require(Matching)
matches <- list()

make.matching.row <- function(m, sample, election, registration){
  m.est <- m$e
  data.frame(Difference = m$est, SE = m$se, sample, election, registration)
}




row.counter <- 0
for(i in 1:length(regvh.cols)){
  vh <- regvh.cols[i]
  for(reg in names(hh)){
    row.counter <- row.counter + 1
    ok <- hh[[reg]]$oversamp == 0
    matches[[row.counter]] <- 
      Match(Y = hh[[reg]][[vh]][ok], Tr = hh[[reg]]$complier[ok], X = hh[[reg]]$number.of.people[ok]) %>% 
      make.matching.row("Main Sample", regvh.labs[i], reg)
    
    row.counter <- row.counter + 1
    ok <- T
    matches[[row.counter]] <- 
      Match(Y = hh[[reg]][[vh]][ok], Tr = hh[[reg]]$complier[ok], X = hh[[reg]]$number.of.people[ok]) %>% 
      make.matching.row("Full Sample", regvh.labs[i], reg)
  }
}

matches <- bind_rows(matches) %>% filter(Difference != 0)
matches

require(gdata)
matches$election <- reorder.factor(factor(matches$election), new.order = c(1,2,3,5,4))

require(stringr)
matches.final <- matches %>% filter(str_detect(sample,"Main"))

require(ggplot2)
require(scales)
require(extrafont)
pdf("figure1.pdf", width = 7, height = 5, family = "Verdana")

ggplot(matches.final, aes(x = election, 
                         y = Difference, 
                         ymin = Difference - 2 * SE, 
                         ymax = Difference + 2 * SE)) + 
  geom_errorbar( width = 0, size = 2, colour = "grey") +
  geom_point(fill = "white", colour = "black", shape = 15, size = 2) + 
  facet_grid( ~ registration, scales = "free_x") +
  scale_x_discrete("") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous("Nonresponse Bias", labels = percent) +
  theme_bw() +
  theme(axis.text=element_text(size=7), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(fill = "white", size = 1 ),
        strip.text.y = element_text(angle = 0)) 


dev.off()
embed_fonts("figure1.pdf" )
ggsave("figure1.pdf", plot = p, width = 7, height = 5, units = "in")

