set.seed(20160625)
require(dplyr)
require(reshape2)
require(ggplot2)
require(stringr)
require(xtable)
require(sqldf)
load("att.RData")
anes_sample <- read.csv("anes_sample.csv")
colnames(anes_sample) <- tolower(colnames(anes_sample))



# Find compliers and non-complier numbers ---------------------------------


ace.abtid <- list()
ace.abtid[["Full Sample"]] <- unique(short.list.permutation[[1]]$abtid)
ace.abtid[["Main Sample"]] <- unique(short.list.permutation[[1]]$abtid[short.list.permutation[[1]]$oversample==0])

compliance.rate.df <- list()
cace.df <- list() 
for(samp in c("Full Sample","Main Sample")){
  cace.df[[samp]] <- list()
  ok.anes <- anes_sample$col0_abtid %in% ace.abtid[[samp]]
  compliers <- sum(anes_sample$disp[ok.anes]=="Complete")
  noncompliers <- sum(anes_sample$disp[ok.anes]!="Complete")
  compliance.rate.df[[samp]] <- data.frame(samp,compliers,noncompliers)
}
compliance.rate.df <- bind_rows(compliance.rate.df)
rm(ace.abtid)
compliance.rate.df
    

# Compute ITT and CACE ----------------------------------------------------

att.perm <- function(df, include.oversample=F, print.n=F){
  require(dplyr)
  if(!include.oversample) ok <- df$oversample_case==0
  if(include.oversample)  ok <- T
  selected <- as.logical(df$selected) & ok
  not.selected <- !as.logical(df$selected) & ok
  
  e2012g <- mean(df$vf_g2012[selected] - df$vf_g2012[not.selected])
  e2010g <- mean(df$vf_g2010[selected] - df$vf_g2010[not.selected])
  e2008g <- mean(df$vf_g2008[selected] - df$vf_g2008[not.selected])
  e2006g <- mean(df$vf_g2006[selected] - df$vf_g2006[not.selected])
  #Combined <- (e2010g + e2008g + e2006g)/3
  if(print.n)  print(sum(selected + not.selected))
  if(include.oversample) return(data.frame(sample = "Full Sample", e2012g, e2010g, e2008g, e2006g))
  if(!include.oversample) return(data.frame(sample = "Main Sample", e2012g, e2010g, e2008g, e2006g))
}



#%>% mutate(CACE = value / rbeta(length(value), 
#                              compliance.rate.df$compliers[compliance.rate.df$samp == "Main Sample"],
#                              compliance.rate.df$noncompliers[compliance.rate.df$samp == "Main Sample"]))

observed.means <- lapply(short.list.true, att.perm) %>% rbind_all %>%
  melt(measure.vars = c("e2012g","e2010g","e2008g","e2006g")) 

perm.means <- lapply(short.list.permutation, att.perm) %>% rbind_all %>%
  melt(measure.vars = c("e2012g","e2010g","e2008g","e2006g")) 

observed.means <- lapply(short.list.true, att.perm, include.oversample=T) %>% rbind_all %>%
  melt(measure.vars = c("e2012g","e2010g","e2008g","e2006g")) %>%
  rbind(observed.means)

perm.means <- lapply(short.list.permutation, att.perm, include.oversample=T) %>% rbind_all %>%
  melt(measure.vars = c("e2012g","e2010g","e2008g","e2006g")) %>%
  rbind(perm.means)
# Check that they line up
all.equal(perm.means$sample, observed.means$sample)
all.equal(perm.means$variable, observed.means$variable)

perm.means$source <- "permutation"
observed.means$source <- "observed"


perm.means$observed.value <- observed.means$value

require(stringr)
require(dplyr)
observed.means$variable <- str_replace_all(observed.means$variable, pattern = "^e","") %>%
  str_replace_all( pattern = "g$","") %>%
  factor(c(2012,2010,2008,2006))
perm.means$variable <- str_replace_all(perm.means$variable, pattern = "^e","") %>%
  str_replace_all(pattern = "g$","") %>%
  factor(c(2012,2010,2008,2006))


summary.observed.means <- observed.means %>%
  dplyr::group_by(sample,variable) %>%
  dplyr::summarise(truevalue = mean(value), sd = sd(value))

# 
# require(ggplot2)
# pdf("hhitt.pdf", width = 7, height = 4)
# ggplot(perm.means, aes(x=value)) + 
#   theme_bw() + 
#   facet_grid(variable~sample) + 
#   geom_vline(data = summary.observed.means, aes(xintercept=truevalue) , colour = "red", linetype="dashed")  + 
#   geom_vline(xintercept = 0, colour = "black") + 
#   scale_x_continuous("Difference in Means", lim=c(-.08,.08)) + 
#   scale_y_continuous("Density",labels=NULL, breaks = NULL)+
#   geom_density() + 
#   ggtitle("Distribution of Difference of Means \n Under Within-HH Permutations")
# dev.off()

perm.df <- inner_join(perm.means, observed.means, by=c("sample","variable"))

colnames(perm.df) <- str_replace(colnames(perm.df), pattern = ".x",".perm") %>%
  str_replace( pattern = ".y",".obs")

perm.df <- 
  perm.df %>%
  mutate(CATE.est = (value.obs - value.perm) / rbeta(length(value.obs),
         compliance.rate.df$compliers[compliance.rate.df$samp == sample[1]],
         compliance.rate.df$noncompliers[compliance.rate.df$samp == sample[1]])) 

perm.df <- 
  perm.df %>% 
  dplyr::group_by(variable,sample) %>%
  dplyr::summarise(ITT = 100 * mean(value.obs), 
                   p = 100*mean( abs(value.perm) > abs(value.obs)), 
                   CACE = 100 * mean(CATE.est)) %>%
  dplyr::rename(Sample = sample, Election = variable) %>%
  arrange(Election, Sample)

perm.df$Election <- perm.df$Election %>% str_replace_all("[a-z]","")

perm.df <- data.frame(perm.df[, ])
require(xtable) 
print.xtable(xtable(perm.df, digits=1, caption = "Statistics for each election on both the full sample and main sample only. $p$-values refer to the estimated intent-to-treat effects. The 2.5\\%ile and 97.5\\%ile columns refer to those quantiles of the CATE.", label = "itt"), include.rownames = F, file = "table4.tex")

#### NOTE: fix column names for quantile columns and refomat p values (take them off the percent scale) before procedding.


