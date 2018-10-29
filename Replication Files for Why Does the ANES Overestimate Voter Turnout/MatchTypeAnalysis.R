
require(stringr)
require(dplyr)
sampexp <- read.delim("sampexp_nodupes.txt", stringsAsFactor=F )
str(sampexp)

ok <- sampexp$namematch == 1 & sampexp$dispo == "Complete"

sampexp$birthmatchtype <- ifelse(sampexp$birthyearmatch == "match", "Birth Year Match", NA)
sampexp$birthmatchtype <- ifelse(sampexp$dobmatch == "match", "Date of Birth Match", sampexp$birthmatchtype)
sampexp$birthmatchtype <- ifelse(sampexp$birthyearmatch == "mismatch", "Mismatch", sampexp$birthmatchtype)
sampexp$birthmatchtype <- ifelse(sampexp$birthyearmatch == "missing data", "Missing Data", sampexp$birthmatchtype)

sampexp$birthmatchtype <- factor(sampexp$birthmatchtype, levels = sort(unique(sampexp$birthmatchtype))[c(2,1,3,4)])

sampexp$notatsample <- sampexp$cat.notatsample
sampexp$notatsample[ok & is.na(sampexp$cat.notatsample)] <- 0 

sampexp$notatsample.verbose <- ifelse(sampexp$notatsample == 1, "Found Elsewhere", NA)
sampexp$notatsample.verbose <- ifelse(sampexp$cat.samezip5 == 1 | sampexp$cat.sametown==1, "Matched in Same City", sampexp$notatsample.verbose)
sampexp$notatsample.verbose <- ifelse(sampexp$cat.samezip9 == 1 , "Matched in Same Zip+4", sampexp$notatsample.verbose)
sampexp$notatsample.verbose <- ifelse(sampexp$cat.samestreet == 1 , "Matched to Same Street", sampexp$notatsample.verbose)
sampexp$notatsample.verbose <- ifelse(coalesce(sampexp$altstreet_match,F), "Matched to Alternate Reg Address", sampexp$notatsample.verbose)
sampexp$notatsample.verbose <- ifelse(sampexp$notatsample == 0, "Matched to Sampled Address", sampexp$notatsample.verbose)


sampexp$notatsample.verbose <- factor(sampexp$notatsample.verbose, sort(unique(sampexp$notatsample.verbose)) [rev(c(1,2,5,3,4,6))])
require(xtable)
tab <- addmargins(table( sampexp$notatsample.verbose[ok], sampexp$birthmatchtype[ok]))
attributes(tab)$dimnames <- lapply(attributes(tab)$dimnames, function(i) str_replace(i, "Sum", "Total"))
print(xtable(tab, label = "tab:matchtypetable", 
       caption = "Type of match among ANES respondents. The name of the voter or commercial record always matches the name given to ANES by the respondent.", digits=0), file = "table1.tex")


## Berent et al comparison
ok <- sampexp$namematch == 1 & sampexp$registration==1
sum(ok)/2054
sum(sampexp$weight_ftf[ok])/2054
