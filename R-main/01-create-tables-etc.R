library(dplyr)
library(reshape2)


years <- 1998:2002
names(years) <- years


#### read in the 9642 "good indviduals".  Call them "good_ass"  ####

# read the posteriors
good_inds <- lapply(years, function(x) {
  foo <- paste("CAhistorical", x, "SW_mjport", sep = "_")
  bar <- paste(foo, "goodindivs_repunit_posteriors.txt", sep = "_")
  path <- file.path("stuff_from_crandall/historical_gsi_posteriors", foo, bar)
  read.csv(path)
})%>%
  plyr::ldply(., as.data.frame, .id = "year") %>%
  tbl_df


# to find the maxes we melt and such
meta_dat_cols <- good_inds %>% select(ID:date)
slim <- good_inds %>% select(-(weeks:ValueType))

# now melt the slim one and pick out the max rows by filtering
# on the value being equal to the max (this should almost always be unique)
good_ass <- melt(slim, id.vars = "ID", value.name = "posterior_prob", variable.name = "repunit") %>%
  tbl_df %>%
  group_by(ID) %>%
  filter(posterior_prob == max(posterior_prob)) %>%
  ungroup %>%
  arrange(repunit, posterior_prob, ID) %>%
  inner_join(meta_dat_cols, .)


#### Read in the DFG/CWT data and then make a table #### 
dfg <- read.csv("stuff_from_crandall/OSP_NMFS_GSI_Samples9802_v2_092012mpz.csv", na.strings = c("", "NA")) %>%
  tbl_df

# now just get the ones that have cwts and inner join that to the good_ass and select columns we want
cwt_slim <- dfg %>%
  filter(!is.na(cwtcode)) %>%
  inner_join(., good_ass, by = c("nmfs_id" = "ID")) %>%
  select(repunit, nmfs_id, runame, stockname, posterior_prob) %>%
  arrange(repunit, posterior_prob)


# now, let's enumerate these guys and also compute the range of values of the posterior prob
cwt_table <- cwt_slim %>%
  group_by(repunit, stockname, runame) %>%
  summarise(n = n(), pp_low = min(posterior_prob), pp_hi = max(posterior_prob)) %>%
  ungroup %>%
  arrange(repunit, desc(n))

write.csv(cwt_table, file = "gsi_vs_cwt_table.csv", quote = FALSE, row.names = FALSE)


#### Compare assignments of "good_inds" to those that were tossed as "Low-confidence" assignments ####


# first, read in the data that we have on the "doubtful_assignments".
# These are not the ones that had sketchy data quality (wonky internal HZ).
# These are the ones that had z-scores that were too large and also assignments
# less than 90% OR data missing from more than 5 loci.  (Presumably, this is what 
# Eric C did.)

doubtful_inds <- lapply(years, function(x) {
  foo <- paste("CAhistorical", x, "SW_mjport", sep = "_")
  bar <- "doubtful_assignment.txt"
  path <- file.path("stuff_from_crandall/GSIhistorical_edc", foo, bar)
  read.table(path, header = TRUE, sep = "\t")
})%>%
  plyr::ldply(., as.data.frame, .id = "year") %>%
  tbl_df

# now we count up the assignments in the good inds and sort them and toss out
# factor levels that were not observed:
ga_tab <- table(good_ass$repunit) %>% 
  sort(., decreasing = TRUE)  %>%
  unclass
ga_tab <- ga_tab[ga_tab > 0]  # toss out the zeros

di_tab <- table(doubtful_inds$Max.RepU.1)

# make our contingency matrix of counts.  First column is the good_inds and
# second are the doubtfuls
cm <- cbind(ga_tab, di_tab[names(ga_tab)])
cm[is.na(cm)] <- 0

# then, when you do this we see the distribution of 
# assignments are not significantly different between good and doubtful individuals
CHISQ <- chisq.test(cm, simulate.p.value = TRUE, B = 10^6)
