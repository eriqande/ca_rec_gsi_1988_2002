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