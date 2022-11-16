# This script calculates the effect of treatment on Aristotelian friendships and
# other types of mutual ties for All Terrorism is Local
# By William Minozzi
# Edited by Matthew Simonson
# Last updated 2022-October-24
# Status: Go!

# Libraries -------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(fixest)


# Strings -----------------------------------------------------------------

outcomes <- c("Esteem", "Time", "Non-negative", "Friend", "Aristotelian", "Political")
networks_fpath <- "input_data/edges.rda"
# Prep Network Data -------------------------------------------------------


load(networks_fpath)

# `all_edges` is an edge list (in the form of a data.table) with sender id,
# reciever id, edge type, chapter, and year

all_edges <- copy(edges)

# `sites` is a list 112 of chapter-years (in the form of a data.table) to which
# various calculation will be appended

sites <- as.data.table(all_edges[, unique(cbind(chapter, year))])[
  order(chapter, year)]
sites <- sites[chapter != 15]

# `vertices` is a list of 112 character vectors. Each vector lists the scholars IDs
# present in a chapter-year (site)

vertices <- lapply(1:nrow(sites), function(i) {
  all_edges[chapter == sites$chapter[i] & year == sites$year[i],
    sort(unique(c(ego_id, alt_id)))]
})

# `dyads` is a list of 112 data.tables. Each data table contains a row for each
# unordered pair of scholars in that chapter-year (site). Additional columns
# will be appended specifying whether each type of edge exists between those
# two scholars

dyads <- lapply(vertices, function(v) {
  CJ(v1 = v, v2 = v)[v1 < v2][order(v1, v2)]
})

# merge information on edges into the list of dyad data.tables

dyads <- lapply(1:nrow(sites), function(i) {
  dyads[[i]] <- merge(
    dyads[[i]],
    all_edges[
      type == "time" & chapter == sites$chapter[i] & year == sites$year[i],
      .(v1 = ego_id, v2 = alt_id, time_out = 1)],
    by = c("v1", "v2"), all.x = TRUE, sort = FALSE)
  dyads[[i]] <- merge(
    dyads[[i]],
    all_edges[
      type == "time" & chapter == sites$chapter[i] & year == sites$year[i],
      .(v1 = alt_id, v2 = ego_id, time_in = 1)],
    by = c("v1", "v2"), all.x = TRUE, sort = FALSE)
  dyads[[i]] <- merge(
    dyads[[i]],
    all_edges[
      type == "friend" & chapter == sites$chapter[i] & year == sites$year[i],
      .(v1 = ego_id, v2 = alt_id, friend_out = 1)],
    by = c("v1", "v2"), all.x = TRUE, sort = FALSE)
  dyads[[i]] <- merge(
    dyads[[i]],
    all_edges[
      type == "friend" & chapter == sites$chapter[i] & year == sites$year[i],
      .(v1 = alt_id, v2 = ego_id, friend_in = 1)],
    by = c("v1", "v2"), all.x = TRUE, sort = FALSE)
  dyads[[i]] <- merge(
    dyads[[i]],
    all_edges[
      type == "esteem" & chapter == sites$chapter[i] & year == sites$year[i],
      .(v1 = ego_id, v2 = alt_id, esteem_out = 1)],
    by = c("v1", "v2"), all.x = TRUE, sort = FALSE)
  dyads[[i]] <- merge(
    dyads[[i]],
    all_edges[
      type == "esteem" & chapter == sites$chapter[i] & year == sites$year[i],
      .(v1 = alt_id, v2 = ego_id, esteem_in = 1)],
    by = c("v1", "v2"), all.x = TRUE, sort = FALSE)
  dyads[[i]] <- merge(
    dyads[[i]],
    all_edges[
      type == "negative" & chapter == sites$chapter[i] & year == sites$year[i],
      .(v1 = ego_id, v2 = alt_id, negative_out = 1)],
    by = c("v1", "v2"), all.x = TRUE, sort = FALSE)
  dyads[[i]] <- merge(
    dyads[[i]],
    all_edges[
      type == "negative" & chapter == sites$chapter[i] & year == sites$year[i],
      .(v1 = alt_id, v2 = ego_id, negative_in = 1)],
    by = c("v1", "v2"), all.x = TRUE, sort = FALSE)
  dyads[[i]] <- merge(
    dyads[[i]],
    all_edges[
      type == "political" & chapter == sites$chapter[i] & year == sites$year[i],
      .(v1 = ego_id, v2 = alt_id, political_out = 1)],
    by = c("v1", "v2"), all.x = TRUE, sort = FALSE)
  dyads[[i]] <- merge(
    dyads[[i]],
    all_edges[
      type == "political" & chapter == sites$chapter[i] & year == sites$year[i],
      .(v1 = alt_id, v2 = ego_id, political_in = 1)],
    by = c("v1", "v2"), all.x = TRUE, sort = FALSE)
  dyads[[i]][is.na(time_out), time_out := 0]
  dyads[[i]][is.na(time_in), time_in := 0]
  dyads[[i]][is.na(friend_out), friend_out := 0]
  dyads[[i]][is.na(friend_in), friend_in := 0]
  dyads[[i]][is.na(esteem_out), esteem_out := 0]
  dyads[[i]][is.na(esteem_in), esteem_in := 0]
  dyads[[i]][is.na(negative_out), negative_out := 0]
  dyads[[i]][is.na(negative_in), negative_in := 0]
  dyads[[i]][is.na(political_out), political_out := 0]
  dyads[[i]][is.na(political_in), political_in := 0]
  dyads_opp <- copy(dyads[[i]])
  sw1 <- dyads_opp$v1
  sw2 <- dyads_opp$v2
  dyads_opp[, `:=`(v1 = sw2, v2 = sw1)]
  rbind(dyads[[i]], dyads_opp)[order(v1, v2)]
})

# add a column for Aristotelian friendships and calculate which dyads have them
for (i in 1:length(dyads)) {
  dyads[[i]][,
    aristo := time_out*time_in*
      friend_out*friend_in*
      esteem_out*esteem_in*
      (1-negative_out)*(1-negative_in)]
}

# count the number of Aristotelian friendships in each chapter-year (site)
# and the number of reciprocal ties of other types

aristo_counts <- rbindlist(lapply(1:nrow(sites), function(i) {
  dyads[[i]][, .(
    chapter = sites$chapter[i],
    year = sites$year[i],
    n_aristo = sum(aristo),
    n_recip_time = sum(time_out*time_in),
    n_recip_friend = sum(friend_out*friend_in),
    n_recip_esteem = sum(esteem_out*esteem_out),
    n_recip_political = sum(political_out*political_in),
    n_recip_nonnegative = sum((1-negative_out)*(1-negative_in))
  ), .(id = v1)]
}))

# attach treatment indicators, chapter, year, and number of nodes columns to
# `aristo_counts`
aristo_counts[, treated_chapter := as.numeric(chapter == 12)]
aristo_counts[, treated_year := as.numeric(year == 2016)]
aristo_counts[, treatment := treated_chapter*treated_year]
aristo_counts[, chapter_year := paste(chapter, year)]
sites[, n_nodes := sapply(vertices, length)]
aristo_counts <- merge(aristo_counts, sites[, .(chapter, year, n_nodes)],
  by = c("chapter", "year"))

message("Network data loaded and prepped")
# Models ------------------------------------------------------------------
count_mods <- feglm(c(n_recip_esteem, n_recip_time, n_recip_nonnegative,
                      n_recip_friend, n_aristo, n_recip_political) ~ treatment |
                      chapter + year,
                    data = aristo_counts,
                    cluster = ~chapter_year)

binary_mods <- feglm(c(n_recip_esteem>0, n_recip_time>0, n_recip_nonnegative == n_nodes - 1,
                       n_recip_friend>0, n_aristo>0, n_recip_political>0) ~ treatment |
                       chapter + year,
                     data = aristo_counts,
                     cluster = ~chapter_year)
# Tables ------------------------------------------------------------------

etable(count_mods, headers = outcomes, depvar = F,
       se.below = T)

etable(count_mods, headers = outcomes, depvar = F, replace = T,
       title = "Fixed effects models regressing number of each type of network tie on treatment",
       label = "tab:net_count",
       file = "SI/table_network_count.tex")

etable(binary_mods, headers = outcomes, depvar = F, se.below = T)

etable(binary_mods, headers = outcomes, depvar = F, replace = T,
       title = "Fixed effects models regressing existance of each type of network tie on treatment",
       label = "tab:net_bin",
       file = "SI/table_network_binary.tex")

# Plots -------------------------------------------------------------------

count_plot_tab <- data.table(coefplot(count_mods, only.params = T)$prms)
count_plot_tab[ , outcome := factor(outcomes, levels = rev(outcomes))]
count_plot_tab[ , label := sprintf("%.02f", estimate)]

binary_plot_tab <- data.table(coefplot(binary_mods, only.params = T)$prms)
binary_plot_tab[ , outcome := factor(outcomes, levels = rev(outcomes))]
binary_plot_tab[ , label := sprintf("%.02f", estimate)]


count_plot <- ggplot(count_plot_tab[outcome %like% "^(E|T|F|A)"],
                     aes(estimate, outcome, xmin = ci_low, xmax = ci_high, label = label)) +
  geom_vline(xintercept = 0, linetype = 3, color = "gray") +
  geom_errorbarh(height = 0) + geom_label(size = 2.5) +
  theme_bw() + xlab("Estimate (95% Interval)") + ylab("") +
  ggtitle("Effect on Number of Ties") +
  theme(plot.title = element_text(hjust = .5, size = 10),
        axis.title= element_text(size=8),
        axis.text=element_text(size=8))

binary_plot <- ggplot(binary_plot_tab[outcome %like% "^(E|T|F|A)"],
                      aes(estimate, outcome, xmin = ci_low, xmax = ci_high, label = label)) +
  geom_vline(xintercept = 0, linetype = 3, color = "gray") +
  geom_errorbarh(height = 0) + geom_label(size = 2.5) +
  theme_bw() + xlab("Estimate (95% Interval)") + ylab("") +
  ggtitle("Effect on Pr(Having Any Ties)") +
  theme(plot.title = element_text(hjust = .5, size = 10),
        axis.title= element_text(size=8),
        axis.text=element_text(size=8))

cowplot::plot_grid(count_plot, binary_plot, align = "h")

ggsave("figures/Networks.jpeg", width = 17.8, height = 6, units = "cm")

message("Figure 4 created")

# Placebo Models ----------------------------------------------------------

count_placebos <- rbindlist(lapply(1:nrow(sites), function(i) {
  aristo_counts[, treated_chapter := as.numeric(chapter == sites$chapter[i])]
  aristo_counts[, treated_year := as.numeric(year == sites$year[i])]
  models <- feglm(c(n_recip_esteem, n_recip_time, n_recip_nonnegative,
                    n_recip_friend, n_aristo, n_recip_political) ~ treated_chapter:treated_year |
                    chapter + year,
                  data = aristo_counts,
                  cluster = ~chapter_year)
  cbind(
    data.table(chapter = sites$chapter[i], year = sites$year[i],
               outcome = c("esteem", "time", "nonnegative",
                           "friend", "aristo", "political")),
    t(sapply(models, function(m) coeftable(m)))
  )
}))


binary_placebos <- rbindlist(lapply(1:nrow(sites), function(i) {
  aristo_counts[, treated_chapter := as.numeric(chapter == sites$chapter[i])]
  aristo_counts[, treated_year := as.numeric(year == sites$year[i])]
  models <- feglm(c(n_recip_esteem>0, n_recip_time>0, n_recip_nonnegative == n_nodes - 1,
                    n_recip_friend>0, n_aristo>0, n_recip_political>0) ~ treated_chapter:treated_year |
                    chapter + year,
                  data = aristo_counts,
                  cluster = ~chapter_year)
  cbind(
    data.table(chapter = sites$chapter[i], year = sites$year[i],
               outcome = c("esteem", "time", "nonnegative",
                           "friend", "aristo", "political")),
    t(sapply(models, function(m) coeftable(m)))
  )
}))

# Summarize placebo tests

# calculate z-score for every placebo treatment estimate and cast into a wide
# data.table in which columns are edge types, rows are chapter-years (sites),
# and cell entries are the z-scores

count_placebos_wide <- dcast(
  count_placebos[, .(chapter, year, outcome, z = V3)],
  chapter + year ~ outcome, value.var = "z")

binary_placebos_wide <- dcast(
  binary_placebos[, .(chapter, year, outcome, z = V3)],
  chapter + year ~ outcome, value.var = "z")

# fraction of sites for which ANY z-score is smaller than the
# z-scores of a the site in question


sites$prop_any_count_est_larger <- sapply(1:nrow(sites), function(i) {
  x <- count_placebos_wide[chapter == sites$chapter[i] & year == sites$year[i]]
  count_placebos_wide[!(chapter == sites$chapter[i] & year == sites$year[i]),
                      mean(aristo < x[1, aristo] |
                             esteem < x[1, esteem] |
                             nonnegative < x[1, nonnegative] |
                             friend < x[1, friend] |
                             time < x[1, time])]

})


sites$prop_any_binary_est_larger <- sapply(1:nrow(sites), function(i) {
  x <- binary_placebos_wide[chapter == sites$chapter[i] & year == sites$year[i]]
  binary_placebos_wide[!(chapter == sites$chapter[i] & year == sites$year[i]),
                       mean(aristo < x[1, aristo] |
                              esteem < x[1, esteem] |
                              nonnegative < x[1, nonnegative] |
                              friend < x[1, friend] |
                              time < x[1, time])]

})

# fraction of sites for which ALL z-scores are smaller than the
# z-scores of the site in question
sites$prop_all_count_est_larger <- sapply(1:nrow(sites), function(i) {
  x <- count_placebos_wide[chapter == sites$chapter[i] & year == sites$year[i]]
  count_placebos_wide[!(chapter == sites$chapter[i] & year == sites$year[i]),
                      mean(aristo < x[1, aristo] &
                             esteem < x[1, esteem] &
                             nonnegative < x[1, nonnegative] &
                             friend < x[1, friend] &
                             time < x[1, time])]

})


sites$prop_all_binary_est_larger <- sapply(1:nrow(sites), function(i) {
  x <- binary_placebos_wide[chapter == sites$chapter[i] & year == sites$year[i]]
  binary_placebos_wide[!(chapter == sites$chapter[i] & year == sites$year[i]),
                      mean(aristo < x[1, aristo] &
                             esteem < x[1, esteem] &
                             nonnegative < x[1, nonnegative] &
                             friend < x[1, friend] &
                             time < x[1, time])]

})

message("Placebo models generated")
# Ranking -----------------------------------------------------------------

# this section examines whether the treated site is an outside compared to
# other sites

## Raw data (Aristo ties) ----------------------------------------------------------------

## Does the treated site have more Aristotelian friendships (or more scholars
## with at least one Aristotelian friendship) than other sites?

# mean number of Aristotelian friendships among treated scholars
n_aristo_treated <- aristo_counts[chapter == 12 & year == 2016, mean(n_aristo),
                                 .(chapter, year)][,V1]
# mean number of treated scholars withany Aristotelian friendships
n_any_aristo_treated <- aristo_counts[chapter == 12 & year == 2016,
                                     mean(n_aristo>0), .(chapter, year)][,V1]

# prop. of chapter-years with fewer Aristotelian friendships than treated site
aristo_counts[, mean(n_aristo), .(chapter, year)][
  !(chapter == 12 & year == 2016), mean(V1 < n_aristo_treated)]

# prop. of chapter-years with fewer scholars with ANY Aristotelian friendships
# than treated site
aristo_counts[, mean(n_aristo>0), .(chapter, year)][
  !(chapter == 12 & year == 2016), mean(V1 < n_any_aristo_treated)]

## Model estimates (Aristo ties) -------------------------------------------------

## Does treatment have a larger effect on Aristotelian friendship in the
## model of the observed data compared to the placebo models?

# z-score for effect of treatment on count of Aristotelian friendships
obs_aristo_z_score <- count_placebos[chapter == 12 & year == 2016,
                                .(outcome, V3)][outcome == "aristo", V3]

# prop. of placebos with a smaller z-score than observed (count model)
count_placebos[outcome == "aristo" & !(chapter == 12 & year == 2016),
  mean(V3 < obs_aristo_z_score)]

# z-score for effect of treatment on presence of Aristotelian friendships
obs_aristo_z_score <- binary_placebos[chapter == 12 & year == 2016,
                                .(outcome, V3)][outcome == "aristo", V3]
# prop. of placebos with a smaller z-score than observed (binary model)
binary_placebos[outcome == "aristo" & !(chapter == 12 & year == 2016),
  mean(V3 < obs_aristo_z_score)]

message("All done")
## Model estimates (all recip. ties) ------------------------------------------
##
## Does treatment have a larger effect on any or all of the six types of
## reciprocal ties in the in the model of the observed data compared to the
##  placebo models?

sites[, rank_any_count_est_larger := rank(-prop_any_count_est_larger)]
sites[, rank_all_count_est_larger := rank(-prop_all_count_est_larger)]
sites[chapter == 12 & year == 2016, .(rank_any_count_est_larger, rank_all_count_est_larger)]

sites[, rank_any_binary_est_larger := rank(-prop_any_binary_est_larger)]
sites[, rank_all_binary_est_larger := rank(-prop_all_binary_est_larger)]
sites[chapter == 12 & year == 2016, .(rank_any_binary_est_larger, rank_all_binary_est_larger)]

