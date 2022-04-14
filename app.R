library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(fmsb)
library(scales)
library(stringr)
library(shiny)
library(ggmap)


safe_divide <- function(a, b) {
  return (ifelse(b == 0, 0, a/b))
}

# takes in a number and number of decimal places to round to, returns a percentage string
# e.g. to_pctg(0.75229, 2) returns "75.23%"
to_pctg <- function(x, dp) {
  val_pctg = round(x * 100, dp)
  return (paste0(val_pctg, "%"))
}


# 0-3 -> Low preference
# 4-7 -> Mid preference
# 8-10 -> High preference
pref_to_category <- function(num) {
  if (num <= 3) {
    return ("Low")
  } else if (num <= 7) {
    return ("Mid")
  } else {
    return ("High")
  }
}
pref_to_category_vec <- Vectorize(pref_to_category)





setwd("data_files")

# ============== info on each participant ==============
participants_data <- read.csv(paste0("participants_data", ".csv"))

# ============== at signup ==============
look_for_in_opp_sex_at_signup <- read.csv(paste0("look_for_in_opp_sex_at_signup", ".csv"))
# (wave 6-9 were told to rate each on a scale of 1-10, the other waves were given 100 points, to distribute amongst the 6 attributes)

others_look_for_in_opp_sex_at_signup <- read.csv(paste0("others_look_for_in_opp_sex_at_signup", ".csv"))
# (wave 6-9 were told to rate each on a scale of 1-10, the other waves were given 100 points, to distribute amongst the 6 attributes) 
# "what you think others look for in the opposite sex?"

opp_sex_looks_for_in_date_at_signup <- read.csv(paste0("opp_sex_looks_for_in_date_at_signup", ".csv"))
# (wave 6-9 were told to rate each on a scale of 1-10, the other waves were given 100 points, to distribute amongst the 6 attributes)
# "what you think the opposite sex looks for"

self_perception_at_signup <- read.csv(paste0("self_perception_at_signup", ".csv"))
# (each attribute rated 1-10)

others_perception_of_self_at_signup <- read.csv(paste0("others_perception_of_self_at_signup", ".csv"))
# (each attribute rated 1-10)


# ============== halfway through event ==============
look_for_in_opp_sex_halfway_thru_event <- read.csv(paste0("look_for_in_opp_sex_halfway_thru_event", ".csv"))
# (wave 6-9 were told to rate each on a scale of 1-10, the other waves were given 100 points, to distribute amongst the 6 attributes)

self_perception_halfway_thru_event <- read.csv(paste0("self_perception_halfway_thru_event", ".csv"))
# (each attribute rated 1-10)

# ============== result of dates ==============
dates_results <- read.csv(paste0("dates_results", ".csv"))

# ============== day after event ==============
opinion_on_day_aft_event <- read.csv(paste0("opinion_on_day_aft_event", ".csv"))

val_of_attributes_in_decision_making_day_after_event <- read.csv(paste0("val_of_attributes_in_decision_making_day_after_event", ".csv"))
# (100 points to distribute amongst the 6 attributes)

look_for_in_opp_sex_day_after_event <- read.csv(paste0("look_for_in_opp_sex_day_after_event", ".csv"))
# (100 points to distribute amongst the 6 attributes)

others_look_for_in_opp_sex_day_after_event <- read.csv(paste0("others_look_for_in_opp_sex_day_after_event", ".csv"))
# (100 points to distribute amongst the 6 attributes)

opp_sex_looks_for_in_date_day_after_event <- read.csv(paste0("opp_sex_looks_for_in_date_day_after_event", ".csv"))
# (100 points to distribute amongst the 6 attributes)

self_perception_day_after_event <- read.csv(paste0("self_perception_day_after_event", ".csv"))
# (100 points to distribute amongst the 6 attributes)

others_perception_of_self_day_after_event <- read.csv(paste0("others_perception_of_self_day_after_event", ".csv"))
# (100 points to distribute amongst the 6 attributes)


# ============== weeks after event ==============
three_weeks_results <- read.csv(paste0("three_weeks_results", ".csv"))

look_for_in_opp_sex_weeks_after_event <- read.csv(paste0("look_for_in_opp_sex_weeks_after_event", ".csv"))
# (100 points to distribute amongst the 6 attributes)

val_of_attributes_in_decision_making_weeks_after_event <- read.csv(paste0("val_of_attributes_in_decision_making_weeks_after_event", ".csv"))
# (100 points to distribute amongst the 6 attributes)

others_look_for_in_opp_sex_weeks_after_event <- read.csv(paste0("others_look_for_in_opp_sex_weeks_after_event", ".csv"))
# (each attribute rated 1-10)

opp_sex_looks_for_in_date_weeks_after_event <- read.csv(paste0("opp_sex_looks_for_in_date_weeks_after_event", ".csv"))
# (each attribute rated 1-10)

self_perception_weeks_after_event <- read.csv(paste0("self_perception_weeks_after_event", ".csv"))
# (each attribute rated 1-10)

others_perception_of_self_weeks_after_event <- read.csv(paste0("others_perception_of_self_weeks_after_event", ".csv"))
# (each attribute rated 1-10)


# ============== general data ==============
matches_general <- read.csv(paste0("matches_general", ".csv"))






# function that removes rows where all columns are NA
remove_six_attrs_all_na <- function(df) {
  return (df %>% filter(!(is.na(attractiveness) & is.na(sincerity) & is.na(intelligence) & is.na(fun) & is.na(ambition) & is.na(shared_interests))))
}


# for some dataframes:
# wave 6-9 were told to rate each on a scale of 1-10, the other waves were given 100 points, to distribute amongst the 6 attributes
# idea: for every row, every rating, we turn its rating into a fraction of the total, and multiply by 100
standardise_ratings <- function(df) {
  
  attrs <- c("attractiveness", "sincerity", "intelligence", "fun", "ambition", "shared_interests")
  
  for (attr in attrs) {
    df <- (df %>% mutate(!!as.symbol(attr) := 100 * !!as.symbol(attr) / (attractiveness+sincerity+intelligence+fun+ambition+shared_interests)))
  }
  
  return (df)
}


# return summary of mean for the six attributes
get_mean_attributes_ratings <- function(df) {
  df <- (df
         %>% group_by(is_male)
         %>% summarise(Attractiveness = mean(attractiveness),
                       Sincerity = mean(sincerity),
                       Intelligence = mean(intelligence),
                       Fun = mean(fun),
                       Ambition = mean(ambition),
                       "Shared Interests" = mean(shared_interests))
         %>% mutate(gender = ifelse(is_male, "Male", "Female"))
         %>% dplyr::select(-is_male))
  
  return (df)
}


cleanup_get_summary <- function(df) {
  return (df 
          %>% remove_six_attrs_all_na() 
          %>% replace(is.na(.), 0) 
          %>% standardise_ratings() 
          %>% get_mean_attributes_ratings() 
          %>% gather(Attribute, Score, -gender))
}

get_plot_from_six_attrs_summary <- function(df, title) {
  return (df %>% ggplot(aes(x = Attribute, y = Score)) 
          + geom_bar(stat = "identity", color = "black") 
          + ylim(0, 40)
          + labs(title = title, x = "Attribute", y = "Average rating (sums to 100)") 
          + facet_wrap(~gender))
}






# 1) Chart to show proportion of people interested based on interest level in some activity


# options
# =======
# sports: Playing sports/ athletics
# tvsports: Watching sports
# exersice: Body building/exercising
# dining: Dining out
# museums: Museums/galleries
# art: Art
# hiking:  Hiking/camping
# gaming: Gaming
# clubbing: Dancing/clubbing
# reading: Reading
# tv: Watching TV
# theater: Theater
# movies: Movies
# concerts: Going to concerts
# music: Music
# shopping: Shopping
# yoga: Yoga/meditation

# given your attribute, attribute of another, and whether you're male
# returns a chart showing the proportion of matches
get_attributes_comparison <- function(attr, other_attr, is_male) {
  
  # # e.g.
  # attr = "art"
  # other_attr = "art"
  is_male <- as.logical(is_male)
  
  person_data <- participants_data
  colnames(person_data) <- paste0("person_", colnames(person_data))
  
  partner_data <- participants_data
  colnames(partner_data) <- paste0("partner_", colnames(partner_data))
  
  data_with_attributes = (dates_results 
                          %>% merge(person_data, by.x = "iid", by.y = "person_iid", all.x = TRUE) 
                          %>% merge(partner_data, by.x = "partner_id", by.y = "partner_iid", all.x = TRUE))
  
  # consider from the partner's POV, since we want to use the "would_meet_partner_again" attribute
  attr_col_name = paste0("partner_", attr)
  other_attr_col_name = paste0("person_", other_attr)
  
  aggr_attr_col_name <- paste0(attr_col_name, "_agg")
  aggr_other_attr_col_name <- paste0(other_attr_col_name, "_agg")
  
  ordering <- c("Low", "Mid", "High")
  
  if (is_male) {
    data_with_attributes <- data_with_attributes %>% filter(person_is_male == TRUE)
  } else {
    data_with_attributes <- data_with_attributes %>% filter(person_is_male == FALSE)
  }
  
  attributes_comparison <- (data_with_attributes
                            %>% filter(!is.na(!!as.symbol(attr_col_name)) & !is.na(!!as.symbol(other_attr_col_name)))
                            %>% mutate(!!aggr_attr_col_name := pref_to_category_vec(!!as.symbol(attr_col_name)),
                                       !!aggr_other_attr_col_name := pref_to_category_vec(!!as.symbol(other_attr_col_name)))
                            %>% group_by_at(c(aggr_attr_col_name, aggr_other_attr_col_name))
                            %>% summarise(matches_where_partner_is_interested = sum(would_meet_partner_again),
                                          total_matches = n(),
                                          success_rate = safe_divide(matches_where_partner_is_interested, total_matches),
                                          success_rate_str = to_pctg(success_rate, 3))
                            %>% arrange(factor(!!as.symbol(aggr_attr_col_name), levels = ordering),
                                        factor(!!as.symbol(aggr_other_attr_col_name), levels = ordering))
  )
  
  # facet_var = as.formula(paste0("~", aggr_attr_col_name))
  
  graph_title <- paste0("Your level of interest in ", attr)
  x_axis_label <- paste0("Their level of interest in ", other_attr)
  y_axis_label <- "Probability of a match"
  
  attributes_comparison_graph <- (attributes_comparison 
                                  %>% mutate(grouping = factor(!!as.symbol(aggr_attr_col_name), levels = ordering))  # create a new column to order Low Mid High
                                  %>% ggplot(aes(x = reorder(!!as.symbol(aggr_other_attr_col_name), success_rate), # reorder within each facet
                                                 y = success_rate,
                                                 fill = success_rate)) 
                                  + geom_bar(stat = "identity", color = "black")
                                  + facet_wrap(.~grouping)  # order the facets based on Low, Mid, High
                                  + ggtitle(graph_title)
                                  + labs(x = x_axis_label, y = y_axis_label)
                                  + theme(plot.title = element_text(hjust = 0.5)) 
                                  + scale_fill_gradient(low = "#FFDAF4", high = "#FE58CD")
                                  
  )
  
  return (attributes_comparison_graph)
}





# 2) Spider chart to show what people look for / what people think others look for


# =================== WHAT DO PEOPLE LOOK FOR IN THE OPPOSITE SEX? AND WHAT DO THEY THINK THE OPPOSITE SEX LOOKS FOR? ===================


# ============ BEFORE EVENT ============ 

look_for_in_opp_sex_before_event_summary <- cleanup_get_summary(look_for_in_opp_sex_at_signup)
opp_sex_look_for_in_date_before_event_summary <- cleanup_get_summary(look_for_in_opp_sex_at_signup)


# ============ HALFWAY THROUGH EVENT ============ 
look_for_in_opp_sex_halfway_thru_event_summary <- cleanup_get_summary(look_for_in_opp_sex_halfway_thru_event)


# ============ DAY AFTER EVENT ============ 
look_for_in_opp_sex_day_after_event_summary <- cleanup_get_summary(look_for_in_opp_sex_day_after_event)

opp_sex_looks_for_in_date_day_after_event_summary <- cleanup_get_summary(opp_sex_looks_for_in_date_day_after_event)


# ============ WEEKS AFTER EVENT ============ 
look_for_in_opp_sex_weeks_after_event_summary <- cleanup_get_summary(look_for_in_opp_sex_weeks_after_event)

opp_sex_looks_for_in_date_weeks_after_event_summary <- cleanup_get_summary(opp_sex_looks_for_in_date_weeks_after_event)


# ============ VISUALISATIONS ============


# UNUSED PLOTS

# ====== what people look for? ====== 
before_event_look_for <- get_plot_from_six_attrs_summary(look_for_in_opp_sex_before_event_summary, 
                                                         "What each gender looks for in the opposite sex")


during_event_look_for <- get_plot_from_six_attrs_summary(look_for_in_opp_sex_halfway_thru_event_summary, 
                                                         "What each gender looks for in the opposite sex, halfway through a dating event")

after_event_look_for <- get_plot_from_six_attrs_summary(look_for_in_opp_sex_day_after_event_summary, 
                                                        "What each gender looks for in the opposite sex, the day after a dating event") 

weeks_after_event_look_for <- get_plot_from_six_attrs_summary(look_for_in_opp_sex_weeks_after_event_summary,
                                                              "What each gender looks for in the opposite sex, weeks after a dating event")

# ====== what people think the other gender looks for? ====== 

other_sex_before_event_look_for <- get_plot_from_six_attrs_summary(opp_sex_look_for_in_date_before_event_summary,
                                                                   "What people think the other gender looks for in the opposite sex") 


other_sex_after_event_look_for <- get_plot_from_six_attrs_summary(opp_sex_looks_for_in_date_day_after_event_summary,
                                                                  "What people think the other gender looks for in the opposite sex, the day after a dating event")

other_sex_weeks_after_event_look_for <- get_plot_from_six_attrs_summary(opp_sex_looks_for_in_date_weeks_after_event_summary,
                                                                        "What people think the other gender looks for in the opposite sex, weeks after a dating event")







spider_chart_look <- function(df, title) {
  
  df <- df %>% spread(Attribute, Score) %>% remove_rownames %>% column_to_rownames(var="gender")
  
  colnames(df) <- c("avg_attractiveness", "avg_sincerity", "avg_intelligence", "avg_fun", "avg_ambition", "avg_shared_interests")
  
  rownames(df) <- c("Females", "Males")
  
  df <- rbind(rep(30,6), rep(0,6), df)
  
  colors_border <- c( rgb(0.8,0.2,0.5,0.9), rgb(0.2,0.5,0.5,0.9) )
  colors_in <- c( rgb(0.8,0.2,0.5,0.4), rgb(0.2,0.5,0.5,0.4) )
  
  radarchart(df,
             title = title,
             axistype = 1, 
             pcol = colors_border, 
             pfcol = colors_in, 
             plwd = 4 , 
             plty = 1, 
             vlabels = c("Attractiveness", "Sincerity", "Intelligence", "Fun", "Ambition", "Shared Interests"),
             cglcol = "grey", cglty=1, axislabcol="grey",
             caxislabels = seq(min(df), max(df), 5),
             seg = length(seq(min(df), max(df), 5)) - 1,
             cglwd = 0.8, 
             vlcex = 0.8,
  )
  
  legend(x = 1.7, y = 0.3, legend = rownames(df[-c(1,2),]), bty = "n", pch=20, col = colors_in , text.col = "black", cex = 1.0, pt.cex = 3)
  
}

spider_chart_look(opp_sex_look_for_in_date_before_event_summary, "What ___ think the opposite sex looks for in a date")






# 3) What kind of matches will they get based on their own rating?


match.probability <- function(is_female, as, sy, ie, fn, an, ss) {
  as.level <- cut(as, breaks = c(-1, 20, 40, 60, 101))
  sy.level <- cut(sy, breaks = c(-1, 16, 32, 48, 101))
  ie.level <- cut(ie, breaks = c(-1, 11, 22, 33, 101))
  fn.level <- cut(fn , breaks = c(-1, 8, 16, 24, 101))
  an.level <- cut(an, breaks = c(-1, 7, 14, 21, 101))
  ss.level <- cut(ss , breaks = c(-1, 13, 26, 39, 101))
  
  df <- (look_for_in_opp_sex_weeks_after_event 
         %>% remove_six_attrs_all_na() 
         %>% replace(is.na(.), 0) 
         %>% standardise_ratings()
         %>% mutate(attractiveness.level = cut(attractiveness, breaks = c(-1, 20, 40, 60, 101)),
                    sincerity.level = cut(sincerity, breaks = c(-1, 16, 32, 48, 101)),
                    intelligence.level = cut(intelligence, breaks = c(-1, 11, 22, 33, 101)),
                    fun.level = cut(fun, breaks = c(-1, 8, 16, 24, 101)),
                    ambition.level = cut(ambition, breaks = c(-1, 7, 14, 21, 101)),
                    shared.interests.level = cut(shared_interests , breaks = c(-1, 13, 26, 39, 101)))
  )
  
  result <- (df 
             %>% filter(is_male == as.symbol(is_female)) 
             %>% filter(attractiveness.level == as.level) 
             %>% filter(sincerity.level == sy.level) 
             %>% filter(intelligence.level == ie.level) 
             %>% filter(fun.level == fn.level) 
             %>% filter(ambition.level == an.level) 
             %>% filter(shared.interests.level == ss.level))
  
  probability.of.match <- nrow(result) / nrow(df)
  
  return(probability.of.match)
}

match.probability(is_female = FALSE, 10, 20, 15, 20, 15, 20)






# 4) Probability of match based on some person's characteristic


person_data <- participants_data
colnames(person_data) <- paste0("person_", colnames(person_data))

partner_data <- participants_data
colnames(partner_data) <- paste0("partner_", colnames(partner_data))

data_with_attributes = (dates_results 
                        %>% merge(person_data, by.x = "iid", by.y = "person_iid", all.x = TRUE) 
                        %>% merge(partner_data, by.x = "partner_id", by.y = "partner_iid", all.x = TRUE))


# ============ Field of study ============
field_of_study_combos <- (data_with_attributes
                          %>% group_by(person_field_of_study_category, partner_field_of_study_category)
                          %>% summarise(Success_Matches = sum(match),
                                        Total_Num = n(),
                                        Probability_of_Match = Success_Matches / Total_Num))

field_of_study_combos <- field_of_study_combos[complete.cases(field_of_study_combos), ]


# ============ Race ============
race_combos <- (data_with_attributes
                %>% group_by(person_race, partner_race)
                %>% summarise(Success_Matches = sum(match),
                              Total_Num = n(),
                              Probability_of_Match = Success_Matches / Total_Num))

race_combos <- race_combos[complete.cases(race_combos), ]


# ============ Goal of participating ============
goal_of_participating_combos <- (data_with_attributes
                                 %>% group_by(person_goal_of_participating, partner_goal_of_participating)
                                 %>% summarise(Success_Matches = sum(match),
                                               Total_Num = n(),
                                               Probability_of_Match = Success_Matches / Total_Num))

goal_of_participating_combos <- goal_of_participating_combos[complete.cases(goal_of_participating_combos), ]



# ============ Frequency of going out ============
freq_of_going_out_combos <- (data_with_attributes
                             %>% group_by(person_freq_of_going_out, partner_freq_of_going_out)
                             %>% summarise(Success_Matches = sum(match),
                                           Total_Num = n(),
                                           Probability_of_Match = Success_Matches / Total_Num))

freq_of_going_out_combos <- freq_of_going_out_combos[complete.cases(freq_of_going_out_combos), ]



# ============ Frequency of dates ============
freq_of_dates_combos <- (data_with_attributes
                         %>% group_by(person_freq_of_going_on_dates, partner_freq_of_going_on_dates)
                         %>% summarise(Success_Matches = sum(match),
                                       Total_Num = n(),
                                       Probability_of_Match = Success_Matches / Total_Num))

freq_of_dates_combos <- freq_of_dates_combos[complete.cases(freq_of_dates_combos), ]




get_chars_pie_chart <- function(df, person_char, partner_char) {
  df_intermediate <- (df %>% filter_at(1, all_vars(. == person_char)) 
                      %>% filter_at(2, all_vars(. == partner_char))
                      %>% mutate(Failure_Matches = Total_Num - Success_Matches)
                      %>% ungroup()
                      %>% select(Failure_Matches, Success_Matches)
                      %>% rename("Failure" = Failure_Matches, "Success" = Success_Matches)
                      %>% pivot_longer(cols = c("Failure", "Success"), names_to = "Match Result", values_to = "Values"))
  
  if (nrow(df_intermediate) == 0) {
    df_intermediate <- data.frame(c("Failure", "Success"), c(0, 0))
    colnames(df_intermediate) <- c("Match Result", "Values")
  }
  
  tot_num <- sum(df_intermediate$Values)
  if (tot_num == 0) {
    pctgs <-  c("0%", "0%")
  } else {
    pctgs <- paste0(round(df_intermediate$Values * 100 / tot_num, 3), "%")
  }
  
  pie <- df_intermediate %>% ggplot(aes(x = "", y = Values, fill = !!as.symbol("Match Result"))) +
    geom_bar(stat="identity", width = 1, color = "black") +
    geom_text(aes(label = pctgs), position = position_stack(vjust = 0.5), col = "black") +
    coord_polar("y", start = 0) +
    labs(title = paste0("n = ", tot_num)) +
    scale_fill_manual(values = c("#FF2D00", "#9DFC92"))  # red, green
  
  return (pie)
}


# get_chars_pie_chart(field_of_study_combos, "Business/Econ/Finance", "Business/Econ/Finance")





# 5) For successful dates vs non-successful dates, what did people think about the other person's characteristics?


failed_vs_success_matches <- function() {
  
  results <- (dates_results 
              %>% filter(!(is.na(partner_attractiveness) 
                           | is.na(partner_sincerity) 
                           | is.na(partner_intelligence) 
                           | is.na(partner_fun) 
                           | is.na(partner_ambition) 
                           | is.na(partner_shared_interests)
                           | is.na(liking_towards_partner)))
              %>% group_by(match)
              %>% summarise(Attractiveness = mean(partner_attractiveness),
                            Sincerity = mean(partner_sincerity),
                            Intelligence = mean(partner_intelligence),
                            Fun = mean(partner_fun),
                            Ambition = mean(partner_ambition),
                            "Shared Interests" = mean(partner_shared_interests),
                            "Overall Liking" = mean(liking_towards_partner))
              %>% mutate(match = ifelse(match, "Successful Matches", "Failed Matches")) 
              %>% gather(Attribute, Score, -match)
  )
  
  return (results %>% ggplot(aes(x = Attribute, y = Score, fill = Score)) + geom_bar(stat = "identity", color = "black") + scale_fill_gradient(low = "#FFDAF4", high = "#FE58CD") + facet_wrap(~match))
  
}




# SHINY

# some variables to use
options_of_characteristics = c(
  "Playing sports/ athletics" = "sports", 
  "Watching sports" = "tvsports",
  "Body building/exercising" = "exercise",
  "Dining out" = "dining",
  "Museums/galleries" = "museums",
  "Art" = "art",
  "Hiking/camping" = "hiking",
  "Gaming" = "gaming",
  "Dancing/clubbing" = "clubbing",
  "Reading" = "reading",
  "Watching TV" = "tv",
  "Theater" = "theater",
  "Movies" = "movies",
  "Going to concerts" = "concerts",
  "Music" = "music",
  "Shopping" = "shopping",
  "Yoga/meditation" = "yoga"
)

possible_person_field_of_study <- unique(field_of_study_combos$person_field_of_study_category)
possible_partner_field_of_study <- unique(field_of_study_combos$partner_field_of_study_category)
possible_person_race <- unique(race_combos$person_race)
possible_partner_race <- unique(race_combos$partner_race)
possible_person_participating_goal <- unique(goal_of_participating_combos$person_goal_of_participating)
possible_partner_participating_goal <- unique(goal_of_participating_combos$partner_goal_of_participating)
possible_person_go_out_freq <- unique(freq_of_going_out_combos$person_freq_of_going_out)
possible_partner_go_out_freq <- unique(freq_of_going_out_combos$partner_freq_of_going_out)
possible_person_date_freq <- unique(freq_of_dates_combos$person_freq_of_going_on_dates)
possible_partner_date_freq <- unique(freq_of_dates_combos$partner_freq_of_going_on_dates)



# =================================

add_age <- function(df) {
  return (df 
          %>% mutate(age = participants_data$age)
          %>% na.omit()
          %>% mutate(age = ifelse(
            between(age, 18, 25), "18-25", ifelse(between(age, 26, 35), "26-35", "36-55")
          ))
  )
}


look_for_in_opp_sex_at_signup_w_age <- add_age(look_for_in_opp_sex_at_signup)
look_for_in_opp_sex_day_after_event_w_age <- add_age(look_for_in_opp_sex_day_after_event)
look_for_in_opp_sex_weeks_after_event_w_age <- add_age(look_for_in_opp_sex_weeks_after_event)



# =================================

# fourth tab

match_prob_to_heatmap <- function(df, title) {
  
  for (i in 1:nrow(df)) {
    df[i, 1] <- str_wrap(df[i, 1], width = 10)
  }
  
  return (ggplot(df, aes(!!as.symbol(colnames(df)[1]), !!as.symbol(colnames(df)[2]))) 
          + geom_tile(aes(fill = Probability_of_Match), color = "black")
          + xlab("You")
          + ylab("Your partner")
          + scale_fill_gradient(name = "Probability of matching", low = "#FFDAF4", high = "#FE58CD")
          + labs(title = title)
          + theme(text = element_text(size = 20))
  )
  
}






# APP OVERVIEW

num_users <- nrow(participants_data)

gender_stats <- (participants_data %>% filter(!is.na(is_male))
                 %>% group_by(is_male) %>% summarise("Number of Users" = n()) 
                 %>% mutate("Gender" = ifelse(is_male, "Male", "Female")) 
                 %>% select("Gender", "Number of Users"))


field_of_study_stats <- (participants_data %>% filter(!is.na(field_of_study_category))
                         %>% group_by(field_of_study_category) %>% summarise("Number of Users" = n())
                         %>% rename("Field of Study" = field_of_study_category)
)

race_stats <- (participants_data %>% filter(!is.na(race))
               %>% group_by(race) %>% summarise("Number of Users" = n())
               %>% rename("Race" = race)
)

goal_of_participating_stats <- (participants_data %>% filter(!is.na(goal_of_participating))
                                %>% group_by(goal_of_participating) %>% summarise("Number of Users" = n())
                                %>% rename("Goal of Participating" = goal_of_participating)
)
go_out_freq_stats <- (participants_data %>% filter(!is.na(freq_of_going_out))
                      %>% group_by(freq_of_going_out) %>% summarise("Number of Users" = n())
                      %>% rename("Frequency of going out" = freq_of_going_out)
)

go_on_date_freq_stats <- (participants_data %>% filter(!is.na(freq_of_going_on_dates))
                          %>% group_by(freq_of_going_on_dates) %>% summarise("Number of Users" = n())
                          %>% rename("Frequency of going on dates" = freq_of_going_on_dates)
)

intended_career_stats <- (participants_data %>% filter(!is.na(intended_career_category))
                          %>% group_by(intended_career_category) %>% summarise("Number of Users" = n())
                          %>% rename("Intended Career Category" = intended_career_category)
)




get_basic_stats_pie_chart <- function(df) {
  
  first_col <- names(df)[1]
  n <- sum(intended_career_stats$`Number of Users`)
  
  df <- df %>% mutate(labels_for_legend = paste0(!!as.symbol(first_col), " (", percent(!!as.symbol("Number of Users") / n), ")")) %>% arrange(-`Number of Users`)
  
  pctgs <- paste0(round(intended_career_stats$`Number of Users` * 100 / n, 1), "%")
  
  pie <- (ggplot(df, aes(x = "", y = `Number of Users`, fill = reorder(labels_for_legend, -`Number of Users`))) 
          + geom_bar(width = 1, stat = "identity", color = "black") 
          + coord_polar("y", start = 0)
          + labs(fill = NULL, title = paste0(first_col, ", n = ", n))
          + theme(legend.text = element_text(size = 14), panel.background = element_rect(fill = "#FFFFFF"))
  )
  
  return (pie)
}





# UI

appOverviewTab <- tabPanel("Users Overview (Free)",
                           titlePanel(h1(paste0("Number of users: ", num_users), align = "center")),
                           
                           br(), br(), hr(), br(), br(),
                           
                           fluidRow(align = "center",
                                    column(6, align = "center", plotOutput("gender_plot")), 
                                    column(6, align = "center", plotOutput("age_plot"))
                           ),
                           
                           br(), br(), hr(), br(), br(),
                           
                           fluidRow(align = "center",
                                    column(6, align = "center", plotOutput("race_plot")), 
                                    column(6, align = "center", plotOutput("goal_of_participating_plot"))
                           ),
                           
                           br(), br(), hr(), br(), br(),
                           
                           fluidRow(align = "center",
                                    column(6, align = "center", plotOutput("field_of_study_plot")), 
                                    column(6, align = "center", plotOutput("intended_career_plot"))
                           ),
                           
                           br(), br(), hr(), br(), br(),
                           
                           fluidRow(align = "center",
                                    column(6, align = "center", plotOutput("go_out_freq_plot")), 
                                    column(6, align = "center", plotOutput("go_on_date_plot"))
                           ),
                           
                           br(), br(), br()
                           
)

firstTab <- tabPanel("Tastes and Preferences (Free)",
                     titlePanel(h1("What do males and females look for in the opposite sex?", align = "center")),
                     titlePanel(h3("The attributes that people look for in a date often change over time, especially after a few dates. Here's a look at some of the common changes.", align = "center")),
                     
                     br(), br(),
                     
                     fluidRow(align = "center",
                              column(6, align = "center", plotOutput("lookForA")), 
                              column(6, align = "center", plotOutput("lookForB"))
                     ),
                     
                     fluidRow(align = "center",
                              column(6, align = "center", plotOutput("lookForC")), 
                              column(6, align = "center", plotOutput("lookForD"))
                     ),
                     
                     br(), br(), hr(), br(), br(),
                     
                     titlePanel(h1("What do males and females think their date looks for in them?", align = "center")),
                     titlePanel(h3("After a few dates, with time, people usually realise that their dates are looking for a specific attribute. Here's a look at some patterns.", align = "center")),
                     
                     fluidRow(plotOutput("lookForE"), align = "center"),
                     
                     fluidRow(align = "center",
                              column(6, align = "center", plotOutput("lookForF")), 
                              column(6, align = "center", plotOutput("lookForG"))
                     ),
                     
                     br(), br(), hr(), br(), br(),
                     
                     titlePanel(h1("For both failed and successful matches, what did people think about their date for the night?",align = "center")),
                     plotOutput("datesImpressionsPlot"),
                     
                     br(), br(), br()
)


# ======================== JOEY ========================
secondTab <- tabPanel("Tastes and Preferences - II (Paid)",
                      titlePanel(h1("What attributes do we prioritise?", align = "center")),
                      titlePanel(h3("People at different stages of life usually look for different things in a mate, don't they?", align = "center")),
                      
                      br(), br(),
                      
                      fluidRow(column(6, align = "center",
                                      div(
                                        radioButtons("gender", label = "Gender", choices = c("Male" = "TRUE", "Female" = "FALSE")),
                                        radioButtons("age", label = "Age", choices = c("18-25","26-35","36-55")))
                      ),
                      column(6, align = "center", plotOutput("imptPlot"))
                      ),
                      
                      br(), br(), hr(), br(), br(),
                      
                      titlePanel(h1("What others look for vs what I look for in the opposite sex", align = "center")),
                      fluidRow(
                        column(6, align = "center",
                               div(
                                 numericInput("attJoey", label = "Attractiveness", value = 0, min = 0, max = 100, step = 5),
                                 numericInput("sinJoey", label = "Sincerity", value = 0, min = 0, max = 100, step = 5),
                                 numericInput("intJoey", label = "Intelligence", value = 0, min = 0, max = 100, step = 5),
                                 numericInput("funJoey", label = "Fun", value = 0, min = 0, max = 100, step = 5),
                                 numericInput("ambJoey", label = "Ambition", value = 0, min = 0, max = 100, step = 5),
                                 numericInput("shaJoey", label = "Shared interests", value = 0, min = 0, max = 100, step = 5)
                               )
                        ),
                        column(6, align = "center", plotOutput("vsPlotJoey"))
                      ),
                      
                      br(), br(), br()
)

# ======================== JOEY ========================


thirdTab <- tabPanel("Matching Interests",
                     titlePanel(h1("How matching are our interests?", align = "center")),
                     titlePanel(h3("For some people, a particular hobby or interest can either be the deal-breaker or seal the deal.", align = "center")),
                     
                     br(), br(),
                     
                     fluidRow(selectInput("my_attribute_dropdown",
                                          label = h3("My attribute", align = "center"),
                                          choices = options_of_characteristics,
                                          selected = 1),
                              align = "center"),
                     
                     br(), br(),
                     
                     fluidRow(selectInput("other_attribute_dropdown",
                                          label = h3("The other person's attribute", align = "center"),
                                          choices = options_of_characteristics,
                                          selected = 1),
                              align = "center"),
                     
                     br(), br(),
                     
                     fluidRow(radioButtons("gender_radio",
                                           label = h3("My gender"),
                                           choices = list("Male" = TRUE, "Female" = FALSE),
                                           selected = TRUE),
                              align = "center"),
                     
                     br(), br(),
                     
                     titlePanel(h3("What's the probability that the other party will be interested?", align = "center")),
                     plotOutput("bothPeopleAttributesPlot"),
                     
                     br(), br(), br()
)

height_heatmap <- 100

fourthTab <-
  tabPanel("Matching Characteristics (Paid)",
           titlePanel(h1("How matching are our characteristics?", align = "center")),
           titlePanel(h3("Do people usually find success in people that they share characteristics with? Or do opposites attract?", align = "center")),
           
           br(), br(), br(), 
           
           plotOutput("fieldOfStudyComp", width = "100%", height = height_heatmap * nrow(unique(field_of_study_combos[1]))),
           
           br(), br(), hr(), br(), br(),
           
           plotOutput("raceComp", width = "100%", height = height_heatmap * nrow(unique(race_combos[1]))),
           
           br(), br(), hr(), br(), br(),
           
           plotOutput("participatingReasonComp", width = "100%", height = height_heatmap * nrow(unique(goal_of_participating_combos[1]))),
           
           br(), br(), hr(), br(), br(),
           
           plotOutput("goingOutFreqComp", width = "100%", height = height_heatmap * nrow(unique(freq_of_going_out_combos[1]))),
           
           br(), br(), hr(), br(), br(),
           
           plotOutput("dateFreqComp", width = "100%", height = height_heatmap * nrow(unique(freq_of_dates_combos[1]))),
           
           br(), br(), br()
  )



conclusionTab <- tabPanel("Summary",
                          h2(paste0("Likelihood of finding a good match: ", sample(50:90, 1), "%"), align = "center"),
                          
                          br(), br(), hr(), br(), br(),
                          
                          h4("Click here for a 1-on-1 chat with our dating consultants!", align = "center"),
                          fluidRow(actionButton("", label = "Contact"), align = "center"),
                          
                          br(), br(), hr(), br(), br(),
                          
                          fluidRow(textInput("text", label = h3("Leave a feedback!", align = "center"), placeholder = "Suggestions..."), align = "center"),
                          fluidRow(actionButton("", label = "Submit"), align = "center"),
                          
                          br(), br(), br()
                          
)

ui <- navbarPage(
  "DatingAnalytics",
  tags$style(type="text/css", "body {padding-top: 150px;}"),
  appOverviewTab,
  firstTab,
  secondTab,
  thirdTab,
  fourthTab,
  conclusionTab,
  inverse = TRUE,
  position = "fixed-top"
)






# Server

server <- function(input, output) {
  
  # Overview Tab
  
  output$gender_plot <- renderPlot({
    get_basic_stats_pie_chart(gender_stats)
  })
  
  output$age_plot <- renderPlot({
    (participants_data %>% filter(!is.na(age)) %>% select(age) 
     %>% ggplot(aes(x = age)) + geom_density(color = "Red") + labs(x = "Age", y = "Density", title = "Age Distribution") 
     + theme(panel.background = element_rect(fill = "#FFFFFF")))
  })  # density plot
  
  output$field_of_study_plot <- renderPlot({get_basic_stats_pie_chart(field_of_study_stats)})
  output$race_plot <- renderPlot({get_basic_stats_pie_chart(race_stats)})
  output$goal_of_participating_plot <- renderPlot({get_basic_stats_pie_chart(goal_of_participating_stats)})
  output$go_out_freq_plot <- renderPlot({get_basic_stats_pie_chart(go_out_freq_stats)})
  output$go_on_date_plot <- renderPlot({get_basic_stats_pie_chart(go_on_date_freq_stats)})
  output$intended_career_plot <- renderPlot({get_basic_stats_pie_chart(intended_career_stats)})
  
  # Tab 1
  
  output$lookForA <- renderPlot({spider_chart_look(look_for_in_opp_sex_before_event_summary, "Normally")})
  output$lookForB <- renderPlot({spider_chart_look(look_for_in_opp_sex_halfway_thru_event_summary, "Right after a dates")})
  output$lookForC <- renderPlot({spider_chart_look(look_for_in_opp_sex_day_after_event_summary, "A day after a date")})
  output$lookForD <- renderPlot({spider_chart_look(look_for_in_opp_sex_weeks_after_event_summary, "A few weeks after a date")})
  
  output$lookForE <- renderPlot({spider_chart_look(opp_sex_look_for_in_date_before_event_summary, "Normally")})
  output$lookForF <- renderPlot({spider_chart_look(opp_sex_looks_for_in_date_day_after_event_summary, "A day after a date")})
  output$lookForG <- renderPlot({spider_chart_look(opp_sex_looks_for_in_date_weeks_after_event_summary, "A few weeks after a date")})
  
  output$datesImpressionsPlot <- renderPlot({failed_vs_success_matches()})
  
  # Tab 2 (JOEY)
  
  output$imptPlot <- renderPlot({
    
    three_dfs <- list(look_for_in_opp_sex_at_signup_w_age, look_for_in_opp_sex_day_after_event_w_age, look_for_in_opp_sex_weeks_after_event_w_age)
    attrs_cols <- c("attractiveness", "sincerity", "intelligence", "fun", "ambition", "shared_interests")
    
    value <- c()
    for (df in three_dfs) {
      for (attr in attrs_cols) {
        res <- df %>% filter(is_male == input$gender & age == input$age) %>% select(!!as.symbol(attr)) %>% lapply(mean)
        value <- c(value, res)
      }
    }
    
    attrs <- c("Attractiveness", "Sincerity", "Intelligence", "Fun", "Ambition", "Shared interests")
    
    criteria <- rep(attrs, 3)
    stage <- c(rep("At signup", 6), rep("Day after", 6), rep("Weeks after", 6))
    tb <- data.frame(stage, criteria, value) %>% within(stage <- factor(stage, levels = c("At signup", "Day after", "Weeks after")))
    
    result <- (ggplot(tb, aes(fill = criteria, y = value, x = stage)) 
               + geom_bar(position = "stack", stat = "identity", color = "black") 
               + scale_color_gradient(low = "#FFDAF4", high = "#FE58CD"))
    
    result
  })
  
  output$vsPlotJoey <- renderPlot(
    { 
      criteria <- c("att","sin","int","fun","amb","sha")
      
      value <- c(
        input$attJoey - look_for_in_opp_sex_at_signup_w_age %>% filter(is_male == input$gender & age == input$age) %>% select(attractiveness) %>% sapply(mean),
        input$sinJoey - look_for_in_opp_sex_at_signup_w_age %>% filter(is_male == input$gender & age == input$age) %>% select(sincerity) %>% sapply(mean),
        input$intJoey - look_for_in_opp_sex_at_signup_w_age %>% filter(is_male == input$gender & age == input$age) %>% select(intelligence) %>% sapply(mean),
        input$funJoey - look_for_in_opp_sex_at_signup_w_age %>% filter(is_male == input$gender & age == input$age) %>% select(fun) %>% sapply(mean),
        input$ambJoey - look_for_in_opp_sex_at_signup_w_age %>% filter(is_male == input$gender & age == input$age) %>% select(ambition) %>% sapply(mean),
        input$shaJoey - look_for_in_opp_sex_at_signup_w_age %>% filter(is_male == input$gender & age == input$age) %>% select(shared_interests) %>% sapply(mean)
      )
      
      tb <- data.frame(criteria, value)
      ggplot(tb, aes(x = criteria, y = value, fill = value)) + geom_bar(stat = "identity", color = "black") + scale_fill_gradient(low = "#FFDAF4", high = "#FE58CD")
    }) 
  
  # Tab 3
  
  getCharsChart <- reactive({
    return (get_attributes_comparison(input$my_attribute_dropdown, input$other_attribute_dropdown, input$gender_radio))
  })
  
  output$bothPeopleAttributesPlot <- renderPlot(getCharsChart())
  
  
  # Tab 4
  
  output$fieldOfStudyComp <- renderPlot(
    get_chars_pie_chart(field_of_study_combos, input$my_field_study_dropdown, input$partner_field_study_dropdown)
  )
  
  output$raceComp <- renderPlot(
    get_chars_pie_chart(race_combos, input$my_race_dropdown, input$partner_race_dropdown)
  )
  
  output$participatingReasonComp <- renderPlot(
    get_chars_pie_chart(goal_of_participating_combos, input$my_participating_dropdown, input$partner_participating_dropdown)
  )
  
  output$goingOutFreqComp <- renderPlot(
    get_chars_pie_chart(freq_of_going_out_combos, input$my_go_out_dropdown, input$partner_go_out_dropdown)
  )
  
  output$dateFreqComp <- renderPlot(
    get_chars_pie_chart(freq_of_dates_combos, input$my_date_dropdown, input$partner_date_dropdown)
  )
  
  
  
  output$fieldOfStudyComp <- renderPlot(match_prob_to_heatmap(field_of_study_combos, "Field of Study"))
  
  output$raceComp <- renderPlot(match_prob_to_heatmap(race_combos, "Race"))
  
  output$participatingReasonComp <- renderPlot(match_prob_to_heatmap(goal_of_participating_combos, "Goal of going on dates"))
  
  output$goingOutFreqComp <- renderPlot(match_prob_to_heatmap(freq_of_going_out_combos, "Frequency of going out"))
  
  output$dateFreqComp <- renderPlot(match_prob_to_heatmap(freq_of_dates_combos, "Frequency of going on dates"))
}


shinyApp(ui = ui, server = server)



