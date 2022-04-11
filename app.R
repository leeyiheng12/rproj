
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(fmsb)

library(shiny)

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
          + geom_bar(stat = "identity") 
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
  
  graph_title <- paste0("Proportion of people of the other gender interested in you, 
                        \n based on your level of interest in ", attr, " and his/her level of interest in " , other_attr, 
                        "\n\nYour level of interest in ", attr)
  x_axis_label <- paste0("Their level of interest in ", other_attr)
  y_axis_label <- "Proportion of interest"
  
  attributes_comparison_graph <- (attributes_comparison 
                                  %>% mutate(grouping = factor(!!as.symbol(aggr_attr_col_name), levels = ordering))  # create a new column to order Low Mid High
                                  %>% ggplot(aes(x = reorder(!!as.symbol(aggr_other_attr_col_name), success_rate), # reorder within each facet
                                                 y = success_rate)) 
                                  + geom_bar(stat = "identity", fill = rainbow(nrow(attributes_comparison)))
                                  + facet_wrap(.~grouping)  # order the facets based on Low, Mid, High
                                  + ggtitle(graph_title)
                                  + labs(x = x_axis_label, y = y_axis_label)
                                  + theme(plot.title = element_text(hjust = 0.5))
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
    geom_bar(stat="identity", width = 1) +
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
  
  return (results %>% ggplot(aes(x = Attribute, y = Score)) + geom_bar(stat = "identity") + facet_wrap(~match))
  
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



ui <- fluidPage(
  
  titlePanel("Title"),
  
  mainPanel(
    tabsetPanel(type = "tabs",
                
                tabPanel("Tastes and Preferences", 
                         titlePanel("What do ___ look for in the oppsite sex..."),
                         fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("lookForA"), plotOutput("lookForB"))),
                         fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("lookForC"), plotOutput("lookForD"))),
                         hr(),
                         titlePanel("What do ___ think the opposite sex looks for..."),
                         plotOutput("lookForE"),
                         fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("lookForF"), plotOutput("lookForG"))),
                ),
                
                tabPanel("Impressions on Dates", 
                         titlePanel("On failed vs successful matches, what did people think about the other person's characteristics?"),
                         plotOutput("datesImpressionsPlot")
                ),
                
                tabPanel("How matching are our attributes?",
                         fluidRow(splitLayout(cellWidths = c("33.33%", "33.33%", "33.33%"),
                                              absolutePanel(selectInput("my_attribute_dropdown",
                                                                        label = h3("My attribute"),
                                                                        choices = options_of_characteristics,
                                                                        selected = 1)),
                                              absolutePanel(selectInput("other_attribute_dropdown",
                                                                        label = h3("The other person's attribute"),
                                                                        choices = options_of_characteristics,
                                                                        selected = 1)),
                                              radioButtons("gender_radio",
                                                           label = h3("My gender"),
                                                           choices = list("Male" = TRUE, "Female" = FALSE),
                                                           selected = TRUE)
                         )),
                         plotOutput("bothPeopleAttributesPlot")
                ),
                
                tabPanel("How matching are our characteristics?",
                         
                         fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                              absolutePanel(
                                                selectInput("my_field_study_dropdown", label = h3("My field of study"),
                                                            choices = possible_person_field_of_study, selected = 1),
                                                selectInput("partner_field_study_dropdown", label = h3("Partner's field of study"),
                                                            choices = possible_partner_field_of_study, selected = 1),
                                              ),
                                              plotOutput("fieldOfStudyComp"))
                         ),
                         hr(),
                         
                         fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                              plotOutput("raceComp"),
                                              absolutePanel(
                                                selectInput("my_race_dropdown", label = h3("My race"),
                                                            choices = possible_person_race, selected = 1),
                                                selectInput("partner_race_dropdown", label = h3("Partner's race"),
                                                            choices = possible_partner_race, selected = 1),
                                              ))
                         ),
                         hr(),
                         
                         fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                              absolutePanel(
                                                selectInput("my_participating_dropdown", label = h3("My reason for participating in dating events"),
                                                            choices = possible_person_participating_goal, selected = 1),
                                                selectInput("partner_participating_dropdown", label = h3("Partner's reason for participating in dating events"),
                                                            choices = possible_partner_participating_goal, selected = 1),
                                              ),
                                              plotOutput("participatingReasonComp"))
                         ),
                         hr(),
                         
                         fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                              plotOutput("goingOutFreqComp"),
                                              absolutePanel(
                                                selectInput("my_go_out_dropdown", label = h3("My frequency of going out (not necessarily on dates)"),
                                                            choices = possible_person_go_out_freq, selected = 1),
                                                selectInput("partner_go_out_dropdown", label = h3("Partner's frequency of going out (not necessarily on dates)"),
                                                            choices = possible_partner_go_out_freq, selected = 1),
                                              ))
                         ),
                         hr(),
                         
                         fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                              absolutePanel(
                                                selectInput("my_date_dropdown", label = h3("My frequency of going out on dates"),
                                                            choices = possible_person_date_freq, selected = 1),
                                                selectInput("partner_date_dropdown", label = h3("Partner's frequency of going out on dates"),
                                                            choices = possible_partner_date_freq, selected = 1),
                                              ),
                                              plotOutput("dateFreqComp"))
                         )
                         
                )
    )
    
  ),
  
  hr()  # some divider
  
)


server <- function(input, output) {
  
  # Tab 1
  
  output$lookForA <- renderPlot({spider_chart_look(look_for_in_opp_sex_before_event_summary, "...normally?")})
  output$lookForB <- renderPlot({spider_chart_look(look_for_in_opp_sex_halfway_thru_event_summary, "...right after meeting a few dates?")})
  output$lookForC <- renderPlot({spider_chart_look(look_for_in_opp_sex_day_after_event_summary, "...a day after meeting some dates?")})
  output$lookForD <- renderPlot({spider_chart_look(look_for_in_opp_sex_weeks_after_event_summary, "...a few weeks after meeting some dates?")})
  
  output$lookForE <- renderPlot({spider_chart_look(opp_sex_look_for_in_date_before_event_summary, "...normally?")})
  output$lookForF <- renderPlot({spider_chart_look(opp_sex_looks_for_in_date_day_after_event_summary, "...a day after meeting some dates?")})
  output$lookForG <- renderPlot({spider_chart_look(opp_sex_looks_for_in_date_weeks_after_event_summary, "...a few weeks after meeting some dates?")})
  
  # Tab 2
  output$datesImpressionsPlot <- renderPlot({failed_vs_success_matches()})
  
  # Tab 3
  
  getCharsChart <- reactive({
    return (get_attributes_comparison(input$my_attribute_dropdown, input$other_attribute_dropdown, input$gender_radio))
  })
  
  output$bothPeopleAttributesPlot <- renderPlot(getCharsChart())
  
  # Tab 4
  
  output$fieldOfStudyComp <- renderPlot(get_chars_pie_chart(field_of_study_combos, input$my_field_study_dropdown, input$partner_field_study_dropdown))
  output$raceComp <- renderPlot(get_chars_pie_chart(race_combos, input$my_race_dropdown, input$partner_race_dropdown))
  output$participatingReasonComp <- renderPlot(get_chars_pie_chart(goal_of_participating_combos, input$my_participating_dropdown, input$partner_participating_dropdown))
  output$goingOutFreqComp <- renderPlot(get_chars_pie_chart(freq_of_going_out_combos, input$my_go_out_dropdown, input$partner_go_out_dropdown))
  output$dateFreqComp <- renderPlot(get_chars_pie_chart(freq_of_dates_combos, input$my_date_dropdown, input$partner_date_dropdown))
  
}


shinyApp(ui = ui, server = server)




