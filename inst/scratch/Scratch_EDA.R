remotes::install_github("CTN-0094/public.ctn0094data")
library(tidyverse)
library(ggplot2)
library(dplyr) # not sure if this is necessary after the above tbh
library(patchwork) # for mixed color plots
packageDescription("public.ctn0094data") # sees what the package is before I even open it


library(public.ctn0094data)


# Creates a pop-up window listing every dataset in pkg
data(package = "public.ctn0094data")

#####Looking at Everybody#####
#"everybody" dataset has "everybody with any data"

# Tidyverse summary of cols

glimpse(everybody)
# 3560 rows, 2 cols ("who" and "project")

# Investigating "who".
#"who" is an ID number, we want to check that they are ascending
#do this w freq plot?
hist(everybody$who)
#nvm lol. that was a "hint" that there's one number per person, but not a guarantee.

# This just says there are 3560 unique rows, not that theyre ascending from 1
everybody |> nrow() # 3560
everybody |> distinct() |> nrow() # 3560

# Getting this with range is good
everybody |> summarize(
  min_id = min(who),
  max_id = max(who),
  unique_count = n_distinct(who)
)

# Investigating "project"
# Q: How many of each distinct project value are there?
# I can't visualize what's happening at the group_by and summarize stage
everybody |> select(project) |> group_by(project)|>
  summarize(count = n())
# A: There are 3 distinct project values, 37, 30, and 51 (they're factors)
# The counts are 27:1920, 30:868, and 51:772

##############################



##### Looking at Demographics #####

#"demographics" probably has one row per person.
# longitudinal probably has multiple rows per person like tflb
glimpse(demographics)
#9 cols: who (same key), age, is_hispanic, race, job, is_living_stable,
# ,education, marital, is_male
# all are factor except who <int> and age <dbl>

# We're gonna have to do fun transform things to is_male lol.

#Q: is "who" same as in everybody?
#A: Yes, see below.


###age
#Simple age histogram
demographics |> ggplot(aes(x = age)) + geom_histogram()
# Now I want to color the age histogram by the study they belong to
demographics |> left_join(everybody, by="who") |>
  ggplot(aes(x = age, fill = project)) + geom_histogram(color = "white") + scale_fill_viridis_d()
# Not so useful to actually see differences in distribs want to partition

demographics |> left_join(everybody, by="who") |> ggplot(aes(x=age)) + geom_histogram() + facet_wrap(~project)
# "Think of ggplot() as the foundation - you can't put your color_map inside it as an argument; you have to "glue" it on top."

histo_plot_1 <- ggplot(iris) + geom_histogram() + color_map

###is_hispanic
#Counts for is_hispanic split out by study
demographics |> left_join(everybody, by="who") |> select(who, project, is_hispanic) |>
  mutate(is_hispanic = tolower(is_hispanic)) |>
  mutate(hispanic_binary = case_match(is_hispanic,
                                      "yes" ~ 1,
                                      "no"~ 0,
                                      .default = 0)) |>
  group_by(project) |>
  summarize(
    hispanic_count = sum(hispanic_binary),
    total_participants = n(),
    prop_hispanic = hispanic_count / total_participants
  )
demographics |> left_join(everybody, by="who") |> select(who, project, is_hispanic) |> group_by(project) |> summarize(count = n())
# Study 30 has abt 5% hispanic while the others have 17

### Race

# We want a sense of the unique options here.
demographics |> select(who, race) |> group_by(race) |> summarize(count = n())
# The four options are "Black", "Other", "Refused/missing", "White"
#My guess is lots of the "Other" and "Refused/missing" are latino. Ok checks.

#Double check the totals in the above sum up right to 3560. Yes.
demographics |> select(who, race) |> group_by(race) |> summarize(count = n()) |>
  summarize(total_sum = sum(count, na.rm = TRUE))

# Counts for race split out by study/"project"
race_by_project <- demographics |> left_join(everybody, by="who") |> select(who, project, race) |>
  group_by(project,race) |> summarize(count = n(), .groups = "drop") #was told .groups = "drop" prevents silent dropping which gets buggy


ggplot(race_by_project, aes(x = "", y = count, fill = race) +
  geom_bar(stat = "identity", width= 1)
  labs(title = "Race Distribution by Project") +
  facet_wrap(~project) +
  coord_polar(theta = "y")



##############################

##### Looking at all_drugs #####

glimpse(all_drugs)
# Four cols, 307523 rows, "who" "what" "source" "when" are cols.

# Rows are  a single instance of
