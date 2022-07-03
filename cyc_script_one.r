# load the tidyverse, fs, janitor, jcolors, treemapify, and patchwork packages.
packages <- c("tidyverse", "fs", "janitor",
              "patchwork", "jcolors", "treemapify")

lapply(packages, library, character.only = TRUE) |> invisible()

# get files using the 'fs' library
dir  <- "Data\\one_year"
files  <- list.files(dir, "\\.csv$", full.names = TRUE)

# make sure the correct files are in the list
files

# save files to a data frame
cyc_init <- do.call(rbind, lapply(files, read.csv))

# viewing and cleaning data:
glimpse(cyc_init)
summary(cyc_init)

# viewing data frame and removing rows that were
# converted to empty strings during import

head(cyc_init)

cyc_init <- cyc_init %>%
    select(-start_station_id, -start_station_name,
           -end_station_id, -end_station_name)

# viewing data frame
head(cyc_init)

# removing duplicate rows
cyc_init <- cyc_init %>%
    distinct()

# let's gather some basic information
# the number of rows
cyc_init %>%
    summarize(length(ride_id))

# getting the number of riders by type
cyc_init %>%
    group_by(Type = member_casual) %>%
    summarize(Riders = length(ride_id),
              Percentage = length(ride_id) / nrow(cyc_init) * 100)

# plotting the data we just summarized
cyc_init %>%
    ggplot(aes(x = member_casual, fill = member_casual)) +
    geom_bar() +
    ylab("Total # of Members") +
    labs(fill = "Members v. Casuals",
         title = "Number of Riders by Membership Type") +
   scale_x_discrete("Membership Type",
                     labels = c("Casual", "Member")) +
    theme(legend.position = c(0.07, 0.90),
          legend.justification = "left",
          plot.title = element_text(hjust = 0.5,
                                    face = "bold",
                                    size = rel(2))) +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank())

# modifying the data further
# the code here will get the name of the month in which each trip was taken
cyc_init <- cyc_init %>%
    mutate(rides_monthly = lubridate::month(started_at,
                                            label = TRUE, abbr = TRUE))

# taking care of this as well before continuing
# this code renames our data frame variable
# and gets the name of the days of the week

cyclistic <- cyc_init %>%
    mutate(rides_daily = lubridate::wday(started_at, label = TRUE, abbr = TRUE))

months <- cyclistic$rides_monthly %>%
    levels()

months

correct_month_order <- c("May", "Jun", "Jul", "Aug", "Sep", "Oct",
                         "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")

cyclistic$rides_monthly <- cyclistic$rides_monthly %>%
    fct_relevel(correct_month_order)

cyclistic$rides_monthly %>% levels()


# making sure I've done both correctly
head(cyclistic)

cyclistic %>%
    group_by(Month = rides_monthly) %>%
    summarize(Rides = length(ride_id),
              Percentage = length(ride_id) / nrow(cyclistic) * 100)

cyclistic %>%
    ggplot(aes(x = rides_monthly, y = length(ride_id), fill = member_casual)) +
    geom_col() +
    coord_flip() +
    scale_x_discrete(limits = rev) +
    labs(title = "Riders per Month",
         subtitle = "by membership type",
         x = "Months",
         y = "Number of Rides",
         colors = "Membership Type") +
    theme(plot.title = element_text(face = "bold"),
          plot.title.position = "panel",
          legend.position = c(0.85, 0.10),
          legend.direction = "vertical",
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank())

 # for this one, I want to include percentages inside the center of the bars
 # and for the months to go in order
 # of the csv files (May, Jun, ..., March, April, etc.)

 cyclistic %>%
    group_by(Day = rides_daily) %>%
    summarize(Rides = length(ride_id),
              Percentage = length(ride_id) / nrow(cyclistic) * 100)

 cyclistic %>%
    ggplot(aes(x = rides_daily, y = length(ride_id), fill = member_casual)) +
    geom_col() +
    labs(title = "Rides per Week",
         x = "Day of Week",
         y = "# of Rides") +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          plot.title.position = "panel",
          legend.position = c(0.24, 0.95),
          legend.direction = "horizontal",
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank())

cyclistic %>%
    group_by(Type = rideable_type, Membership = member_casual) %>%
    summarize(Rides = length(ride_id),
              Percentage = length(ride_id) / nrow(cyclistic) * 100)

# daily rides by bike type, wrapped by user type
cyclistic %>%
    ggplot(aes(x = rides_daily, fill = rideable_type)) +
    geom_bar(position = position_dodge()) +
    facet_wrap(~ member_casual) +
    labs(title = "Preferred Weekly Rideable Type",
         subtitle = "by membership type",
         x = "Day of Week",
         y = "# of Users") +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(face = "bold"),
          plot.title.position = "panel",
          legend.position = "right",
          legend.direction = "vertical",
          legend.justification = "top")

cyclistic %>%
    ggplot(aes(x = rides_monthly, fill = rideable_type)) +
    geom_bar(position = position_dodge()) +
    facet_wrap(~ member_casual) +
    coord_flip() +
    scale_x_discrete(limits = rev) +
    labs(title = "Preferred Monthly Rideable Type",
         subtitle = "by membership type",
         x = "Month",
         y = "# of Users") +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          plot.title = element_text(face = "bold"),
          legend.justification = "top")

cyclistic %>%
    ggplot(aes(x = rideable_type, fill = member_casual)) +
    geom_bar(position = position_dodge(width = 1)) +
    labs(title = "Preferred Rideable Type",
         subtitle = "by membership type",
         x = "Type of Rideable",
         y = "# of Users") +
    theme(plot.title = element_text(face = "bold"),
          plot.title.position = "panel",
          legend.position = c(0.89, 0.97),
          legend.justification = "top")

# cyclistic <- cyclistic %>%
    # lubridate::parse_date_time(started_at)

# cyclistic <- cyclistic %>%
    # lubridate::parse_date_time(ended_at, roll = TRUE)

# these lines are just taking too long to run and I want to finish my project.
# I'll try to look at this again at a later time.

lapply(packages, citation)