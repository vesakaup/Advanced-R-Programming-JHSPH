
library(dplyr)
library(tidyr)
library(readr)
library(magrittr)

#A class called “LongitudinalData” that characterizes the structure of this longitudinal dataset:

LongitudinalData <- function(df) {
        structure(list(df = df), class = "LongitudinalData")
}

# make_LD() converts a data frame into a “LongitudinalData” object

make_LD <- function(df) UseMethod("make_LD")

make_LD <- function(df) {
        LongitudinalData(df)
}

#subject() is a  function for extracting subject-specific information

subject <- function(x, subj_index) {
        subj_df <- filter(x$df, id == subj_index)
        structure(list(id = subj_df$id[1], subj_df = subj_df), class = "subject")
}

#For subject() we need to implement also the print and summary methods:

print.subject <- function(s) {
        cat("Subject ID:", s$id)
}

summary.subject <- function(s) {
        structure(list(s = s), class="subjectSummary")
}

print.subjectSummary <- function(ss) {
        summary_df <- ss$s$subj_df %>% group_by(visit, room) %>% select(visit, room, value) %>% summarize(value=mean(value)) %>% spread(room, value)
        print(paste("ID:", ss$s$id))
        print(summary_df)
}

# visit() is a generic function for extracting visit-specific information

visit <- function(s, visit_index) {
        structure(list(subj = s, visit_df = filter(s$subj_df, visit == visit_index), visit_index = visit_index), class = "visit")
}

#Fourth function to be implemented: room() is a generic function for extracting room-specific information

room <- function(v, room_type) {
        structure(list(visit = v, room_df = filter(v$visit_df, room == room_type), room_type = room_type), class = "room")
}

#For room() print and summary methods are implemented:

print.room <- function(r) {
        print(paste("ID:", r$v$subj$id))
        print(paste("Visit:", r$v$visit_index))
        print(paste("Room:", r$room_type))
}

summary.room <- function(r) {
        structure(list(r = r), class="roomSummary")
}

print.roomSummary <- function(rs) {
        print(paste("ID:", rs$r$v$subj$id))
        print(paste("Visit:", rs$r$v$visit_index))
        print(paste("Room:", rs$r$room_type))
        print(summary(rs$r$room_df$value))
}