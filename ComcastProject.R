# Comcast Telecom Consumer Complaints (Project 2)
#Load Library

library(tidyverse)
library(lubridate)
library(zoo)
library(ggplot2)
library(ggfortify)
library(scales)

### A : Import  Data And Provide Trend chart for Daily and Monthly Granularity

# Part 1 : Reading the CSV file

raw_data <- read.csv("/Users/Manor/Desktop/Comcast Telecom Complaints data.csv", stringsAsFactors = FALSE)

# Part 2 : Daily and Monthly Granularity Chart
## Cleaning the date format
raw_data$Date <-dmy(raw_data$Date)
raw_data$Date <- as.Date(raw_data$Date)

## working with sample data
sample_data <-raw_data %>% 
  select(-c("Ticket..","Zip.code","Filing.on.Behalf.of.Someone","Time","City"))

daily_complaints <- sample_data %>%
  mutate(Daily_Complaints = strftime(sample_data$Date,"%d")) %>% 
  count(Daily_Complaints) %>% 
  rename(daily_Count = "n")


monthly_complaints <- sample_data %>% 
  mutate(Monthly_Complaints = strftime(sample_data$Date,"%m")) %>% 
  count(Monthly_Complaints) %>% 
  rename(monthly_Count = "n") 

## Total Daily Complaint graph

(g1 <- ggplot(data = daily_complaints,aes(x = Daily_Complaints, y = daily_Count , group = 1, color = "orange")) + geom_line() + geom_point() + labs (x = "Days", y = "Sum of Complaints", title = "Figure 1: A graph showing the daily complaints by customers", fill = NULL  ) + geom_path()+ theme_classic() + theme(legend.position="none"))

## Total Monthly Complaint graph
(g2 <- ggplot(data = monthly_complaints,aes(x = Monthly_Complaints, y = monthly_Count , group = 1, color = "orange")) + geom_line() + geom_point() + labs (x = "Months", y = "Sum of Complaints", title = "Figure 2 : A graph showing the monthly complaints by customers", fill = NULL  ) + geom_path()+ theme_classic() + theme(legend.position="none"))


# Part 3: Provide a table with the frequency of complaint types.

# Creating a new column with NA values

sample_data <- sample_data %>%  
  add_column(Complaint_Type = NA)

# Classifying customer complaints into  5  groups using certain keys 

internet <- contains(raw_data$Customer.Complaint ,match = c("internet service","gb", "bundle","cap","mb","speed","internet"),ignore.case = TRUE)
billing <- contains(raw_data$Customer.Complaint ,match = c("billing","charged", "charges","billed"),ignore.case = TRUE)
service <- contains(raw_data$Customer.Complaint ,match = "service",ignore.case = TRUE)
email <- contains(raw_data$Customer.Complaint ,match = "email",ignore.case = TRUE)

## adding new classifications into the dataframe column as values 
sample_data$Complaint_Type[internet] <- "Internet"
sample_data$Complaint_Type[service] <- "Service" 
sample_data$Complaint_Type[billing] <- "Billing"
sample_data$Complaint_Type[email] <- "Email"
sample_data$Complaint_Type[-c(internet,service,billing, email)] <- "Others"

# Frequency of Customer Complaints   
Frequency_data <- data.frame(table(sample_data$Complaint_Type))
colnames(Frequency_data) <- c("Complaint Category", "Frequency")

### Part B :Which complaint types are maximum ??

# Part 1 : New category Creation

# Create new column with NA values
sample_data <- sample_data %>%  
  add_column(New_Status = NA)

## Categorize the data based on the status of complaint 
sample_data$New_Status[sample_data$Status == "Open"|sample_data$Status == "Pending"] = "Open"
sample_data$New_Status[sample_data$Status == "Closed"|sample_data$Status == "Solved"] ="Closed"

# Finding the count of complaints based on the state and status
state_wise_status <- sample_data %>% 
  group_by(State, New_Status) %>% 
  summarise(Count = n())

# plotting graph 
(g3 <- ggplot(data = state_wise_status, aes(x = State, y = Count, fill = New_Status)) + 
    geom_bar(stat = "identity") +  coord_flip() + scale_fill_brewer(palette = "Dark2"))

(g3 + labs(title = "Figure 3  Shows the State Wise Status of Complaints", 
          y = "Number of Complaints", x = "States", fill = "Status"))

# State with the most complaints in descending order

Maximum_complaints <- data.frame (sample_data %>% 
                                    group_by(State) %>% 
                                    summarise(Count = n()) %>% 
                                    arrange(desc(Count)))

## State with highest percentage of unresolved complaints 
Highest_unresolved_Complaints <- sample_data %>% 
  group_by(State) %>% 
  filter(New_Status == "Open") %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = formattable::percent(Count/sum(Count))) %>% 
  arrange(desc(Percentage))


## State with highest percentage of resolved Complaints
Highest_resolved_Complaints <- sample_data %>% 
  group_by(State) %>% 
  filter(New_Status == "Closed") %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage =formattable::percent (Count/sum(Count))) %>% 
  arrange(desc(Percentage))


### Resolved complaints till date based on Received Via

Resolved_complaints_till_date <- sample_data%>% 
  group_by(Received.Via) %>% 
  filter(New_Status == "Closed" ) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = formattable::percent(Count/sum(Count))) %>% 
  arrange(desc(Percentage))


# Graph showing Total Complaints Resolved Till Date  based on Received Via
(g4 <- ggplot(data =Resolved_complaints_till_date, aes(x="",y=Percentage, fill = Received.Via ) ) +
    geom_bar(width = 1, stat = "identity") + theme_classic() + coord_polar("y", start=0) + theme(axis.line = element_blank()) + theme(axis.text = element_blank()) + theme(axis.ticks = element_blank()) +
    labs(x = NULL, y = NULL , fill = "Received.Via", title = "Figure 4 Total Complaints Resolved Till Date"   ) + geom_text(aes(label = paste0(Percentage)), position= position_stack(vjust = 0.5)))

### Unresolved Complaints till date  based on Received Via

Unresolved_complaints_till_date <- sample_data%>% 
  group_by(Received.Via) %>% 
  filter(New_Status == "Open" ) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = formattable::percent(Count/sum(Count))) %>% 
  arrange(desc(Percentage))

## Graph showing Unresolved Complaints till date  based on Received Via

(g5 <- ggplot(data =Unresolved_complaints_till_date, aes(x="",y=Percentage, fill = Received.Via ) ) +
    geom_bar(width = 1, stat = "identity") + theme_classic() + coord_polar("y", start=0) + theme(axis.line = element_blank()) + theme(axis.text = element_blank()) + theme(axis.ticks = element_blank()) +
    labs(x = NULL, y = NULL , fill = "Received.Via", title = "Figure 5 Total Complaints Unresolved Till Date"   ) + geom_text(aes(label = paste0(Percentage)), position= position_stack(vjust = 0.5)))

## Total complaints per Received Via (Internet) 
internet_only <- sample_data%>% 
  group_by(New_Status) %>% 
  filter(Received.Via == "Internet" ) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = formattable::percent(Count/sum(Count))) %>% 
  arrange(desc(Percentage))

## Graph showing Total complaints per Received Via (Internet) 

(g6 <- ggplot(data =internet_only, aes(x="",y=Percentage, fill = New_Status ) ) +
    geom_bar(width = 1, stat = "identity") + theme_classic() + coord_polar("y", start=0) + theme(axis.line = element_blank()) + theme(axis.text = element_blank()) + theme(axis.ticks = element_blank()) +
    labs(x = NULL, y = NULL , fill = "Received.Via", title = "Figure 6 Total  complaints per Received Via (Internet) "   ) + geom_text(aes(label = paste0(Percentage)), position= position_stack(vjust = 0.5)))


## Total  complaints per Received Via (Customer Care)
Customer_Care <- sample_data%>% 
  group_by(New_Status) %>% 
  filter(Received.Via == "Customer Care Call" ) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = formattable::percent(Count/sum(Count))) %>% 
  arrange(desc(Percentage))


## Graph showing Total  complaints per Received Via (Customer Care)
(g7 <- ggplot(data =Customer_Care, aes(x="",y=Percentage, fill = New_Status ) ) +
    geom_bar(width = 1, stat = "identity") + theme_classic() + coord_polar("y", start=0) + theme(axis.line = element_blank()) + theme(axis.text = element_blank()) + theme(axis.ticks = element_blank()) +
    labs(x = NULL, y = NULL , fill = "Received.Via", title = "Figure 7 Total  complaints per Received Via (Customer Care) "   ) + geom_text(aes(label = paste0(Percentage)), position= position_stack(vjust = 0.5)))







