##Assignment 2
csv_files <- list.files("Data", pattern = ".csv")
length(csv_files)
df <- read.csv("Data/wingspan_vs_mass.csv")
head(df)
b_files <- list.files("Data", pattern = "^b", recursive = TRUE)
b_files
for (f in b_files) {
  print(readLines(paste0("Data/", f), n = 1))
}
for (f in csv_files) {
  print(readLines(paste0("Data/", f), n = 1))
}
