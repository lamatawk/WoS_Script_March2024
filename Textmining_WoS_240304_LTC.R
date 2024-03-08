getwd()
setwd("C:/Users/ltawk/Documents/WoS_Script_March2024")

install.packages("tidyverse")
install.packages("reshape2")
install.packages("tidyr")
install.packages("dplyr")


### Step1 publication temporal distribution
# Analyze the distribution of studies over time by extracting the publication year ('PY') from the data and plotting its frequency

# Load the data
data <- read.delim("C:/Users/ltawk/Documents/WoS_Script_March2024/shannon2.txt", header=TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE, row.names=NULL)

# Study Time Distribution
data %>%
  count(PD) %>%
  ggplot(aes(x=PD, y=n)) +
  geom_col() +
  theme_minimal() +
  labs(x="Publication Year", y="Number of Studies", title="Study Time Distribution")
ggsave("plot.png", plot = last_plot(), width = 10, height = 8, dpi = 300)


## Step 2 Wrod frequency analysis
install.packages("tidytext")
library(tidytext)

# Check the structure of the data to see what type the AB column is
str(data$PY)

# If it's not a character vector, convert it
data$PY <- as.character(data$PY)

# Prepare text data
text_data <- data %>%
  unnest_tokens(word, PY)

# Word frequency analysis
word_freq <- text_data %>%
  count(word, sort = TRUE)

# Top 20 words
word_freq %>%
  top_n(20) %>%
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col() +
  coord_flip() +
  labs(x="Word", y="Frequency", title="Top 20 Frequent Words in Abstracts")

### Phase 2 testing out the bibliometrix pacakge 

install.packages("bibliometrix")
library("bibliometrix")

setwd("C:/Users/ltawk/Documents/WoS_Script_March2024/")

# Read the file into R using the bibliometrix function
# The function to use from bibliometrix might be something like `convert2df()`
# which converts WoS plain text files to a bibliographic data frame
M <- convert2df(file = "shannon2.txt", dbsource = "isi", format = "plaintext")

# Now you can perform various bibliometric analyses using the data frame M
# For example, to perform a descriptive analysis:
results <- bibliometrix::biblioAnalysis(M)

# To explore the results, you can use the `summary()` function or other bibliometrix functions
summary(results)