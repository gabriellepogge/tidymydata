Tidying Very Messy Data
================
Gaby Pogge
2024-04-16

## Initial Setup

If you want to keep only markdown and get GitHub-flavored markdown, use
output: github_document above.

In many cases, you only want the markdown, which is most useful on
github because it will be a directly viewable/readable report. If you
want to use output: html_document but keep the intermediate markdown
(.md) file, use: output: html_document: keep_md: true

If there are R chunks that make figures, the usage of markdown output
formats will also cause those figure files to be left behind locally on
your machine in a sensibly named sub-directory such as foo_files.

If you commit and push foo.md and everything inside foo_files, then
anyone with permission to view your GitHub repo can see a decent-looking
version of your report.

By making foo.Rmd available, others can see and run your actual code. By
also sharing foo.md and/or foo.html, others can casually browse your end
product and decide if they want to obtain and run the code.

## Installing packages needed for initial steps

``` r
library(rmarkdown)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.5.0     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(googlesheets4)
```

## Importing the Data

Because the data is stored in a google sheet, we’ll use the
googlesheets4 package to create a sheet ID and then read in the data
using that sheet ID.

# Create a sheet id from google sheet data

``` r
gs4_deauth() #tells r to not ask for authentication (for publicly hosted data such as this file)

ssid1 <- googlesheets4::as_sheets_id("https://docs.google.com/spreadsheets/d/1lo91j3axIZr-z85tam2KvCMf2UkFYlp6BrtMC2Bt7To/edit#gid=0") # creating a sheet id from the source url
```

# Inspect the ssid1 object

``` r
class(ssid1) # confirms that ssid1 is indeed a sheet id, and the column type is character
```

    ## [1] "sheets_id"  "drive_id"   "vctrs_vctr" "character"

``` r
unclass(ssid1) # this is the google sheet ID, dropping the url
```

    ## [1] "1lo91j3axIZr-z85tam2KvCMf2UkFYlp6BrtMC2Bt7To"

# Read in verymessydata from google sheets

We’ll use the sheet id we created from the source url

``` r
df1 <- read_sheet(ssid1)
```

    ## ✔ Reading from "verymessydata".

    ## ✔ Range 'Sheet1'.

# View and inspect the data

``` r
glimpse(df1) # provides number of rows, columns, and column types
```

    ## Rows: 186
    ## Columns: 2
    ## $ OMID              <dbl> 3.8079e+12, 7.2605e+11, 8.5051e+12, 3.6257e+12, 9.21…
    ## $ AssessmentAnswers <chr> "[[80, 21, 3, 2, 2, 2, 5, 4, 5, 6, 5, Yes, 5, 5, 6, …

We see 186 rows and two columns (OMID and AssessmentAnswers) in this
data. The first column is numeric and contains response IDs that serve
as a unique identifier to differentiate the responses of individual
participants. The second column is a character vector that contains ALL
of the responses participants provided to each item in the survey.

# The task at hand

We will need to “tidy” this data by breaking the values in the
AssessmentAnswers column into individual columns and setting their
appropriate column types (eg numeric vs character) before we can proceed
to analysis.

# First impressions

``` r
head(df1, 3) # shows just the first few rows of data for visual inspection
```

    ## # A tibble: 3 × 2
    ##            OMID AssessmentAnswers                                               
    ##           <dbl> <chr>                                                           
    ## 1 3807900000000 "[[80, 21, 3, 2, 2, 2, 5, 4, 5, 6, 5, Yes, 5, 5, 6, (not asked)…
    ## 2  726050000000 "[[100, 37, 6, 6, 6, 4, 1, 6, 6, 6, 4, Unsure, 6, 4, 6, (not as…
    ## 3 8505100000000 "[[50, 50, 2, 4, 5, 6, 0, 4, 4, 3, 6, Yes, 6, 6, 5, (not asked)…

Take a peek at just the first row of data. You can see there are several
different styles of separators (commas, parentheses, brackets) that
indicate the responses to different items in the survey.

Notice there are also two time stamps indicating the responses were
collected at two different points in time: a “pre” and a “post”. In this
context, both pre and post responses were collected because something
happened in between these two time points that we think might impact the
variation in responses from pre-test to post-test.

Given this sample data is from a social-psychological survey, the “test”
in between pre and post responses was likely some experimental
condition - either between-groups experiment (where some participants
received one experimental manipulation and some participants received a
different experimental manipulation) or within-groups experiment (where
all participants received the same manipulation).

## Initial diagnostics - let R try guessing the separators

One reason to start with this approach is because it’s a quick and easy
way to eyeball where we may run into issues. R will produce individual
columns and automatically number them in the output data.

``` r
df_r <- df1 %>%
  tidyr::separate_wider_delim(AssessmentAnswers, delim = " ", names_sep = "", too_few = "align_start")

head(df_r, 2)
```

    ## # A tibble: 2 × 318
    ##            OMID AssessmentAnswers1 AssessmentAnswers2 AssessmentAnswers3
    ##           <dbl> <chr>              <chr>              <chr>             
    ## 1 3807900000000 [[80,              21,                3,                
    ## 2  726050000000 [[100,             37,                6,                
    ## # ℹ 314 more variables: AssessmentAnswers4 <chr>, AssessmentAnswers5 <chr>,
    ## #   AssessmentAnswers6 <chr>, AssessmentAnswers7 <chr>,
    ## #   AssessmentAnswers8 <chr>, AssessmentAnswers9 <chr>,
    ## #   AssessmentAnswers10 <chr>, AssessmentAnswers11 <chr>,
    ## #   AssessmentAnswers12 <chr>, AssessmentAnswers13 <chr>,
    ## #   AssessmentAnswers14 <chr>, AssessmentAnswers15 <chr>,
    ## #   AssessmentAnswers16 <chr>, AssessmentAnswers17 <chr>, …

We can see that R does a pretty good job guessing the “,” delimiter for
AssessmentAnswers1 - AssessmentAnswers11. Values that now appear in
those columns are all integers, although they still include the
delimiters for now.

Where R’s guessing breaks down first is at AssessmentAnswers12. But we
can’t necessarily tell that from the two lines above produced by head().

“Yes” and “Unsure”responses in the AssessmentAnswers12 column include
the “,” separator that R identified, and they are followed by integers
in the adjacent AssessmentAnswers13 column. With these two pieces of
information we can probably assume that these values represent single
word responses to the corresponding question.

But let’s take a closer peek at the values in the AssessmentAnswers12
column to see if there are additional responses that we can’t see here.
Minimally, we might expect we’ll find a “No” response somewhere in this
column given we see yes and unsure responses.

``` r
table(df_r$AssessmentAnswers12) # all 186 rows start with one of these four responses to the question corresponding with AssessmentAnswers12
```

    ## 
    ##     I'm     No, Unsure,    Yes, 
    ##      11      22      68      85

Upon further inspection we can see that there are four values of
character responses to AssessmentAnswers12, with two additional
responses we didn’t see before. We see that there is indeed a “No”
response option, and an additional response appears to begin with “I’m”.
We can surmise that this word/value is probably followed by additional
words/values that R placed in adjacent columns following
AssessmentAnswers12 - in part because we don’t see the comma separator
included last in the value/character string for “I’m” whereas it is
included in the “No,” “Unsure,” and “Yes,” responses.

Let’s check out the values in AssessmentAnswers13 and
AssessmentAnswers14 to see what may have spilled over into those
columns.

``` r
table(df_r$AssessmentAnswers13)
```

    ## 
    ##    (not      2,      3,      4,      5,      6, already 
    ##      33       3      20      24      43      52      11

``` r
table(df_r$AssessmentAnswers14)
```

    ## 
    ##          0,          1,          2,          3,          4,          5, 
    ##           2           2           7          23          23          47 
    ##          6,     asked),       met), subscribed, 
    ##          38          20          13          11

For the same reasons we suspected that responses that began with “I’m”
for AssessmentAnswers12 spilled into AssessmentAnswers13, we can expect
that: 1. “(not” values contained in AssessmentAnswers13 are probably
followed by either “asked)” or “met)” values in AssessmentAnswers14. 2.
““already” values in AssessmentAnswers13 are probably followed by
“subscribed” values in AssessmentAnswers14.

We now have strong suspicions that for some of the 186 rows there is
spillage from the values in AssessmentAnswers12 to the values in
AssessmentAnswers13 and AssessmentAnswers14.

Let’s take a look at the values that appear when we “cross” the adjacent
columns (ie 12 with 13 and, separately, 13 with 14).

``` r
table(df_r$AssessmentAnswers12, df_r$AssessmentAnswers13)
```

    ##          
    ##           (not 2, 3, 4, 5, 6, already
    ##   I'm        0  0  0  0  0  0      11
    ##   No,        4  1  2  4  5  6       0
    ##   Unsure,   12  1 11  7 12 25       0
    ##   Yes,      17  1  7 13 26 21       0

``` r
table(df_r$AssessmentAnswers13, df_r$AssessmentAnswers14)
```

    ##          
    ##           0, 1, 2, 3, 4, 5, 6, asked), met), subscribed,
    ##   (not     0  0  0  0  0  0  0      20    13           0
    ##   2,       0  0  0  0  2  1  0       0     0           0
    ##   3,       2  0  3  8  3  2  2       0     0           0
    ##   4,       0  1  1 10  9  3  0       0     0           0
    ##   5,       0  0  2  3  3 29  6       0     0           0
    ##   6,       0  1  1  2  6 12 30       0     0           0
    ##   already  0  0  0  0  0  0  0       0     0          11

Look at the pattern of 0s in the first row of the top table (beginning
with I’m) and the last row in the bottom table (beginning with
“already”. We can make an educated guess that there were 11 responses of
the 186 total responses that indicated “I’m already subscribed”, as each
of those values don’t appear with any other adjacent values.

Apply this same logic to the pattern of 0s for the “asked),” and “met)”
columns in the first row of the bottom table, beginning with “(not”. We
can see that, again, each of those values don’t appear with any other
adjacent values.

# Why are we bothering with this?

This may seem like a tedious exercise in this “small data” context where
the urge may be to just visually inspect the data to confirm your
suspicions about what cell values should contain for particular columns.
But finding other methods to conduct this process becomes increasingly
important with larger datasets that make it untenable to glance at every
single row.

Collectively, we can learn 3 things from the above tables without even
looking at the data frame df_r itself.

1.  There are four possible responses to AssessmentAnswers12: I’m
    already subscribed, No, Unsure, Yes.
2.  Responses to AssessmentAnswers13 are integers ranging from 2-6
    inclusive OR one of two character responses: (not asked) or (not
    met).
3.  Responses to AssessmentAnswers14 are integers ranging from 0-6
    inclusive.

Clearly, R’s attempt to find the appropriate separators and create new
columns using the “,” separator didn’t get us where we want to go. But
it’s useful for both initial diagnostic purposes.

## Lets try an alternative approach

Let’s first separate the AssessmentAnswers column into more manageable
chunks of data. Whereas R’s naiive approach was to separate on the
commas in the data, we’ll first try a different separator.

Recall that we also saw some brackets in the data. They were much less
frequent than the commas so let’s see how often each of the separators
in this data appear in each row of our original dataframe.

We’ll separate on the closed bracket so that R will break everything
before the \] into a new column.

``` r
df2 <- df1 %>%
  tidyr::separate_wider_delim(AssessmentAnswers, delim = "]", names = c("chunk1", "chunk2", "chunk3", "chunk4"),
                              too_many = "debug")
```

    ## Warning: Debug mode activated: adding variables `AssessmentAnswers_ok`,
    ## `AssessmentAnswers_pieces`, and `AssessmentAnswers_remainder`.

My own naiive guess taking a quick glance at the first row of data was
that there would be 4 chunks. Note the warning produced here.

Because we included too_many = “debug” in the above R added new columns
that tell us for each row: 1. AssessmentAnswers_ok column - indicates
whether all values in the split were accounted for (TRUE/FALSE). 2.
AssessmentAnswers_pieces column - indicates how many chunks R was
expecting using \] as the delimiter. 3. AssessmentAnswers_remainder
column - identifies the values that were left out of the separation.

Let’s check out values in these 3 columns to investigate that warning
message.

``` r
table(df2$AssessmentAnswers_ok)
```

    ## 
    ## FALSE 
    ##   186

``` r
table(df2$AssessmentAnswers_pieces)
```

    ## 
    ##   6 
    ## 186

``` r
table(df2$AssessmentAnswers_remainder)
```

    ## 
    ##  ]] 
    ## 186

For all 186 rows in the data R is expecting 6 chunks (ie columns) where
we specified 4 chunks with our selected delimiter “\]”. Because of this
mis-match R flagged all 186 rows as FALSE in the AssessmentAnswers_ok
column.

But, because the only values in the AssessmentAnswers_remainder column
are separators we no longer need we can continue to break out columns
from these four chunks

# Starting with chunk1 -

Again peeking at the first few rows of data in the below we see several
\[ separators.

``` r
head(df2, 3) # view data
```

    ## # A tibble: 3 × 9
    ##          OMID chunk1 chunk2 chunk3 chunk4 AssessmentAnswers AssessmentAnswers_ok
    ##         <dbl> <chr>  <chr>  <chr>  <chr>  <chr>             <lgl>               
    ## 1     3.81e12 [[80,… ", (n… , [75… ", (n… "[[80, 21, 3, 2,… FALSE               
    ## 2     7.26e11 [[100… ", (n… , [84… ", (n… "[[100, 37, 6, 6… FALSE               
    ## 3     8.51e12 [[50,… ", (n… , [59… ", (n… "[[50, 50, 2, 4,… FALSE               
    ## # ℹ 2 more variables: AssessmentAnswers_pieces <int>,
    ## #   AssessmentAnswers_remainder <chr>

We know we’ll want to drop the first two \[\[ that appear in chunk1 for
every row because they aren’t meaningful. So we’ll first specify four
chunks so we can easily drop those first two \[ and account for the
third appearance of the \[ that occurs before the string of text
responses.

``` r
# Now, separate chunk1 into four pieces using the [ delimiter.
df3 <- df2 %>%
  tidyr::separate_wider_delim(chunk1, delim = "[", names = c("chunk1a", "chunk1b", "chunk1c", "chunk1d"),
                              too_many = "debug")
```

    ## Warning: Debug mode activated: adding variables `chunk1_ok`, `chunk1_pieces`, and
    ## `chunk1_remainder`.

Let’s see how that worked out by checking values of chunk1_ok to make
sure we’ve accounted for each value with our 4 new chunks (ie
chunk1a-chunk1d).

``` r
table(df3$chunk1_ok)
```

    ## 
    ## TRUE 
    ##  186

Returns TRUE for all 186 rows - these 4 new chunks are good to go.

But let’s peek at the values in the first few rows of this new data
frame to see what chunk1a-chunk1d now contain.

``` r
head(df3, 3)
```

    ## # A tibble: 3 × 16
    ##            OMID chunk1a chunk1b chunk1c   chunk1d chunk1 chunk1_ok chunk1_pieces
    ##           <dbl> <chr>   <chr>   <chr>     <chr>   <chr>  <lgl>             <int>
    ## 1 3807900000000 ""      ""      "80, 21,… |Peer … [[80,… TRUE                  4
    ## 2  726050000000 ""      ""      "100, 37… |If th… [[100… TRUE                  4
    ## 3 8505100000000 ""      ""      "50, 50,… |right… [[50,… TRUE                  4
    ## # ℹ 8 more variables: chunk1_remainder <chr>, chunk2 <chr>, chunk3 <chr>,
    ## #   chunk4 <chr>, AssessmentAnswers <chr>, AssessmentAnswers_ok <lgl>,
    ## #   AssessmentAnswers_pieces <int>, AssessmentAnswers_remainder <chr>

Chunk1a and chunk1b appear to be empty, as we would expect. Chunk1c
contains a mixture of integer and text responses, all separated between
commas. Chunk1d contains a series of text responses - with each response
appearing between a single set of pipes (ie \|some text here\|) - and
commas separating the pipes that contain the text responses.

Let’s confirm that chunk1a-chunk1b are empty by peeking at their values.

``` r
table(df3$chunk1a)
```

    ## 
    ##     
    ## 186

``` r
table(df3$chunk1b)
```

    ## 
    ##     
    ## 186

Now that we’ve confirmed these columns are empty we can now safely
remove them from the dataframe and proceed to breaking apart chunk1c
using the “,” delimiter.

But how many pieces should we expect? We can count the number of commas
that appear in each row to give us a hint about how many new columns
we’ll want to add to break up chunk1c. Better to do this with
reproducible code than relying on your eyeballs.

``` r
commas <- str_count(df3$chunk1c, pattern = ",") # count and store the number of commas appearing in each row in an object "commas"
table(commas) # summarize the number of rows in the commas object in which there are 22 or 23 commas 
```

    ## commas
    ##  22  23 
    ##  50 136

What the above values tell us is that: 1. We will want 23 new columns.
2. 50 rows contain only 22 commas in chunk1c and thus will have missing
data in one of the 23 columns. We’ll want to make sure we keep a lookout
for that in the output data.

Note that I named the new columns using some educated guesses based on
the values in each column.

``` r
df4 <- df3 %>%
  # let's first do some data cleanup by dropping columns we don't need anymore
  select(-c("chunk1a", "chunk1b", "chunk1":"chunk1_remainder", "AssessmentAnswers":"AssessmentAnswers_remainder")) %>%
  
  # Now, separate chunk1c into columns using the comma delimiter.
  separate_wider_delim(chunk1c, delim = ",", names=c("Q1_pre", "Q2_pre", "Q3_pre", "Q4_pre", "Q5_pre", "Q6_pre",
                                                     "Q7_pre", "Q8_pre", "Q9_pre", "Q10_pre", "Q11_pre", "Q12_pre",
                                                     "Q13_pre", "Q14_pre", "Q15_pre", "Q16_pre", "Age", "Sex", 
                                                     "Race_ethnicity", "Political_ideology", "Religion", "Zip", 
                                                     "Condition_pre"), too_many = "debug")
```

    ## Warning: Debug mode activated: adding variables `chunk1c_ok`, `chunk1c_pieces`, and
    ## `chunk1c_remainder`.

Investigating the warning message with the tables below to confirm what
we learned in the last step.

``` r
table(df4$chunk1c_ok) # this actually misleads us! Here the rows flagged as FALSE actually appear correctly in the data.
```

    ## 
    ## FALSE  TRUE 
    ##   136    50

``` r
table(df4$chunk1c_remainder) # but to clearly see that we have to look at values in this column. We see 136 rows with a comma and 50 rows that are empty cells. 
```

    ## 
    ##      ,  
    ##  50 136

``` r
table(df4$chunk1c_pieces) # here we see our 50 cases with only 23 pieces (instead of 24) - let's find the source of the missing data
```

    ## 
    ##  23  24 
    ##  50 136

Now lets take a quick peek at the first few rows of data in df4.

``` r
head(df4, 3)
```

    ## # A tibble: 3 × 32
    ##      OMID Q1_pre Q2_pre Q3_pre Q4_pre Q5_pre Q6_pre Q7_pre Q8_pre Q9_pre Q10_pre
    ##     <dbl> <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  
    ## 1 3.81e12 80     " 21"  " 3"   " 2"   " 2"   " 2"   " 5"   " 4"   " 5"   " 6"   
    ## 2 7.26e11 100    " 37"  " 6"   " 6"   " 6"   " 4"   " 1"   " 6"   " 6"   " 6"   
    ## 3 8.51e12 50     " 50"  " 2"   " 4"   " 5"   " 6"   " 0"   " 4"   " 4"   " 3"   
    ## # ℹ 21 more variables: Q11_pre <chr>, Q12_pre <chr>, Q13_pre <chr>,
    ## #   Q14_pre <chr>, Q15_pre <chr>, Q16_pre <chr>, Age <chr>, Sex <chr>,
    ## #   Race_ethnicity <chr>, Political_ideology <chr>, Religion <chr>, Zip <chr>,
    ## #   Condition_pre <chr>, chunk1c <chr>, chunk1c_ok <lgl>, chunk1c_pieces <int>,
    ## #   chunk1c_remainder <chr>, chunk1d <chr>, chunk2 <chr>, chunk3 <chr>,
    ## #   chunk4 <chr>

Separating chunk1c on the comma delimiter did a pretty job populating
new columns here.

Where the above code for splitting chunk1c breaks down is somewhere
between the Political_ideology column and the Condition_pre column.

Chunk1d and chunk2-chunk4 were unaffected, so we’ll focus on the
Political_ideology, Religion, Zip, and Condition_pre columns.

``` r
table(df4$Political_ideology) # 11 unique values appear in this column - all seem to be reasonable responses to the corresponding item
```

    ## 
    ##             Conservative/right       Don't know/not political 
    ##                             12                             29 
    ##  Libertarian/classical liberal    Moderate/middle-of-the-road 
    ##                              6                             39 
    ##                          Other              Prefer not to say 
    ##                              1                              9 
    ##               Progressive/left    Slightly conservative/right 
    ##                             31                             18 
    ##      Slightly progressive/left        Very conservative/right 
    ##                             22                              6 
    ##          Very progressive/left 
    ##                             13

``` r
table(df4$Religion) # these values are a mix of character and numeric responses, along with a "Prefer not to say" response.
```

    ## 
    ##                                         02026 
    ##                                             1 
    ##                                         06903 
    ##                                             1 
    ##                                         10459 
    ##                                             1 
    ##                                         10460 
    ##                                             1 
    ##                                         14618 
    ##                                             1 
    ##                                         33176 
    ##                                             1 
    ##                                         36830 
    ##                                             1 
    ##                                         37909 
    ##                                             1 
    ##                                         43016 
    ##                                             1 
    ##                                         45011 
    ##                                             1 
    ##                                         46168 
    ##                                             1 
    ##                                         46202 
    ##                                             2 
    ##                                         46205 
    ##                                             1 
    ##                                         46214 
    ##                                             1 
    ##                                         60022 
    ##                                             1 
    ##                                         72575 
    ##                                             1 
    ##                                         75007 
    ##                                             2 
    ##                                         75010 
    ##                                             1 
    ##                                         75038 
    ##                                             1 
    ##                                         75205 
    ##                                             1 
    ##                                         75220 
    ##                                             1 
    ##                                         75229 
    ##                                             1 
    ##                                         75248 
    ##                                             1 
    ##                                         78332 
    ##                                             2 
    ##                                         78343 
    ##                                             1 
    ##                                         78363 
    ##                                             1 
    ##                                         78372 
    ##                                             1 
    ##                                         78380 
    ##                                             2 
    ##                                         90630 
    ##                                             1 
    ##                                         91355 
    ##                                             1 
    ##                                         91606 
    ##                                             1 
    ##                                         92131 
    ##                                             1 
    ##                                         93221 
    ##                                             1 
    ##                                      Agnostic 
    ##                                            16 
    ##                                       Atheist 
    ##                                             9 
    ##                                      Buddhist 
    ##                                             1 
    ##                                         Hindu 
    ##                                             6 
    ##                                        Jewish 
    ##                                             2 
    ##                                        Muslim 
    ##                                             6 
    ##                                        N2T1T4 
    ##                                             1 
    ##                         Nothing in particular 
    ##                                            21 
    ##  Orthodox (such as Greek or Russian Orthodox) 
    ##                                             2 
    ##                             Prefer not to say 
    ##                                            12 
    ##                                    Protestant 
    ##                                            20 
    ##                                Roman Catholic 
    ##                                            34 
    ##                                     Shintoist 
    ##                                             1 
    ##                                          Sikh 
    ##                                             1 
    ##                                Something else 
    ##                                            17

``` r
table(df4$Zip) # these values are a mix of character and numeric responses, along with a "Prefer not to say" response.
```

    ## 
    ##              02554              06040              06109              06114 
    ##                  1                  1                  1                  1 
    ##              06831              10510              10576               1124 
    ##                  1                  1                  1                  1 
    ##              14424               1609              19454              19807 
    ##                  1                  1                  1                  1 
    ##              21043              21048              21085              21113 
    ##                  1                  1                  1                  1 
    ##              21206              21210              21212              21230 
    ##                  1                  1                  2                  1 
    ##              21239              21252              21403              30458 
    ##                  1                  1                  1                  2 
    ##              30461              32204              33487              35803 
    ##                  1                  1                  1                  1 
    ##              36856              45011              45013              45014 
    ##                  1                  1                  1                  1 
    ##              45044              45069              45251              45309 
    ##                  1                  1                  1                  1 
    ##              46112              46202              46224              46236 
    ##                  2                  1                  2                  1 
    ##              49015              60026              60623              60631 
    ##                  1                  1                  1                  1 
    ##              60640              62226              63141              75038 
    ##                  1                  1                  2                  1 
    ##              75070              75082              75201              75204 
    ##                  2                  1                  1                  1 
    ##              75205              75206              75209              75229 
    ##                  1                  3                  1                  1 
    ##              77433              78628              78705              78751 
    ##                  1                  1                  1                  1 
    ##              79703              79707              89011              90405 
    ##                  2                  1                  1                  1 
    ##              95632              98686                 B1                CM1 
    ##                  1                  1                 50                  1 
    ##             Durham            L0C 1A0             l1v7c5             L4C2S6 
    ##                  1                  1                  1                  1 
    ##             L5W1E2             L7A2N5             m3j0b2            M4N 0A7 
    ##                  1                  1                  1                  1 
    ##             m5s2x1             N2L0E1            N2T 0A1             N2V1G4 
    ##                  1                  1                  1                  1 
    ##  Prefer not to say            T3G 4E6                V1W 
    ##                 46                  1                  1

``` r
# most notably, "B1" appears 50 times in this column - and we know that the adjacent Condition_pre column should contain these values.

table(df4$Condition_pre) # confirms the missing data for the 50 rows in which values of Condition_pre (ie B1) are instead contained in Zip.
```

    ## 
    ##      B1 
    ##  50 136

Collectively, the tables above tell us that the Religion column is the
underlying culprit - and our source of missing data.

Something VERY important to note about the “missing” values currently
contained in Condition_pre:

Although our eyes may digest those 50 cases as “missing” - R does not
know that they are missing, just that they are blank/empty cells. So, we
need to tell R to treat those empty cells as missing values.

Now we’ll use some case logic to fix the values in these columns.

``` r
df5 <- df4 %>%
  
  mutate(Condition_pre_r = case_when(
    Condition_pre == " " ~ NA_character_,
    !is.na(Condition_pre) ~ as.character(Condition_pre))) %>%
  
  mutate(Zip_r = case_when(
    is.na(Condition_pre_r) ~ as.character(Religion),
    !is.na(Condition_pre_r) ~ as.character(Zip))) %>%
  
  mutate(Religion_r = case_when(
    is.na(Condition_pre_r) ~ NA_character_,
    !is.na(Condition_pre_r) ~ as.character(Religion))) %>%
  
  select(-c(Religion, Zip, Condition_pre, chunk1c:chunk1c_remainder))
```

We’ll leave chunk1d alone for now as those responses appear to represent
responses to a single item/question. Note the pipes \|\| that wrap the
values in this column.

# Moving on to chunk2 -

We’ll repeat how we started off breaking up chunk1c by noticing that in
the chunk2 column 6 commas appear in each of the 186 rows.

``` r
commas <- str_count(df5$chunk2, pattern = ",") # count and store the number of commas appearing in each row in an object "commas"
table(commas) # summarize the number of rows in the commas object 
```

    ## commas
    ##   6 
    ## 186

``` r
df6 <- df5 %>%
  separate_wider_delim(chunk2, delim = ",", names=c("chunk2a", "chunk2b", "chunk2c", "chunk2d", "chunk2e", "chunk2f", "chunk2g"),
                       too_many="debug")
```

    ## Warning: Debug mode activated: adding variables `chunk2_ok`, `chunk2_pieces`, and
    ## `chunk2_remainder`.

Then investigate the warning message with the tables below.

``` r
table(df6$chunk2_ok) 
```

    ## 
    ## TRUE 
    ##  186

``` r
table(df6$chunk2_remainder) 
```

    ## 
    ##     
    ## 186

``` r
table(df6$chunk2_pieces)
```

    ## 
    ##   7 
    ## 186

We’re all set!

# On to breaking up chunk3

Take a peek at the chunk3 column for just the first row. You’ll see lots
of commas and a couple of brackets.

``` r
df6$chunk3[1] 
```

    ## [1] ", [75, 37, 1, 4, 3, 5, 5, 6, 6, 6, 6, Yes, (not asked), (not asked), (not asked), (not asked), 18, Female, (White/Caucasian), Slightly progressive/left, 60022, B1, [|Good candidates|, |Better Laws|, |Better Representatives|, |Worse Laws|, |Worse canadites|, |Worse tax breaks|"

``` r
commas <- str_count(df6$chunk3, pattern = ",") # count and store the number of commas appearing in each row in an object "commas"
table(commas) # summarize the number of rows in the commas object 
```

    ## commas
    ##  28  29  30  31  32  33 
    ##  47 131   3   2   2   1

The number of commas in each row of chunk3 varies from 28-33. That’s
more mess than we’d like to correct.

Let’s do some cleanup again by dropping columns we don’t need before
breaking apart chunk3 using the “\[” delimiter.

``` r
df7 <- df6 %>%
  select(-c("chunk2a", "chunk2d":"chunk2f", "chunk2":"chunk2_remainder")) %>%
  separate_wider_delim(chunk3, delim = "[", names=c("chunk3a", "chunk3b", "chunk3c"), too_many="debug")
```

    ## Warning: Debug mode activated: adding variables `chunk3_ok`, `chunk3_pieces`, and
    ## `chunk3_remainder`.

Then investigate the warning message with the tables below. Everything
checks out here.

``` r
table(df7$chunk3_ok) 
```

    ## 
    ## TRUE 
    ##  186

``` r
table(df7$chunk3_remainder) 
```

    ## 
    ##     
    ## 186

``` r
table(df7$chunk3_pieces)
```

    ## 
    ##   3 
    ## 186

Take a peek at the chunk3b column for just the first row. You’ll see a
number of commas. Let’s count them to tell us how many new columns we’ll
want.

``` r
df7$chunk3b[1] 
```

    ## [1] "75, 37, 1, 4, 3, 5, 5, 6, 6, 6, 6, Yes, (not asked), (not asked), (not asked), (not asked), 18, Female, (White/Caucasian), Slightly progressive/left, 60022, B1, "

``` r
commas <- str_count(df7$chunk3b, pattern = ",") # count and store the number of commas appearing in each row in an object "commas"
table(commas) # summarize the number of rows in the commas object 
```

    ## commas
    ##  22  23 
    ##  50 136

What the above values tell us is that: 1. We will want 23 new columns.
2. 50 rows contain only 22 commas in chunk3b and thus will have missing
data in one of the 23 columns.

Sound familiar? Remember that this dataset contain pre and post values
that were assessed before and after an experimental condition. Again,
we’ll want to make sure we keep a lookout for that in the output data.

Note that I named the new columns following the educated guesses I made
for the postfixed \_pre columns.

Let’s do some cleanup again by dropping columns we don’t need before
breaking apart chunk3b using the “,” delimiter.

``` r
df8 <- df7 %>%
  select(-c("chunk3a", "chunk3":"chunk3_remainder")) %>%
  separate_wider_delim(chunk3b, delim = ",", names=c("Q1_post", "Q2_post", "Q3_post", "Q4_post", "Q5_post", "Q6_post",
                                                    "Q7_post", "Q8_post", "Q9_post", "Q10_post", "Q11_post", "Q12_post",
                                                    "Q13_post", "Q14_post", "Q15_post", "Q16_post", "Age_post", "Sex_post",
                                                    "Race_ethnicity_post", "Political_ideology_post", "Religion_post", "Zip_post",
                                                    "Condition_post"),
                                            too_many="debug")
```

    ## Warning: Debug mode activated: adding variables `chunk3b_ok`, `chunk3b_pieces`, and
    ## `chunk3b_remainder`.

``` r
table(df8$chunk3b_ok)
```

    ## 
    ## FALSE  TRUE 
    ##   136    50

``` r
table(df8$chunk3b_pieces)
```

    ## 
    ##  23  24 
    ##  50 136

``` r
table(df8$chunk3b_remainder)
```

    ## 
    ##      ,  
    ##  50 136

Notice that we’ll have the same problems here with the columns output
from chunk3b as we did when breaking up column 1c

``` r
table(df8$Political_ideology_post) # 11 unique values appear in this column - all seem to be reasonable responses to the corresponding item
```

    ## 
    ##             Conservative/right       Don't know/not political 
    ##                             12                             29 
    ##  Libertarian/classical liberal    Moderate/middle-of-the-road 
    ##                              6                             39 
    ##                          Other              Prefer not to say 
    ##                              1                              9 
    ##               Progressive/left    Slightly conservative/right 
    ##                             31                             18 
    ##      Slightly progressive/left        Very conservative/right 
    ##                             22                              6 
    ##          Very progressive/left 
    ##                             13

``` r
table(df8$Religion_post) # these values are a mix of character and numeric responses, along with a "Prefer not to say" response.
```

    ## 
    ##                                         02026 
    ##                                             1 
    ##                                         06903 
    ##                                             1 
    ##                                         10459 
    ##                                             1 
    ##                                         10460 
    ##                                             1 
    ##                                         14618 
    ##                                             1 
    ##                                         33176 
    ##                                             1 
    ##                                         36830 
    ##                                             1 
    ##                                         37909 
    ##                                             1 
    ##                                         43016 
    ##                                             1 
    ##                                         45011 
    ##                                             1 
    ##                                         46168 
    ##                                             1 
    ##                                         46202 
    ##                                             2 
    ##                                         46205 
    ##                                             1 
    ##                                         46214 
    ##                                             1 
    ##                                         60022 
    ##                                             1 
    ##                                         72575 
    ##                                             1 
    ##                                         75007 
    ##                                             2 
    ##                                         75010 
    ##                                             1 
    ##                                         75038 
    ##                                             1 
    ##                                         75205 
    ##                                             1 
    ##                                         75220 
    ##                                             1 
    ##                                         75229 
    ##                                             1 
    ##                                         75248 
    ##                                             1 
    ##                                         78332 
    ##                                             2 
    ##                                         78343 
    ##                                             1 
    ##                                         78363 
    ##                                             1 
    ##                                         78372 
    ##                                             1 
    ##                                         78380 
    ##                                             2 
    ##                                         90630 
    ##                                             1 
    ##                                         91355 
    ##                                             1 
    ##                                         91606 
    ##                                             1 
    ##                                         92131 
    ##                                             1 
    ##                                         93221 
    ##                                             1 
    ##                                      Agnostic 
    ##                                            16 
    ##                                       Atheist 
    ##                                             9 
    ##                                      Buddhist 
    ##                                             1 
    ##                                         Hindu 
    ##                                             6 
    ##                                        Jewish 
    ##                                             2 
    ##                                        Muslim 
    ##                                             6 
    ##                                        N2T1T4 
    ##                                             1 
    ##                         Nothing in particular 
    ##                                            21 
    ##  Orthodox (such as Greek or Russian Orthodox) 
    ##                                             2 
    ##                             Prefer not to say 
    ##                                            12 
    ##                                    Protestant 
    ##                                            20 
    ##                                Roman Catholic 
    ##                                            34 
    ##                                     Shintoist 
    ##                                             1 
    ##                                          Sikh 
    ##                                             1 
    ##                                Something else 
    ##                                            17

``` r
table(df8$Zip_post) # these values are a mix of character and numeric responses, along with a "Prefer not to say" response.
```

    ## 
    ##              02554              06040              06109              06114 
    ##                  1                  1                  1                  1 
    ##              06831              10510              10576               1124 
    ##                  1                  1                  1                  1 
    ##              14424               1609              19454              19807 
    ##                  1                  1                  1                  1 
    ##              21043              21048              21085              21113 
    ##                  1                  1                  1                  1 
    ##              21206              21210              21212              21230 
    ##                  1                  1                  2                  1 
    ##              21239              21252              21403              30458 
    ##                  1                  1                  1                  2 
    ##              30461              32204              33487              35803 
    ##                  1                  1                  1                  1 
    ##              36856              45011              45013              45014 
    ##                  1                  1                  1                  1 
    ##              45044              45069              45251              45309 
    ##                  1                  1                  1                  1 
    ##              46112              46202              46224              46236 
    ##                  2                  1                  2                  1 
    ##              49015              60026              60623              60631 
    ##                  1                  1                  1                  1 
    ##              60640              62226              63141              75038 
    ##                  1                  1                  2                  1 
    ##              75070              75082              75201              75204 
    ##                  2                  1                  1                  1 
    ##              75205              75206              75209              75229 
    ##                  1                  3                  1                  1 
    ##              77433              78628              78705              78751 
    ##                  1                  1                  1                  1 
    ##              79703              79707              89011              90405 
    ##                  2                  1                  1                  1 
    ##              95632              98686                 B1                CM1 
    ##                  1                  1                 50                  1 
    ##             Durham            L0C 1A0             l1v7c5             L4C2S6 
    ##                  1                  1                  1                  1 
    ##             L5W1E2             L7A2N5             m3j0b2            M4N 0A7 
    ##                  1                  1                  1                  1 
    ##             m5s2x1             N2L0E1            N2T 0A1             N2V1G4 
    ##                  1                  1                  1                  1 
    ##  Prefer not to say            T3G 4E6                V1W 
    ##                 46                  1                  1

``` r
# most notably, "B1" appears 50 times in this column - and we know that the adjacent Condition_pre column should contain these values.

table(df8$Condition_post) # confirms the missing data for the 50 rows in which values of Condition_post (ie B1) are instead contained in Zip_post.
```

    ## 
    ##      B1 
    ##  50 136

Collectively, the tables above tell us that, again, the Religion column
is the underlying culprit - and our source of missing data.

Something important enough to call out again is that the 50 values
currently contained in Condition_post that appear to be “missing” at
first glance are blank/empty cells.

So, we need to use some case logic once again to tell R to treat those
empty cells as missing values.

``` r
df9 <- df8 %>%
  
  mutate(Condition_post_r = case_when(
    Condition_post == " " ~ NA_character_,
    !is.na(Condition_post) ~ as.character(Condition_post))) %>%
  
  mutate(Zip_post_r = case_when(
    is.na(Condition_post_r) ~ as.character(Religion_post),
    !is.na(Condition_post_r) ~ as.character(Zip_post))) %>%
  
  mutate(Religion_post_r = case_when(
    is.na(Condition_post_r) ~ NA_character_,
    !is.na(Condition_post_r) ~ as.character(Religion_post))) %>%
  
  select(-c(Religion_post, Zip_post, Condition_post, chunk3b:chunk3b_remainder))
```

Now let’s see if responses to those \_pre and \_post demographic
questions are the same - as we might expect given the nature of these
questions. If the values match up across the two sets of items we can
safely drop the \_post columns and retain the \_pre columns.

We’ll skip chunk3c - the text responses contained within pipes - for now
for the same reasons we skipped over chunk1d.

# Finally, breaking apart chunk4

We’ll use the “,” delimiter again here.

``` r
df9$chunk4[1] 
```

    ## [1] ", (not asked), (not asked), {\"is_gt\" -> true, \"type\" -> date, \"moment\" -> \"2019-02-11T04:36:04.112Z\", \"timestamp\" -> 2019-02-10T23:36:04-05:00}"

``` r
commas <- str_count(df9$chunk4, pattern = ",") # count and store the number of commas appearing in each row in an object "commas"
table(commas) # summarize the number of rows in the commas object
```

    ## commas
    ##   6 
    ## 186

``` r
df10 <- df9 %>%
  separate_wider_delim(chunk4, delim = ",", names=c("chunk4a", "chunk4b", "chunk4c", "chunk4d", "chunk4e", "chunk4f", "chunk4g"),
                       too_many="debug")
```

    ## Warning: Debug mode activated: adding variables `chunk4_ok`, `chunk4_pieces`, and
    ## `chunk4_remainder`.

``` r
table(df10$chunk4_ok)
```

    ## 
    ## TRUE 
    ##  186

``` r
table(df10$chunk4_pieces)
```

    ## 
    ##   7 
    ## 186

``` r
table(df10$chunk4_remainder)
```

    ## 
    ##     
    ## 186

## Now for some final cleanup and exploration

``` r
df11 <- df10 %>%
  select(OMID:Political_ideology, Religion = Religion_r, Zip = Zip_r, Condition_pre = Condition_pre_r, 
         Text_responses_pre = chunk1d, chunk2b:chunk2c, TimeStampTxt_pre = chunk2g, 
         Q1_post:Q16_post, Age_post:Political_ideology_post, 
         Religion_post = Religion_post_r, Zip_post = Zip_post_r, Condition_post = Condition_post_r,
         Text_responses_post = chunk3c, chunk4b:chunk4c, TimeStampTxt_post = chunk4g)

# fix condition cols
# need to make the time stamp cols into class = date
# remove all unnecessary parentheses
# decide if I want to do anything else with the text response columns in chunk1d and chunk3c
```

## Including Plots

You can also embed plots, for example:

![](tidyingmessydata_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
