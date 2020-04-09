#Packages needed
#install.packages("tibble") # Package to have data in tabular format.
#install.packages("readxl") # Package to read Excel Files into R.
#install.packages("dplyr") # Package in r similar to Pandas
#install.packages("stringr") # Package with advanced String functions

# Load Required Packages into R
library("tibble")
library("readxl")
library("dplyr")


raw_xl = read_excel("C:\\\\Users\\\\Jonathan McAwesome\\\\Dropbox\\\\Projects\\\\vmprework\\\\Source Documentation\\\\Data intern test (1).xlsx")
#typeof(raw_xl) #Test data type
#Analys of DATA
summary(raw_xl)
## Notes:
## o) Data Only recieved for desired timeslot to be analysed, confirmed as per brief.
## o) 50,471 log records - need to remove ID's not apearing more than 2x

#Figuring out String splitting in r
urlex = "https://www.timeslive.co.za/tshisa-lisve/tshisa-live/2019-12-16-watch-relax-fam-rick-ross-is-defs-still-coming-to-sa/"

mid = function(text,start_num,num_char){
  substr(text,start_num,start_num+num_char-2)
}

mid(urlex,29,40-26)
# Starting Character in these URL's will always be at pos 29 when looking for 
# Character.
# Because this is 28 Characters Long: https://www.timeslive.co.za/
# Need to figure out how to determine the next / in the url.

# Testing URL's
test1 <- "https://www.timeslive.co.za/news/south-africa/2019-12-17-kings-parole-half-the-battle-is-won-the-rest-is-up-to-cyril/"
test2 <- "https://www.timeslive.co.za/politics/2019-06-11-r36m-fee-paid-to-gupta-linked-firm-left-auditors-scratching-their-heads/"
test3 <- "https://www.timeslive.co.za/tshisa-live/tshisa-live/2019-11-11-faith-nketsi-causes-a-storm-with-new-ep-disrespectful/"

getCatagory = function(textinquestion,x=29){
  ## Function that removes root URL: https://www.timeslive.co.za/
  ## Evaluates the text that follows the Root URL and grabs the first text up 
  ## to the first "/" which I've determined will leave the catagories of the 
  ## Articles.
  
    
    catgExclMainUrl = substring(textinquestion,x,nchar(textinquestion))
    posFirstSlash = regexpr(pattern ="/",catgExclMainUrl)
    posFirstSlashx = posFirstSlash[1]-1
    print(posFirstSlashx)
    catagory = substring(catgExclMainUrl,1,posFirstSlashx)
    return(catagory)
  
  ## In one line
  ## return(substring(substring(textinquestion,29,nchar(textinquestion)),1,regexpr(pattern ="/",substring(textinquestion,29,nchar(textinquestion)))[1]-1))
}

test_getCatagory= function(){
  #Funtion will evaluate True if getCatagory function works as expected.
  a <- getCatagory(test1)=="news"
  b <- getCatagory(test2)=="politics"
  c <- getCatagory(test3)=="tshisa-live"
  print(a+b+c==3)
}
test_getCatagory()

getCatagory("politics/20",1)
nchar("hello")[1]

## Code below to remove items(ID's) not apearing at least 3 or more times
## Stackoverflow: https://stackoverflow.com/questions/3919205/using-r-delete-rows-when-a-value-repeated-less-than-3-times
data_xl <- raw_xl[raw_xl$vicinity_id %in% names(which(table(raw_xl$vicinity_id) >= 3)), ]

#Now wanting to remove any lines that only log when the user accessed the base website url.
data_xl2 <- filter(data_xl,url!="https://www.timeslive.co.za/")
data_xl3 <- data_xl2

data_xl3 <- transform(data_xl3,catagory=getCatagory(data_xl3[,3]))
data_xl4 <- apply(data_xl3,1)



#R to write to CSV if checking output
#write.csv(data_xl,"C:\\Users\\Jonathan McAwesome\\Dropbox\\Projects\\vmprework\\cc.csv", row.names = FALSE)
