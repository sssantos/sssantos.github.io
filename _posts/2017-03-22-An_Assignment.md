---
layout:     post
title:      "Text Scraping Car Advetisements"
subtitle:   "A STA141A Assignment done in R"
date:       2014-06-10 12:00:00
author:     "Colin Santos"
header-img: "img/post-bg-01.jpg"
---

<hr align = right>Colin Santos</right>
<h1>Text Scraping Car Advertisments</h1>
<p> 
This markdown features my code from part 1 of homework 4 from STA141A Fall 2016. I will be extracting and sorting data from <a href="https://github.com/sssantos/Course-Datasets">a directory of car advertisement text files</a> into a data frame.
</p>
<p>
<strong>End Result:</strong> Obtain a data frame with values from the text advertisements sorted into the followering columns
</p>
<ul>
<li>Year</li>
<li>Make</li>
<li>Model</li>
<li>VIN</li>
<li>Price (USD)</li>
<li>Mileage</li>
<li>Interior</li>
<li>Exterior</li>
<li>Transmission</li>
<li>Engine displacement (L)</li>
<li>Company</li>
<li>Street</li>
<li>State</li>
<li>Zip Code</li>
<li>Phone Number</li>
<li>Website</li>
</ul>
<p>
<h2>Preperation</h2>
<p>
The stringr library will be utilized for text processing and the directory will be set to the one containing the advertisement text files.
</p>


```R
library(stringr)
setwd("/Users/sssantos/Documents/STA141A/HW4/CarAdvert")
```

Now an empty data frame will be prepared.


```R
# Form list of data frames of the files using the file names
file_names <- list.files()
file_text <- sapply(file_names, FUN = readLines)

# Construct the data frame to be filled out
info_names <- c("Year", "Make", "Model", "VIN", "Price (USD)",
                "Mileage", "Interior", "Exterior", "Transmission",
                "Engine displacement (L)", "Company", "Street", "State", "Zip Code", "Phone number","Website")
car_adverts <- data.frame(matrix(ncol = length(info_names), nrow = length(file_text)))
names(car_adverts) <- info_names
```

<h3>
User-defined functions for the task
</h3>
This function is used to separate a file of text into a vector words


```R
unlist.words <- function(text) {
  unlist(lapply(text,strsplit,split=" "))
}
```

This function returns 30 characters of text after given keyword for 'n' samples. It is used in order to help determine considerations for regex ending boundaries when capturing values.


```R
look.ahead <- function(keyword, n = length(file_text)) { 
  sample(sapply(file_text, FUN = function(text) {
    substring(strsplit(paste(text, collapse = " "), split = keyword)[[1]][2],1,30)
  }), size = n )
}
```

<p>
This function is used for extracting the full address (specifically for this data). It takes the file_text and returns the address. The capture is done by isolating contents between "Address:" and "Phone"
</p>


```R
full.address <- function(text) {
  value <- gsub("Address:\\s|Phone.*", "", str_extract(text, "Address:.*Phone:"))
  if(all(is.na(value))) {NA}
  else {str_trim(value[which(!is.na(value))], side = "both")}
}
```

<h2>Extracting the data</h2>
<h3>Year</h3>
<p>
First, I extract the year of the car advertised by searching for the first word with exactly 4 continguous digits. 
</p>


```R
car_adverts["Year"] <- as.numeric(sapply(file_text, FUN = function(text) {
  unlist(grep("^[0-9]{4}$", unlist.words(text) , value = TRUE))[1]
}))

```

<h3>VIN</h3>
<p>
The VIN is found by taking the 17 character word after the instance of "VIN:" in the text file.
</p>


```R
car_adverts["VIN"] <- sapply(file_text, FUN = function(text) {
  x = substring(unlist.words(text)[(agrep("VIN:", unlist.words(text), max = 1) + 1)],1,17)
  if(nchar(x) == 17) {x}
  else {NA}
})
```

<h3>Price</h3>
<p>
The price of the car is found by searching for the first price in USD.
</p>


```R
car_adverts["Price (USD)"] <- suppressWarnings(sapply(file_text, FUN = function(text) {
  price <- grep("\\$(\\d{1,3}(\\,\\d{3})*|\\d+)(\\.\\d{1,2})?", unlist.words(text) , value = TRUE)[1]
  as.numeric(gsub(",|\\$", "", price))
}))

```

<h3>Mileage</h3>
<p>
The used mileage of the car is found by seraching for the number after the word "UsedMileage:" in the textfile
</p>


```R
car_adverts["Mileage"] <- sapply(file_text, FUN = function(text) {
  location <- agrep("UsedMileage:", unlist.words(text), max = 3) + 1
  if(length(location) == 0) {
    return(NA)
  } else {
  # Check for a number 
      grep("^([0-9]{3},)*[0-9]{1,3}", 
           strsplit(unlist.words(text)[[location]], split = "(?=\\D)(?=[^,])", perl = TRUE)[[1]], value = TRUE)
  }
})
```

<h3>Exterior</h3>
<p>
The pattern of words following car exterior information doesn't appear consistent when skimming through the text files, which makes it difficult to tell when the exterior information ends. The look.ahead function will give an idea of when exterior information ends in the text.
</p>


```R
head(look.ahead("Exterior:",30))
```


<dl class=dl-horizontal>
	<dt>491.txt</dt>
		<dd>' BlackInterior: BlackBody: HSE'</dd>
	<dt>57.txt</dt>
		<dd>' Noisette MetallicInterior: Da'</dd>
	<dt>1274.txt</dt>
		<dd>' Liquid PlatinumInterior: Grap'</dd>
	<dt>102.txt</dt>
		<dd>' Indigo Blue PearlInterior: Gr'</dd>
	<dt>628.txt</dt>
		<dd>' Black Sapphire PearlBody: 4dr'</dd>
	<dt>824.txt</dt>
		<dd>' GrayBody: BaseTransmission: A'</dd>
</dl>



<p>
The exterior information seems to end just before "Body", "Transmission", or "Interior" keyword appears. str_extract is used to take the exterior information including the immediate surrounding words, and gsub is used to  isolate just the exterior information.
</p>


```R
car_adverts["Exterior"] <- sapply(file_text, FUN = function(text) {
  color <- tolower(gsub("Exterior:\\s|Body.*|Transmission.*|Interior.*|[Uu]nspecified*", "",
                        str_extract(text, "Exterior:\\s*.*(Body|Transmission|Interior)")))
  if(all(is.na(color))) {NA}
  else{str_trim(color[which(!is.na(color))], side = "both")}
})
```

<h3>Interior</h3>
<p>
Checking for the words that mark the end of Interior information works similarly. 
</p>


```R
head(look.ahead("Interior:", 30))
```


<dl class=dl-horizontal>
	<dt>822.txt</dt>
		<dd>' GrayBody: 2.5 SLTransmission:'</dd>
	<dt>595.txt</dt>
		<dd>' Dk/Light FlintBody: 4dr CarTr'</dd>
	<dt>1476.txt</dt>
		<dd>' BlackBody: 2D CoupeTransmissi'</dd>
	<dt>60.txt</dt>
		<dd>' Black ClothBody: SedanTransmi'</dd>
	<dt>908.txt</dt>
		<dd>' BlackBody: 35 AWDTransmission'</dd>
	<dt>343.txt</dt>
		<dd>'NA'</dd>
</dl>



<p>
Interior information in the text seems to end at "Body" and "Transmission". The same process used in finding <strong>Exterior</strong> information will be used to extract the interior information. 
</p>


```R
car_adverts["Interior"] <- sapply(file_text, FUN = function(text) {
  color <- tolower(gsub("Interior:\\s|Body.*|Transmission.*|([Nn]o [Cc]olor.*)|([Uu]nspecified.*)", "",
                        str_extract(text, "Interior:\\s*.*(Body|Transmission)")))
  if(all(is.na(color))) {NA}
  else {
    # Change color names to be consistent 
    color <- gsub("blk", "black", color)
    color <- gsub("lthr", "leather", color)
    color <- gsub("md", "medium", color)
    str_trim(color[which(!is.na(color))], side = "both")  
  }
})
```

<h3>Transmission</h3>


```R
head(look.ahead("Transmission:", 30))
```


<dl class=dl-horizontal>
	<dt>1087.txt</dt>
		<dd>' AutomaticEngine: 3.5L V6 DOHC'</dd>
	<dt>32.txt</dt>
		<dd>' Automatic 6-SpeedEngine: 3.5L'</dd>
	<dt>541.txt</dt>
		<dd>' AutomaticEngine: Gas V6 3.7LL'</dd>
	<dt>786.txt</dt>
		<dd>' AutomaticEngine: 2.5 4 Cylind'</dd>
	<dt>388.txt</dt>
		<dd>' 6-Speed AutomaticEngine: Vort'</dd>
	<dt>337.txt</dt>
		<dd>'NA'</dd>
</dl>



<p>
Transmission info ends at "Engine". Extracting the information follows the same process as the <strong>Exterior</strong> step.
</p>


```R
car_adverts["Transmission"] <- sapply(file_text, FUN = function(text) {
  value <- gsub("Transmission:\\s|Engine.*", "", str_extract(text, "Transmission:.*(Engine)"))
  if(all(is.na(value))) {NA}
  else{str_trim(value[which(!is.na(value))], side = "both")}
})

```

"Unspecified" cases should be made into NA values and misspellings or capitilization of "Automatic" should be consistent.


```R
car_adverts$Transmission[which(car_adverts$Transmission == "Unspecified")] = NA

automatic <- agrep("Automatic", car_adverts$Transmission, max = 2 )
car_adverts$Transmission[automatic] = "Automatic"
car_adverts$Transmission <- sapply(car_adverts$Transmission, str_trim, side = "both")

```

<h3>Engine displacement</h3>


```R
car_adverts["Engine displacement (L)"] <- sapply(file_text, FUN = function(text) {
  # gsub'ed "0.0" out for cleaning, and removed all cases of "L"
  value <- as.numeric(gsub("0.0", "", str_trim(gsub("L|/", "", str_extract(list(text), "([0-9]\\.[0-9][L/]?)|(^[0-9]$)")), side = "both")))
})
```

<h3>Phone number</h3>


```R
car_adverts["Phone number"] <- sapply(file_text, FUN = function(text) {
  str_extract(list(text),"(\\([2-9][0-9]{2}\\))[- .]([0-9]{3})[- .]([0-9]{4})")
})
```

<h3>Website</h3> 
Extracting the information follows the same process as the <strong>Exterior</strong> step.



```R
head(look.ahead("Website", 30))
```


<dl class=dl-horizontal>
	<dt>845.txt</dt>
		<dd>': www.lendenusedcarsales.comAd'</dd>
	<dt>824.txt</dt>
		<dd>': usa1nyc.comAddress: 12047 Fl'</dd>
	<dt>1184.txt</dt>
		<dd>': JustBetterCars.comAddress: 9'</dd>
	<dt>357.txt</dt>
		<dd>': www.maximumautosearch.comAdd'</dd>
	<dt>860.txt</dt>
		<dd>': www.emgauto.comAddress: 885 '</dd>
	<dt>1500.txt</dt>
		<dd>': www.seemocars.netAddress: 16'</dd>
</dl>



The websites are immediately followed by the word "Address"


```R
car_adverts["Website"] <- sapply(file_text, FUN = function(text) {
  value <- gsub("Website:\\s|Address:.*", "", str_extract(text, "Website:.*(Address:)"))
  if(all(is.na(value))) {NA}
  else{value[which(!is.na(value))]}
})

```

<h3>Company</h3>
Company information is also found in the same way that <strong>Exterior</strong> information is found.


```R
head(look.ahead("Offered by:", 30))
```


<dl class=dl-horizontal>
	<dt>1174.txt</dt>
		<dd>' Mount Eden Motor Inc ??? (718'</dd>
	<dt>458.txt</dt>
		<dd>' Colorado Auto Finders ??? (30'</dd>
	<dt>870.txt</dt>
		<dd>' Xtreme Auto Sales ??? (718) 5'</dd>
	<dt>1371.txt</dt>
		<dd>' Lexus of Concord ??? (925) 39'</dd>
	<dt>721.txt</dt>
		<dd>' Mount Eden Motor Inc ??? (718'</dd>
	<dt>609.txt</dt>
		<dd>' Reliable Automotive Group - D'</dd>
</dl>



Company information is immediately followed by "???"


```R
car_adverts["Company"] <- sapply(file_text, FUN = function(text) {
  value <- gsub("Offered by:\\s|(\\?{3}).*", "", str_extract(text, "Offered by:.*(\\?{3})"))
  if(all(is.na(value))) {NA}
  else{str_trim(value[which(!is.na(value))], side = "both")}
})
```

<h3>Address</h3>
The street address is captured by isolating the street name from the full address after unlisting the words.


```R
car_adverts["Street"] <- sapply(file_text, FUN = function(text) {
  value <- full.address(text)
  if(is.na(value)) {NA}
  else {
  # The following isolates the street from the whole address 
  value <- unlist.words(gsub(",.*", "", value))
  paste(value[-length(value)], sep = " ", collapse = " ")
  }
})
```

<h3>State</h3>
The state is captured by finding the two capital letters for the state from the full address after unlisting the words.


```R
car_adverts["State"] <- sapply(file_text, FUN = function(text) {
  value <- full.address(text)
  if(is.na(value)) {NA}
  else {
    value <- unlist.words(value)
    value[which(grepl("^[A-Z]{2}$",value))]
  }
})

```

<h3>Zip Code</h3>
The Zip Code is found by searching for a word from the text that is in zip code format. In order not to confuse it with the street number if both the Zip Code and street number are 5 digits, only the last captured value is taken.


```R
car_adverts["Zip Code"] <- sapply(file_text, FUN = function(text) {
  value <- full.address(text)
  if(is.na(value)) {NA}
  else {
    value <- unlist.words(value)
    value <- value[which(grepl("^\\d{5}(-\\d{4})?$",value))]
    # In case we capture the street address number, we return only last captured string
    if(length(value) > 1) {value[length(value)]}
    else {value}
  }
})

```

<h3>Make</h3>
The make of the car is found by geting the make and model after the year, then taking the first word.


```R
car_adverts["Make"] <- sapply(file_text, FUN = function(text) {
  value <- unlist.words(str_trim(gsub("(Certified )?[0-9]{4}\\s|[^ .0-9A-Za-z]", "",
                                      str_extract(text[[1]][1], "[0-9]{4}(.*)(\\s{2})*")), side = "both"))
  # To clean all-caps `
  # The first word captured is the Make
  str_to_title(value[1])
})
```

Some of the names need to be changed to be more consistent.


```R
car_adverts$Make[which(car_adverts$Make == str_to_title("BMW"))] <- "BMW"
car_adverts$Make[which(car_adverts$Make == str_to_title("GMC"))] <- "GMC"
# If you want to write the Mercedes-Benz as it should be
car_adverts$Make[which(car_adverts$Make == str_to_title("Mercedesbenz"))] <- "Mercedes-Benz"
```

<h3>Model</h3>
The Model of the car is found by getting the make and model after the year, then taking all but the first word. This will leave in any extra details mentioned by the seller as well.


```R
car_adverts["Model"] <- sapply(file_text, FUN = function(text) {
  value <- unlist.words(str_trim(gsub("(Certified )?[0-9]{4}\\s|[^ .0-9A-Za-z]", "",
                                      str_extract(text[[1]][1], "[0-9]{4}(.*)(\\s{2})*")), side = "both"))
  paste(value[-1], sep = " ", collapse = " ")
})
```

<h2>Saving the results</h3>


```R
write.csv(car_adverts, file = "car_adverts.csv", row.names = FALSE)
```


