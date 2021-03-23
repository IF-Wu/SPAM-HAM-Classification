# Start write function readEmail.
all.files <- unlist(lapply(c("./SpamAssassinTrain/easy_ham", 
                             "./SpamAssassinTrain/easy_ham_2", 
                             "./SpamAssassinTrain/hard_ham", 
                             "./SpamAssassinTrain/spam", 
                             "./SpamAssassinTrain/spam_2"), 
                           function(x) list.files(x, full.names = TRUE)))

length(all.files)
#head(all.files)

# We will find whether the first line is start from "From xxx"
first_line <- sapply(all.files, readLines, 1)
count_from <- grepl("^From .* [0-9]{4}$", first_line)
table(count_from)
# Find out what they look like if it is not start from "From xxx"
#head(first_line[! count_from])

# We also want to discover whether all the remaining files are key:value pair.
count_pair <- grepl("^[A-Za-z][-A-Za-z]+:", first_line[! count_from])
table(count_pair)
# Find out the only one that is not an email
first_line[! count_from][! count_pair]
# So we will exclude this one.
all.files <- all.files[-which(first_line == first_line[! count_from][! count_pair])]
length(all.files)

# Function to process the header
process_header <- function(file){
  line = readLines(file)
  # We first identify the first blank line which divide header and body
  first_blank = which(line == "")[1]
  # Since we know that the first line of some emails are begin with "From xxx", which is not the "key:value" pairs, so we decide to ignore those lines which begin with "From xxx".
  first_line = line[1]
  if(grepl("^From .* [0-9]{4}$", first_line) == TRUE){
    header = line[2 : (first_blank - 1)]
  }
  else{
    header = line[1 : (first_blank - 1)]
  }
  # Since some email has "From xxx" in the header lines, we try to remove those
  if(is.na(grep("^From .* [0-9]{4}$", header)[1]) == TRUE){
    header = header
  }
  else{
    header = header[-which(grepl("^From .* [0-9]{4}$", header) == TRUE)]
  }
  # Using textconnection here
  header_key_value = read.dcf(textConnection(header), all = TRUE)
  # In order to find the boundary of the attachments, we need to know the content type of the email
  return(header_key_value)
}

# Function to process the boundary
process_boundary <- function(file, header){
  line = readLines(file)
  first_blank = which(line == "")[1]
  # The rest part will be the text after the first blank line
  rest = line[(first_blank + 1) : length(line)]
  # make sure content type field exist
  if("Content-Type" %in% colnames(header)){
    content_type = header$"Content-Type"
    if(grepl("boundary", content_type) == TRUE){
      # grab \" in the string
      if(grepl("\"", content_type)){
        split = strsplit(content_type, "boundary=")[[1]][2] # split by "boundary="
        # here we just want the boundary, remove "\"" and everything behind the boundary
        boundary = gsub("\"", "", split)
        boundary = gsub(";.*", "", boundary) # there exists some email have information behind the boundary, remove those.
      }
      else{
        # some boundary do not have double quation, we use "=" as an indicator to split
        boundary = strsplit(content_type, "=")[[1]][2]
      }
    }
    else{
      boundary = "No Boundary"
    }
  }
  # Same as the previous one
  else if("Content-type" %in% colnames(header)){
    content_type = header$"Content-type"
    if(grepl("boundary", content_type) == TRUE){
      if(grepl("\"", content_type)){
        split = strsplit(content_type, "boundary=")[[1]][2]
        boundary = gsub("\"", "", split)
        boundary = gsub(";.*", "", boundary)
      }
      else{
        boundary = strsplit(content_type, "=")[[1]][2]
      }
    }
    else{
      boundary = "No Boundary"
    }
  }
  # if do not have content type in the header, we should check the boundary in the text.
  else{
    if(is.na(grep("^[-]{2}.+[a-zA-Z0-9]+$", rest)[1]) == TRUE | ! "MIME-Version" %in% colnames(header)){
      boundary = "No Boundary"  # here we must ensure the email has MIME-version field in the header, and also the boundary must match certain pattern
    }
    else{
      # here we got our boundary, it is the starting line without the first "--"
      attachment_start = rest[grep("^[-]{2}.+[a-zA-Z0-9]+$", rest)[1]]
      boundary = gsub("^[-]{2}", "", attachment_start)
    }
  }
  return(boundary)
}

# Function to process the body
process_body <- function(file, header){
  line = readLines(file)
  first_blank = which(line == "")[1]
  rest = line[(first_blank + 1) : length(line)]
  boundary = process_boundary(file, header)
  if(boundary == "No Boundary"){
    body = rest # here we do not have any attachment, so the rest part is all body
  }
  else{
    # here we should find whether it has the ending boundary, if not, we use the end line of the email as the end of the attachment
    if(is.na(grep(paste0(paste0("--", boundary), "--"), rest, fixed = TRUE)[1]) == TRUE){
      if(is.na(grep(paste0("--", boundary), rest, fixed = TRUE)[1]) == TRUE){
        # here may exist some non-perfect matching boundaries, we use approximate grep to match those
        attachment_start_line = agrep(paste0("--", boundary), rest, fixed = TRUE)[1]
        attachment_end_line = length(rest)
        body = rest[-(attachment_start_line : attachment_end_line)]
      }
      else{
        # we should set fixed=TRUE to ensure some character won't influence the matching
        attachment_start_line = grep(paste0("--", boundary), rest, fixed = TRUE)[1]
        attachment_end_line = length(rest)
        body = rest[-(attachment_start_line : attachment_end_line)]
      }
    }
    else{ # here we have the ending boundary, the remaining steps are the same as previous one
      if(is.na(grep(paste0("--", boundary), rest, fixed = TRUE)[1]) == TRUE){
        attachment_start_line = agrep(paste0("--", boundary), rest, fixed = TRUE)[1]
        attachment_end_line = agrep(paste0(paste0("--", boundary), "--"), rest, fixed = TRUE)
        body = rest[-(attachment_start_line : attachment_end_line)]
      }
      else{
        attachment_start_line = grep(paste0("--", boundary), rest, fixed = TRUE)[1]
        attachment_end_line = grep(paste0(paste0("--", boundary), "--"), rest, fixed = TRUE)
        body = rest[-(attachment_start_line : attachment_end_line)]
      }
    }
  }
  return(body)
}

# Function to process the attachment
# Since we do not need to use the header or the body of the attachment separately, so we just put them together.
# The way to process the attachment is similar with that for the body
process_attachment <- function(file, header){
  line = readLines(file)
  first_blank = which(line == "")[1]
  rest = line[(first_blank + 1) : length(line)]
  boundary = process_boundary(file, header) # here we get the boundary
  if(boundary == "No Boundary"){
    attachment = list(NULL) # if there is no attachment, we should return a NULL list
  }
  else{
    # this is the situation that do not have the ending boundary
    if(is.na(grep(paste0(paste0("--", boundary), "--"), rest, fixed = TRUE)[1]) == TRUE){
      if(is.na(grep(paste0("--", boundary), rest, fixed = TRUE)[1]) == TRUE){
        attachment_start_line = agrep(paste0("--", boundary), rest, fixed = TRUE)[1]
        attachment_end_line = length(rest)
        whole_attachment = rest[attachment_start_line : attachment_end_line]
        attachment_indicator = c(agrep(paste0("--", boundary), whole_attachment, fixed = TRUE), (length(whole_attachment) + 1))
        attachment = list() # we starting with an empty list
        if(length(attachment_indicator) == 2){ # if there is only one attachment
          one_attachment = whole_attachment[2 : length(whole_attachment)]
          attachment[[1]] = one_attachment # the attachment will be everything between starting and ending boundary.
        }
        else{ # here we have two or more attachments
          for(i in 1 : (length(attachment_indicator) - 1)){ # use for loop to extract each attachment out
            one_attachment = whole_attachment[(attachment_indicator[i] + 1) : (attachment_indicator[i + 1] - 1)]
            attachment[[i]] = one_attachment # attachment will be the text between two starting boundary, the last one will be the text between the starting and ending boundary.
          }
        }
      }
      else{ # here is situation we have the ending boundary, the approach is the same.
        attachment_start_line = grep(paste0("--", boundary), rest, fixed = TRUE)[1]
        attachment_end_line = length(rest)
        whole_attachment = rest[attachment_start_line : attachment_end_line]
        attachment_indicator = c(grep(paste0("--", boundary), whole_attachment, fixed = TRUE), (length(whole_attachment) + 1))
        attachment = list()
        if(length(attachment_indicator) == 2){
          one_attachment = whole_attachment[2 : length(whole_attachment)]
          attachment[[1]] = one_attachment
        }
        else{
          for(i in 1 : (length(attachment_indicator) - 1)){
            one_attachment = whole_attachment[(attachment_indicator[i] + 1) : (attachment_indicator[i + 1] - 1)]
            attachment[[i]] = one_attachment
          }
        }
      }
    }
    else{ # here is the case we do not need to approximate agrep, the remaining steps are the same with previous one
      if(is.na(grep(paste0("--", boundary), rest, fixed = TRUE)[1]) == TRUE){
        attachment_start_line = agrep(paste0("--", boundary), rest, fixed = TRUE)[1]
        attachment_end_line = agrep(paste0(paste0("--", boundary), "--"), rest, fixed = TRUE)
        whole_attachment = rest[attachment_start_line : attachment_end_line]
        attachment_indicator = agrep(paste0("--", boundary), whole_attachment, fixed = TRUE)
        attachment = list()
        if(length(attachment_indicator) == 2){
          one_attachment = whole_attachment[2 : (length(whole_attachment) - 1)]
          attachment[[1]] = one_attachment
        }
        else{
          for(i in 1 : (length(attachment_indicator) - 1)){
            one_attachment = whole_attachment[(attachment_indicator[i] + 1) : (attachment_indicator[i + 1] - 1)]
            attachment[[i]] = one_attachment
          }
        }
      }
      else{
        attachment_start_line = grep(paste0("--", boundary), rest, fixed = TRUE)[1]
        attachment_end_line = grep(paste0(paste0("--", boundary), "--"), rest, fixed = TRUE)
        whole_attachment = rest[attachment_start_line : attachment_end_line]
        attachment_indicator = grep(paste0("--", boundary), whole_attachment, fixed = TRUE)
        attachment = list()
        if(length(attachment_indicator) == 2){
          one_attachment = whole_attachment[2 : (length(whole_attachment) - 1)]
          attachment[[1]] = one_attachment
        }
        else{
          for(i in 1 : (length(attachment_indicator) - 1)){
            one_attachment = whole_attachment[(attachment_indicator[i] + 1) : (attachment_indicator[i + 1] - 1)]
            attachment[[i]] = one_attachment
          }
        }
      }
    }
  }
  return(attachment) # In processing the attachment, except the difference between the situation, the approach we use for each case is the same.
}

# build a function to combine three function above, we want to return a large list, and each email is an element
readEmail <- function(file){
  hdr = process_header(file)
  list("header" = hdr, 
       "body" = process_body(file, hdr), 
       "attachment" = process_attachment(file, hdr))
}

all.emails <- lapply(all.files, readEmail) # use lapply to deal with all emails.

# Start creating an R data frame of "derived" variables that give various measures of the email messages

# 1. isSpam
explore_email <- as.data.frame(grepl("spam", all.files[], fixed = TRUE)) # get the name of the folder
colnames(explore_email) <- "isSpam"

# 2. isRe
all.header <- sapply(all.emails, function(x) x$header)
for(i in 1 : length(all.header)){
  if("Subject" %in% colnames(all.header[[i]])){ # make sure there is the subject field
    if(grepl("Re:", all.header[[i]]$Subject, ignore.case = TRUE) == TRUE){ # here we garb the "Re:" string.
      explore_email[i, 2] = TRUE
    }
    else{
      explore_email[i, 2] = FALSE
    }
  }
  else{
    explore_email[i, 2] = FALSE
  }
}
colnames(explore_email)[2] <- "isRe"

# 3. numLinesInBody
all.body <- sapply(all.emails, function(x) x$body)
for(i in 1 : length(all.body)){ 
  explore_email[i, 3] = length(all.body[[i]]) # since there are many lines of the body, we use length function to calculate
}
colnames(explore_email)[3] <- "numLinesInBody"

# 4. bodyCharacterCount
for(i in 1 : length(all.body)){
  explore_email[i, 4] = sum(nchar(all.body[[i]])) # use nchar to calculate the character in a line and sum them up.
}
colnames(explore_email)[4] <- "bodyCharacterCount"

# 5. isYelling
for(i in 1 : length(all.header)){
  if("Subject" %in% colnames(all.header[[i]])){
    if((grepl("[a-z]", all.header[[i]]$Subject) == TRUE)){ # if there is any lower case letter exist in the subject, it will return FALSE
      explore_email[i, 5] = FALSE
    }
    else{
      explore_email[i, 5] = TRUE
    }
  }
  else{
    explore_email[i, 5] = FALSE
  }
}
colnames(explore_email)[5] <- "isYelling"

# 6. numDollarSigns
for(i in 1 : length(all.body)){
  body = paste(all.body[[i]], collapse = " ") # combine the body into one large string (several lines to one line)
  find_dollar = gregexpr("\\$", body) # count the number of "$"
  if(find_dollar[[1]][1] == -1){ # no dollar sign in the body
    explore_email[i, 6] = 0
  }
  else{
    explore_email[i, 6] = length(find_dollar[[1]])
  }
}
colnames(explore_email)[6] <- "numDollarSigns"

# 7. numAttachments
all.attachment <- sapply(all.emails, function(x) x$attachment)
for(i in 1 : length(all.attachment)){
  if(is.null(all.attachment[[i]][[1]]) == TRUE){ # do not calculate the NULL attachment
    explore_email[i, 7] = 0
  }
  else{
    explore_email[i, 7] = length(all.attachment[[i]]) # use length to calculate the number of attachment.
  }
}
colnames(explore_email)[7] <- "numAttachments"

# 8. subjectExclamationCount
for(i in 1 : length(all.header)){
  if("Subject" %in% colnames(all.header[[i]])){
    subject = all.header[[i]]$Subject # extract the value of the subject
    find_exclamation = gregexpr("\\!", subject) # count the number of "!"
    if(find_exclamation[[1]][1] == -1){ # no "!" in the subject
      explore_email[i, 8] = 0
    }
    else{
      explore_email[i, 8] = length(find_exclamation[[1]]) # return total number
    }
  }
  else{
    explore_email[i, 8] = 0
  }
}
colnames(explore_email)[8] <- "subjectExclamationCount"

# 9. isDear
# We only check that the "Dear" appears at the first place of a line, which could be an introduction dear.
for(i in 1 : length(all.body)){
  if(is.na(grep("^Dear", all.body[[i]])[1]) == TRUE){ # check if a line is start with Dear.
    explore_email[i, 9] = FALSE
  }
  else{
    explore_email[i, 9] = TRUE
  }
}
colnames(explore_email)[9] <- "isDear"

# 10. subjectSpamWords
for(i in 1 : length(all.header)){
  if("Subject" %in% colnames(all.header[[i]])){
    subject = all.header[[i]]$Subject
    if(grepl("viagra", subject) == TRUE){ # From here we check each spam phrases.
      explore_email[i, 10] = TRUE
    }
    else if(grepl("pounds", subject) == TRUE){
      explore_email[i, 10] = TRUE
    }
    else if(grepl("free", subject) == TRUE){
      explore_email[i, 10] = TRUE
    }
    else if(grepl("weight", subject) == TRUE){
      explore_email[i, 10] = TRUE
    }
    else if(grepl("guarantee", subject) == TRUE){
      explore_email[i, 10] = TRUE
    }
    else if(grepl("millions", subject) == TRUE){
      explore_email[i, 10] = TRUE
    }
    else if(grepl("dollars", subject) == TRUE){
      explore_email[i, 10] = TRUE
    }
    else if(grepl("credit", subject) == TRUE){
      explore_email[i, 10] = TRUE
    }
    else if(grepl("risk", subject) == TRUE){
      explore_email[i, 10] = TRUE
    }
    else if(grepl("prescription", subject) == TRUE){
      explore_email[i, 10] = TRUE
    }
    else if(grepl("generic", subject) == TRUE){
      explore_email[i, 10] = TRUE
    }
    else if(grepl("drug", subject) == TRUE){
      explore_email[i, 10] = TRUE
    }
    else if(grepl("money back", subject) == TRUE){
      explore_email[i, 10] = TRUE
    }
    else if(grepl("credit card", subject) == TRUE){
      explore_email[i, 10] = TRUE
    }
    else{
      explore_email[i, 10] = FALSE
    }
  }
  else{
    explore_email[i, 10] = FALSE
  }
}
colnames(explore_email)[10] <- "subjectSpamWords"

# 11. subjectQuestCount
for(i in 1 : length(all.header)){
  if("Subject" %in% colnames(all.header[[i]])){
    subject = all.header[[i]]$Subject
    find_question = gregexpr("\\?", subject) # count the number of "?" in the subject
    if(find_question[[1]][1] == -1){ # no "?" in the subject
      explore_email[i, 11] = 0
    }
    else{
      explore_email[i, 11] = length(find_question[[1]]) # sum up
    }
  }
  else{
    explore_email[i, 11] = 0
  }
}
colnames(explore_email)[11] <- "subjectQuestCount"

# 12. multipartText
for(i in 1 : length(all.header)){
  if("Content-Type" %in% colnames(all.header[[i]])){ # make sure there is the content type in the header
    content_type = all.header[[i]]$"Content-Type"
    if(grepl("multipart/", content_type) == TRUE){ # check if there is "multipart/" in the content type
      explore_email[i, 12] = TRUE
    }
    else{
      boundary = process_boundary(all.files[i], process_header(all.files[i]))
      if(boundary == "No Boundary"){ # no boundary means the header does not state there are attachments
        explore_email[i, 12] = FALSE
      }
      else{
        if(length(all.attachment[[i]]) >= 2){ # check those email has two or more attachment
          explore_email[i, 12] = TRUE
        }
        else{
          explore_email[i, 12] = FALSE
        }
      }
    }
  }
  else if("Content-type" %in% colnames(all.header[[i]])){ # another type of "content type" field in the header
    content_type = all.header[[i]]$"Content-type"
    if(grepl("multipart/", content_type) == TRUE){
      explore_email[i, 12] = TRUE
    }
    else{
      boundary = process_boundary(all.files[i], process_header(all.files[i])) # same as above
      if(boundary == "No Boundary"){
        explore_email[i, 12] = FALSE
      }
      else{
        if(length(all.attachment[[i]]) >= 2){
          explore_email[i, 12] = TRUE
        }
        else{
          explore_email[i, 12] = FALSE
        }
      }
    }
  }
  else{
    explore_email[i, 12] = FALSE
  }
}
colnames(explore_email)[12] <- "multipartText"

# 13. isInReplyTo
for(i in 1 : length(all.header)){
  if("In-Reply-To" %in% colnames(all.header[[i]])){ # check whether there is "In-Reply-To" in the header
    explore_email[i, 13] = TRUE
  }
  else{
    explore_email[i, 13] = FALSE
  }
}
colnames(explore_email)[13] <- "isInReplyTo"

# 14. priority
# Figure out the level of priority
p_level = c()
for(i in 1 : length(all.header)){
  if("X-Priority" %in% colnames(all.header[[i]])){ 
    p_level[i] = all.header[[i]]$"X-Priority" # extract the value of X-Priority
  }
}
table(p_level) # check the number of each level

# We find that there is no "X-Smell-Priority" in the header
count = 0
for(i in 1 : length(all.header)){
  if(! "X-Smell-Priority" %in% colnames(all.header[[i]])){
    count = count + 1
  }
}
print(count)

# Derive the variable. We find that there are three numbers that indicates the priority level. In this case we just set both 1 & 2 as high,and set 3 as normal, for those emails that do not have priority level, we set those as unknown.
for(i in 1 : length(all.header)){
  if("X-Priority" %in% colnames(all.header[[i]])){
    priority = all.header[[i]]$"X-Priority"
    if(grepl("3", priority) == TRUE){ # find number 3 in the X-Priority
      explore_email[i, 14] = "Normal"
    }
    else{
      explore_email[i, 14] = "High" # number 1 and 2 set to high
    }
  }
  else{
    explore_email[i, 14] = "Unknown" # other email messages that do not have the field are all set to unknown
  }
}
colnames(explore_email)[14] <- "priority"

# 15. numRecipients
for(i in 1 : length(all.header)){
  if("To" %in% colnames(all.header[[i]])){
    to = all.header[[i]]$To
    find_recipient_to = gregexpr("\\@", to) # we use "@" as an indicator for a recipient
    if(find_recipient_to[[1]][1] == -1){ # no "@"
      count_to = 0
    }
    else{
      count_to = length(find_recipient_to[[1]])
    }
    if("Cc" %in% colnames(all.header[[i]])){ # here we check whether we have Cc if we already have To
      cc = all.header[[i]]$Cc
      find_recipient_cc = gregexpr("\\@", cc) # check the number of "@"
      if(find_recipient_cc[[1]][1] == -1){
        explore_email[i, 15] = count_to
      }
      else{
        explore_email[i, 15] = count_to + length(find_recipient_cc[[1]]) # sum up two values
      }
    }
    else{
      explore_email[i, 15] = count_to
    }
  }
  else if("Cc" %in% colnames(all.header[[i]])){ # If there is only a Cc in the header (no To)
    cc = all.header[[i]]$Cc
    find_recipient_cc = gregexpr("\\@", cc)
    if(find_recipient_cc[[1]][1] == -1){
      explore_email[i, 15] = 0
    }
    else{
      explore_email[i, 15] = length(find_recipient_cc[[1]])
    }
  }
  else{
    explore_email[i, 15] = 0
  }
}
colnames(explore_email)[15] <- "numRecipients"

# 16. isOriginalMessage
for(i in 1 : length(all.body)){
  body = paste(all.body[[i]], collapse = " ")
  if(grepl("original message text", body, ignore.case = TRUE) == TRUE){ # check two strings, we ignore the upper and lower case by setting ignore.case=TRUE
    explore_email[i, 16] = TRUE
  }
  else if(grepl("Original Message", body, ignore.case = TRUE) == TRUE){ # another string type
    explore_email[i, 16] = TRUE
  }
  else{
    explore_email[i, 16] = FALSE
  }
}
colnames(explore_email)[16] <- "isOriginalMessage"

# 17. fromNumericEnd
for(i in 1 : length(all.header)){
  if("From" %in% colnames(all.header[[i]])){
    if(grepl("[0-9]+@", all.header[[i]]$From) == TRUE){ # check whether the last character before "@" is a number
      explore_email[i, 17] = TRUE
    }
    else{
      explore_email[i, 17] = FALSE
    }
  }
  else{
    explore_email[i, 17] = FALSE
  }
}
colnames(explore_email)[17] <- "fromNumericEnd"

# 18. isPGPsigned
for(i in 1 : length(all.body)){
  body = paste(all.body[[i]], collapse = " ") # multiple lines to one line.
  if(grepl("PGP", body) == TRUE){ # since the "PGP" is an uncommon word, we just search it in the body
    explore_email[i, 18] = TRUE
  }
  else if(grepl("GPG", body) == TRUE){ # also search "GPG" in the body
    explore_email[i, 18] = TRUE
  }
  else if(is.na(grep("PGP", all.attachment[[i]])[1]) == FALSE){ # we also check "PGP" in the attachment
    explore_email[i, 18] = TRUE
  }
  else if(is.na(grep("GPG", all.attachment[[i]])[1]) == FALSE){ # check "GPG" in the attachment
    explore_email[i, 18] = TRUE
  }
  else{
    explore_email[i, 18] = FALSE
  }
}
colnames(explore_email)[18] <- "isPGPsigned"

# 19. percentSubjectBlanks
for(i in 1 : length(all.header)){
  if("Subject" %in% colnames(all.header[[i]])){
    subject = all.header[[i]]$Subject
    char_count = nchar(subject) # calculate the total number of character in the subject
    if(gregexpr(" ", subject)[[1]][1] == -1){ # count the number of blank character in the subject
      explore_email[i, 19] = 0
    }
    else{
      explore_email[i, 19] = length(gregexpr(" ", subject)[[1]]) / char_count # percentage = #of blank/total character
    }
  }
  else{
    explore_email[i, 19] = 0
  }
}
colnames(explore_email)[19] <- "percentSubjectBlanks"

# 20. subjectPunctuationCheck
for(i in 1 : length(all.header)){
  if("Subject" %in% colnames(all.header[[i]])){
    subject = all.header[[i]]$Subject
    if(grepl("[a-zA-Z]+[[:punct:]]+[a-zA-Z]+", subject) == TRUE){ # check whether there is a punctuation between characters
      explore_email[i, 20] = TRUE
    }
    else if(grepl("[a-zA-Z]+[0-9]+[a-zA-Z]+", subject) == TRUE){ # check whether there is a number between characters
      explore_email[i, 20] = TRUE
    }
    else{
      explore_email[i, 20] = FALSE
    }
  }
  else{
    explore_email[i, 20] = FALSE
  }
}
colnames(explore_email)[20] <- "subjectPunctuationCheck"

# 21. replyUnderline
for(i in 1 : length(all.header)){
  if("Reply-To" %in% colnames(all.header[[i]])){ # check whether there is the "Reply-To" field
    reply_to = all.header[[i]]$"Reply-To" 
    if(grepl("_", reply_to) == TRUE){ # check whether there is the underscore "_" in the field
      explore_email[i, 21] = TRUE
    }
    else{
      explore_email[i, 21] = FALSE
    }
  }
  else{
    explore_email[i, 21] = FALSE
  }
}
colnames(explore_email)[21] <- "replyUnderline"

# 22. hourSent
for(i in 1 : length(all.header)){
  if("Date" %in% colnames(all.header[[i]])){
    date = all.header[[i]]$Date # since the format is not the same, we use str_extract in the stringr package to extract the time
    time = stringr::str_extract(date, "[0-9]+:+") # extract the time "xx:..."
    explore_email[i, 22] = strsplit(time, ":")[[1]][1] # divide by ":", the first one is the hour.
  }
  else{
    explore_email[i, 22] = 99
  }
}
colnames(explore_email)[22] <- "hourSent"
explore_email$hourSent <- as.integer(explore_email$hourSent)

# Start explore the derived variables
str(explore_email)

table(explore_email$isSpam)

# We first see plots of those logical variables, we use relative frequency since the number of spam and ham are different
# using prop.table to calculate the relative frequency.
library(ggplot2)

# Relation between isSpam and isRe
plot_isRe <- ggplot(explore_email, aes(x = isSpam, fill = isRe)) + 
  geom_bar(position = "fill") + 
  theme_classic() + 
  xlab("Email Type") + 
  scale_x_discrete(labels = c("Ham", "Spam")) +
  ylab("Frequency") + 
  ggtitle("Relation between isSpam and isRe") 

plot_isRe

prop.table(table(explore_email$isSpam, explore_email$isRe, dnn = c("isSpam", "isRe")), margin = 1)

# Relation between isSpam and isYelling
plot_isYelling <- ggplot(explore_email, aes(x = isSpam, fill = isYelling)) + 
  geom_bar(position = "fill") + 
  theme_classic() + 
  xlab("Email Type") + 
  scale_x_discrete(labels = c("Ham", "Spam")) +
  ylab("Frequency") + 
  ggtitle("Relation between isSpam and isYelling") 

plot_isYelling

prop.table(table(explore_email$isSpam, explore_email$isYelling, dnn = c("isSpam", "isYelling")), margin = 1)

# Relation between isSpam and isDear
plot_isDear <- ggplot(explore_email, aes(x = isSpam, fill = isDear)) + 
  geom_bar(position = "fill") + 
  theme_classic() +
  xlab("Email Type") + 
  scale_x_discrete(labels = c("Ham", "Spam")) +
  ylab("Frequency") + 
  ggtitle("Relation between isSpam and isDear") 

plot_isDear

prop.table(table(explore_email$isSpam, explore_email$isDear, dnn = c("isSpam", "isDear")), margin = 1)

# Relation between isSpam and subjectSpamWords
plot_subjectSpamWords <- ggplot(explore_email, aes(x = isSpam, fill = subjectSpamWords)) + 
  geom_bar(position = "fill") + 
  theme_classic() + 
  xlab("Email Type") + 
  scale_x_discrete(labels = c("Ham", "Spam")) +
  ylab("Frequency") + 
  ggtitle("Relation between isSpam and subjectSpamWords") 

plot_subjectSpamWords

prop.table(table(explore_email$isSpam, explore_email$subjectSpamWords, dnn = c("isSpam", "subjectSpamWords")), margin = 1)

# Relation between isSpam and multipartText
plot_multipartText <- ggplot(explore_email, aes(x = isSpam, fill = multipartText)) + 
  geom_bar(position = "fill") + 
  theme_classic() + 
  xlab("Email Type") + 
  scale_x_discrete(labels = c("Ham", "Spam")) +
  ylab("Frequency") + 
  ggtitle("Relation between isSpam and multipartText") 

plot_multipartText

prop.table(table(explore_email$isSpam, explore_email$multipartText, dnn = c("isSpam", "multipartText")), margin = 1)

# Relation between isSpam and isInReplyTo
plot_isInReplyTo <- ggplot(explore_email, aes(x = isSpam, fill = isInReplyTo)) + 
  geom_bar(position = "fill") + 
  theme_classic() + 
  xlab("Email Type") + 
  scale_x_discrete(labels = c("Ham", "Spam")) +
  ylab("Frequency") + 
  ggtitle("Relation between isSpam and isInReplyTo") 

plot_isInReplyTo

prop.table(table(explore_email$isSpam, explore_email$isInReplyTo, dnn = c("isSpam", "isInReplyTo")), margin = 1)

# Relation between isSpam and isOriginalMessage
plot_isOriginalMessage <- ggplot(explore_email, aes(x = isSpam, fill = isOriginalMessage)) + 
  geom_bar(position = "fill") + 
  theme_classic() + 
  xlab("Email Type") + 
  scale_x_discrete(labels = c("Ham", "Spam")) +
  ylab("Frequency") + 
  ggtitle("Relation between isSpam and isOriginalMessage") 

plot_isOriginalMessage

prop.table(table(explore_email$isSpam, explore_email$isOriginalMessage, dnn = c("isSpam", "isOriginalMessage")), margin = 1)

# Relation between isSpam and fromNumericEnd
plot_fromNumericEnd <- ggplot(explore_email, aes(x = isSpam, fill = fromNumericEnd)) + 
  geom_bar(position = "fill") + 
  theme_classic() + 
  xlab("Email Type") + 
  scale_x_discrete(labels = c("Ham", "Spam")) +
  ylab("Frequency") + 
  ggtitle("Relation between isSpam and fromNumericEnd") 

plot_fromNumericEnd

prop.table(table(explore_email$isSpam, explore_email$fromNumericEnd, dnn = c("isSpam", "fromNumericEnd")), margin = 1)

# Relation between isSpam and isPGPsigned
plot_isPGPsigned <- ggplot(explore_email, aes(x = isSpam, fill = isPGPsigned)) + 
  geom_bar(position = "fill") + 
  theme_classic() + 
  xlab("Email Type") + 
  scale_x_discrete(labels = c("Ham", "Spam")) +
  ylab("Frequency") + 
  ggtitle("Relation between isSpam and isPGPsigned") 

plot_isPGPsigned

prop.table(table(explore_email$isSpam, explore_email$isPGPsigned, dnn = c("isSpam", "isPGPsigned")), margin = 1)

# Relation between isSpam and subjectPunctuationCheck
plot_subjectPunctuationCheck <- ggplot(explore_email, aes(x = isSpam, fill = subjectPunctuationCheck)) + 
  geom_bar(position = "fill") + 
  theme_classic() + 
  xlab("Email Type") + 
  scale_x_discrete(labels = c("Ham", "Spam")) +
  ylab("Frequency") + 
  ggtitle("Relation between isSpam and subjectPunctuationCheck") 

plot_subjectPunctuationCheck

prop.table(table(explore_email$isSpam, explore_email$subjectPunctuationCheck, dnn = c("isSpam", "subjectPunctuationCheck")), margin = 1)

# Relation between isSpam and replyUnderline
plot_replyUnderline <- ggplot(explore_email, aes(x = isSpam, fill = replyUnderline)) + 
  geom_bar(position = "fill") + 
  theme_classic() + 
  xlab("Email Type") + 
  scale_x_discrete(labels = c("Ham", "Spam")) +
  ylab("Frequency") + 
  ggtitle("Relation between isSpam and replyUnderline")

plot_replyUnderline

prop.table(table(explore_email$isSpam, explore_email$replyUnderline, dnn = c("isSpam", "replyUnderline")), margin = 1)

# We also use the same method to explore the categorical variable
# Relation between isSpam and priority
plot_priority <- ggplot(explore_email, aes(x = isSpam, fill = priority)) + 
  geom_bar(position = "fill") + 
  theme_classic() + 
  xlab("Email Type") + 
  scale_x_discrete(labels = c("Ham", "Spam")) +
  ylab("Frequency") + 
  ggtitle("Relation between isSpam and priority")

plot_priority

prop.table(table(explore_email$isSpam, explore_email$priority, dnn = c("isSpam", "priority")), margin = 1)

# We then use density plot to see those numeric variables
# Relation between isSpam and numLinesInBody
plot_numLinesInBody <- ggplot(explore_email, aes(x = numLinesInBody, fill = isSpam)) + 
  geom_histogram(binwidth = 10, position = "dodge") + 
  scale_fill_discrete(name="Email Type", labels = c("Ham", "Spam")) + 
  xlim(0, 1000) + # we check 0-1000
  theme_classic() + 
  ggtitle("Relation between isSpam and numLinesInBody")

plot_numLinesInBody

# Relation between isSpam and bodyCharacterCount
plot_bodyCharacterCount <- ggplot(explore_email, aes(x = bodyCharacterCount, fill = isSpam)) + 
  geom_histogram(binwidth = 50, position = "dodge") + 
  scale_fill_discrete(name="Email Type", labels = c("Ham", "Spam")) + 
  xlim(0, 10000) + # we check 0-10000
  theme_classic() + 
  ggtitle("Relation between isSpam and bodyCharacterCount")

plot_bodyCharacterCount

# Relation between isSpam and numDollarSigns
plot_numDollarSigns <- ggplot(explore_email, aes(x = numDollarSigns, fill = isSpam)) + 
  geom_histogram(binwidth = 1.5, position = "dodge") + 
  scale_fill_discrete(name="Email Type", labels = c("Ham", "Spam")) + 
  xlim(0.5, 75) + # we check 1-75
  theme_classic() + 
  ggtitle("Relation between isSpam and numDollarSigns")

plot_numDollarSigns

# Relation between isSpam and numAttachments
plot_numAttachments <- ggplot(explore_email, aes(x = numAttachments, fill = isSpam)) + 
  geom_histogram(binwidth = 1, position = "dodge") + 
  scale_fill_discrete(name="Email Type", labels = c("Ham", "Spam")) + 
  xlim(0.5, 6.5) + # we check 1-6
  theme_classic() + 
  ggtitle("Relation between isSpam and numAttachments")

plot_numAttachments

# Relation between isSpam and subjectExclamationCount
plot_subjectExclamationCount <- ggplot(explore_email, aes(x = subjectExclamationCount, fill = isSpam)) +
  geom_histogram(binwidth = 1, position = "dodge") + 
  scale_fill_discrete(name="Email Type", labels = c("Ham", "Spam")) + 
  xlim(1.5, 10) + # we check 2-10
  theme_classic() + 
  ggtitle("Relation between isSpam and subjectExclamationCount")

plot_subjectExclamationCount

# Relation between isSpam and subjectQuestCount
plot_subjectQuestCount <- ggplot(explore_email, aes(x = subjectQuestCount, fill = isSpam)) +
  geom_histogram(binwidth = 1, position = "dodge") + 
  scale_fill_discrete(name="Email Type", labels = c("Ham", "Spam")) + 
  xlim(1.5, 8) + # we check 2-8
  theme_classic() + 
  ggtitle("Relation between isSpam and subjectQuestCount")

plot_subjectQuestCount

# Relation between isSpam and numRecipients
plot_numRecipients <- ggplot(explore_email, aes(x = numRecipients, fill = isSpam)) +
  geom_histogram(binwidth = 1, position = "dodge") + 
  scale_fill_discrete(name="Email Type", labels = c("Ham", "Spam")) + 
  xlim(5.5, 100) + # we check 5-100
  theme_classic() + 
  ggtitle("Relation between isSpam and numRecipients")

plot_numRecipients

# Relation between isSpam and hourSent
plot_hourSent <- ggplot(explore_email, aes(x = hourSent, fill = isSpam)) + 
  geom_histogram(binwidth = 1, position = "dodge") + 
  scale_fill_discrete(name="Email Type", labels = c("Ham", "Spam")) + 
  xlim(-1, 24) + # we check 0-23
  theme_classic() + 
  ggtitle("Relation between isSpam and hourSent")

plot_hourSent

# Relation between isSpam and percentSubjectBlanks
plot_percentSubjectBlanks <- ggplot(explore_email, aes(x = percentSubjectBlanks, fill = isSpam)) +
  geom_histogram(binwidth = 0.01, position = "dodge") +
  scale_fill_discrete(name="Email Type", labels = c("Ham", "Spam")) + 
  xlim(0.2, 1) + # we check 0-1
  theme_classic() + 
  ggtitle("Relation between isSpam and percentSubjectBlanks")

plot_percentSubjectBlanks




