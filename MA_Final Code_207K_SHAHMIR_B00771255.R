# Load the package
library(RODBC)

# Connect to MySQL (my credentials are mysql_server_64/root/.)
db = odbcConnect("Shahmir", uid="root", pwd="shahmir93")
sqlQuery(db, "USE ma_charity_full")

#TRAINING SET
query1 = "SELECT *
														   		
FROM assignment2 as a2 

        LEFT JOIN (
        SELECT a.contact_id as 'contact_id',
                DATEDIFF(20180626, MAX(a.act_date)) / 365 AS 'recency',
                COUNT(a.amount) AS 'frequency',
                AVG(a.amount) AS 'avgamount',
                MAX(a.amount) AS 'maxamount',
                DATEDIFF(20180626, MIN(a.act_date)) / 365 AS 'firstdonation',
                IF(COUNT(amount) IS NULL,0,1) AS 'EverDonated'
		    FROM acts as a						
																 
         GROUP BY 1
         ) as nf
		ON a2.contact_id = nf.contact_id
                                                                             
		WHERE (calibration = '1')" 

traindata = sqlQuery(db, query1)

print(head(traindata))


#TEST DATA
query2 = "SELECT *
														   		
FROM assignment2 as a2 

        LEFT JOIN (
        SELECT a.contact_id as 'contact_id',
                DATEDIFF(20180626, MAX(a.act_date)) / 365 AS 'recency',
                COUNT(a.amount) AS 'frequency',
                AVG(a.amount) AS 'avgamount',
                MAX(a.amount) AS 'maxamount',
                DATEDIFF(20180626, MIN(a.act_date)) / 365 AS 'firstdonation',
                IF(COUNT(amount) IS NULL,0,1) AS 'EverDonated'
		    FROM acts as a						
																 
         GROUP BY 1
         ) as nf
		ON a2.contact_id = nf.contact_id
                                                                             
		WHERE (calibration = '0')" 


testdata = sqlQuery(db, query2)

#testdata[is.na(testdata)] = 0
#replace(testdata, is.na(testdata), 0)

print(head(testdata))


# One of the libraries available for (multinomial) logit model
library(nnet)


#MULTI ON AMOUNT
library(nnet)

multinom_prob_log = multinom(formula = donation ~ (recency * frequency) + log(recency) + log(frequency),
                             data = traindata)


#In-sample, donation amount model
# Note that the amount model only applies to a subset of donors...
z = which(!is.na(traindata$amount))
print(head(traindata[z, ]))
multinom_amount_log = lm(formula = log(amount) ~ log(avgamount) + log(maxamount),
                         data = traindata[z, ])












# Out-of-sample predictions
# Do NOT forget to re-transform "log(amount)" into "amount"
out = data.frame(contact_id = testdata$contact_id)
out$probs  = predict(object = multinom_prob_log, newdata = testdata, type = "probs")
out$amount = exp(predict(object = multinom_amount_log, newdata = testdata))
out$score  = out$probs * out$amount 

# Show results
print(head(out))

# Who is likely to be worth more than 2 EUR?
z = which(out$score > 2)
print(length(z))

out$reach = ifelse(out$score>2, 1, 0)
result = data.frame(out[, c("contact_id", "reach")])
write.table(data.frame(result), file = "/Users/shahmir/Documents/MA_Submission1.txt",
            sep = "\t", col.names = FALSE, row.names = FALSE)




#final = out %>% filter(out$score > 2) %>% mutate(result = 1) %>% select(contact_id,result)
#file1 = testdata %>% select(contact_id) %>% mutate(result = 0)
#export = left_join(file1,final,by = "contact_id")
#export[is.na(export)] = 0
#ans = export %>% select(contact_id, result.y)
#write.table(ans,"",sep "\t", col.names = F, row.names = FALSE)