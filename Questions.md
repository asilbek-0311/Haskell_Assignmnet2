# Question: 

You are given the following dataset from https://github.com/MoH-Malaysia/covid19-public. 

Please write a Haskell program to answer the following questions:


  #### 1. Which states in Malaysia has the highest deaths due to Covid ?
  #### 2. What is the average death per day for Malaysia in the provided data ?
  #### 3. Which of the five states are the lowest death in the provided data ?


The Program should display the answer for all the three questions above. 

---

# Details of CSV file

1. `date`: yyyy-mm-dd format; data correct as of 1200hrs on that date
2. `state`: name of state (present in state file, but not country file)
3. `deaths_new`: deaths due to COVID-19 based on date reported to public
4. `deaths_bid`: deaths due to COVID-19 which were brought-in dead based on date reported to public (perfect subset of `deaths_new`)
5. `deaths_new_dod`: deaths due to COVID-19 based on date of death
6. `deaths_bid_dod`: deaths due to COVID-19 which were brought-in dead based on date of death (perfect subset of `deaths_new_dod`)
7. `deaths_pvax`: number of partially-vaccinated individuals who died due to COVID-19 based on date of death (perfect subset of `deaths_new_dod`), where "partially vaccinated" is defined as receiving at least 1 dose of a 2-dose vaccine at least 1 day prior to testing positive, or receiving the Cansino vaccine between 1-27 days before testing positive.
8. `deaths_fvax`: number of fully-vaccinated who died due to COVID-19 based on date of death (perfect subset of deaths_new_dod), where "fully vaccinated" is defined as receiving the 2nd dose of a 2-dose vaccine at least 14 days prior to testing positive, or receiving the Cansino vaccine at least 28 days before testing positive.
9. `deaths_tat`: median days between date of death and date of report for all deaths reported on the day
