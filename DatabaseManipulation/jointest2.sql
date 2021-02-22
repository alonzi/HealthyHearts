Create Table BIG_TABLE AS
(SELECT *
FROM abgs2
FULL JOIN cbc2
USING ("DONOR_ID", "DT", "BrainDeath", "Accepted", "DON_ID", "TRR_ID_CODE","DOB_DON","DOB"));

Create Table BIG_TABLE2 AS
(SELECT *
FROM big_table
FULL JOIN indicators2
USING ("DONOR_ID", "DT", "BrainDeath", "Accepted", "DON_ID", "TRR_ID_CODE","DOB_DON","DOB"));

Create Table BIG_TABLE3 AS
(SELECT *
FROM big_table2
FULL JOIN inotropicscore2
USING ("DONOR_ID", "DT", "BrainDeath", "Accepted", "DON_ID", "TRR_ID_CODE","DOB_DON","DOB"));

Create Table BIG_TABLE4 AS
(SELECT *
FROM big_table3
FULL JOIN labpanels2
USING ("DONOR_ID", "DT", "BrainDeath", "Accepted", "DON_ID", "TRR_ID_CODE","DOB_DON","DOB"));
	   
Create Table BIG_TABLE5 AS
(SELECT *
FROM big_table4
FULL JOIN labvalues2
USING ("DONOR_ID", "DT", "BrainDeath", "Accepted", "DON_ID", "TRR_ID_CODE","DOB_DON","DOB"));   
	   
Create Table FINAL_TABLE AS
(SELECT *
FROM big_table5
FULL JOIN echo2
USING ("DON_ID", "DT")); 