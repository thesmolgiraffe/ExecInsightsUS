# PayDynamicsUS
Statistical Programming Final Project • Exploratory analysis on relationship between executive pay and company performance on US-listed firms using R programming.

## Project Statement
Much has been written about the relationship between CEO pay and company performance.  Shareholders and other stakeholders expect pay and performance to align for executives, in general, and for the CEO. For many companies and boards, building an effective pay-for-performance program requires numerous decisions: 
- What performance metric or metrics should be used to reflect performance?
- What percentage of pay should be linked to incentive or equity compensation? 

To shed light on these issues, this project aims to conduct exploratory analyses to examine the relationship between executive pay and company performance for US-listed firms.

## Final Notes
Datasets execucomp and compustat are required to run the code. Please download them at your own risk from  WRDS (Wharton).

1. Company information(Compustat Database) : Compustat-Capital IQ
Compustat > North America > Fundamental Annual.
Observations are identified by variables gvkey (firm identifier )and fyear (fiscal year).

2. Executive compensation database (Execucomp Database): Compustat-Capital IQ
Compustat > Execucomp > Annual Compensation.
The database provides compensation information for Top 5 executives. Observations are identified by variables execid (executive ID), gvkey (firm identifier) and year (fiscal year). You should rename variable “year” to “fyear” so that you can merge the data with Compustat.

Other databases that could be useful:
1. Share price information (CRSP Database): CRSP > Stock/Security Files > Monthly Stock File. The database provides monthly stock price, return, trading volume information. Observations are identified by variables permno (issue identifier) and date. To map with Compustat database, you would need Compustat CRSP Link file, which is available under CRSP > CRSP/Compustat Merged > Compustat CRSP Link. Alternatively, you could use CRSP > CRSP/Compustat Merged > Fundamentals Annual dataset, where certain pricing information has been merged with Compustat database.

2. Institutional Ownership (Thomson Database): Thomson/Refinitiv > WRDS Thomson Reuters Stock Ownership
3. Analyst coverage and forecast data (IBES Database): IBES > IBES Academic > Detail History
