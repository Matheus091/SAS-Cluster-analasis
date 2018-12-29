proc sort data=p.desc; by symbol;run;
proc sort data=p.fundamentals; by Symbol;run;

data p.fun_desc;
merge p.fundamentals p.desc;
by Symbol;
run;

ods rtf file='D:\SAS\contents.rtf';
Proc contents data=p.fun_desc;
run;
ods rtf close;

ods rtf file='D:\SAS\freq1.rtf';
proc freq data=p.fun_desc;
table For_Year / missing;
run;
ods rtf close;

proc means data=p.fun_desc n nmiss;
class For_Year;
var _numeric_;
where For_Year = 2014;
run;

/*do dalszej analizy z populacji ze względu na dużą liczebność próby wybieram spółki z danymi za 2014 rok*/

data p.fun_desc14;
set p.fun_desc;
where For_Year = 2014;
run;

/*usówam ze zbioru zmienne które cechuja udział braków >10%: Quick_Ratio, Cash_Ratio,Current_Ratio*/
data p.fun_desc14s (drop=Quick_Ratio Cash_Ratio Current_Ratio Period_Ending);
set p.fun_desc14;
run;

data recode;
set p.fun_desc14s;
array zm(73) Accounts_Payable	Accounts_Receivable	Add_l_income_expense_items	After_Tax_ROE	Capital_Expenditures	Capital_Surplus	Cash_and_Cash_Equivalents
Changes_in_Inventories	Common_Stocks	Cost_of_Revenue	Deferred_Asset_Charges	Deferred_Liability_Charges	Depreciation	Earnings_Before_Interest_and_Tax
Earnings_Before_Tax	Effect_of_Exchange_Rate	Equity_Earnings_Loss_Unconsolida	Fixed_Assets	Goodwill	Gross_Margin	Gross_Profit	Income_Tax
Intangible_Assets	Interest_Expense	Inventory	Investments	Liabilities	Long_Term_Debt	Long_Term_Investments	Minority_Interest	Misc__Stocks
Net_Borrowings	Net_Cash_Flow	Net_Cash_Flow_Operating	Net_Cash_Flows_Financing	Net_Cash_Flows_Investing	Net_Income	Net_Income_Adjustments
Net_Income_Applicable_to_Common	Net_Income_Cont__Operations	Net_Receivables	Non_Recurring_Items	Operating_Income	Operating_Margin	Other_Assets
Other_Current_Assets	Other_Current_Liabilities	Other_Equity	Other_Financing_Activities	Other_Investing_Activities	Other_Liabilities
Other_Operating_Activities	Other_Operating_Items	Pre_Tax_Margin	Pre_Tax_ROE	Profit_Margin	Research_and_Development	Retained_Earnings
Sale_and_Purchase_of_Stock	Sales_General_and_Admin_	Short_Term_Debt___Current_Portio	Short_Term_Investments	Total_Assets	Total_Current_Assets
Total_Current_Liabilities	Total_Equity	Total_Liabilities	Total_Liabilities___Equity	Total_Revenue	Treasury_Stock	For_Year	Earnings_Per_Share
Estimated_Shares_Outstanding;
do i=1 to 73;
if zm(i)=0 then zm(i)=1;
else zm(i)=0;
end;
drop i;
run;

proc means data=work.recode sum;
output out=me sum (Accounts_Payable -- Estimated_Shares_Outstanding)=; 
run;

proc transpose data=me out=data3;
run;
proc  sort data=data3; by descending col1;run;
/*z próby wyrzócam zmienne, które mają udział 0 większy niż 5%:

Misc__Stocks
Research_and_Development
Equity_Earnings_Loss_Unconsolida
Short_Term_Investments
Deferred_Asset_Charges
Non_Recurring_Items
Minority_Interest
Long_Term_Investments
Other_Operating_Items
Treasury_Stock
Other_Current_Liabilities
Changes_in_Inventories
Deferred_Liability_Charges
Effect_of_Exchange_Rate
Inventory
Intangible_Assets
Other_Financing_Activities
Short_Term_Debt___Current_Portio
Investments
Goodwill
Other_Current_Assets
Add_l_income_expense_items
Total_Current_Assets
Total_Current_Liabilities
Capital_Surplus
Interest_Expense
Accounts_Receivable
Long_Term_Debt
Other_Liabilities
Other_Assets
Net_Receivables
Cost_of_Revenue
Net_Borrowings
Common_Stocks
Operating_Margin 

*/

data p.fun_desc14ss;
set p.fun_desc14s 
(drop=Misc__Stocks
Research_and_Development
Equity_Earnings_Loss_Unconsolida
Short_Term_Investments
Deferred_Asset_Charges
Non_Recurring_Items
Minority_Interest
Long_Term_Investments
Other_Operating_Items
Treasury_Stock
Other_Current_Liabilities
Changes_in_Inventories
Deferred_Liability_Charges
Effect_of_Exchange_Rate
Inventory
Intangible_Assets
Other_Financing_Activities
Short_Term_Debt___Current_Portio
Investments
Goodwill
Other_Current_Assets
Add_l_income_expense_items
Total_Current_Assets
Total_Current_Liabilities
Capital_Surplus
Interest_Expense
Accounts_Receivable
Long_Term_Debt
Other_Liabilities
Other_Assets
Net_Receivables
Cost_of_Revenue
Net_Borrowings
Common_Stocks
Operating_Margin);
run;

/*z analizy wyrzucam zmienne, które niosą niską wartość merytoryczną:
Accounts_Payable
Depreciation
Estimated_Shares_Outstanding
Income_Tax
Net_Income_Adjustments
Net_Income_Applicable_to_Common
Net_Income_Cont__Operations
Other_Equity
Other_Investing_Activities
Other_Operating_Activities
Sales_General_and_Admin_
Total_Liabilities___Equity
For_Year
*/

data p.fun_desc14sss;
set p.fun_desc14ss 
(drop=Accounts_Payable
Depreciation
Estimated_Shares_Outstanding
Income_Tax
Net_Income_Adjustments
Net_Income_Applicable_to_Common
Net_Income_Cont__Operations
Other_Equity
Other_Investing_Activities
Other_Operating_Activities
Sales_General_and_Admin_
Total_Liabilities___Equity
For_Year);
run;

proc means data=p.fun_desc14sss;
output out=means;
run;

proc transpose data=means out=means2;
id _stat_;
run;

data p.wspzm;
set means2;
wspzm=std/mean;
run;

proc sort data=p.wspzm out=p.wspzm; by descending wspzm;run;

proc means data=p.wspzm;
var wspzm;
run;

/*wszystkie zmienne w dobranej próbie cechuje współczynnik zmienności o wartości conajmniej 54%*/

proc corr data=p.fun_desc14sss noprob nosimple nomiss out=corrmatrix;
	var _NUMERIC_;
run;

data corrmatrix2;
set corrmatrix;
where _type_ = "CORR";
run;

data corrmatrix3;
set corrmatrix2 (drop=_type_);
run;

ods rtf file='D:\SAS\corrmatrix.rtf';
proc iml;
use corrmatrix3;
read all var { After_Tax_ROE	Capital_Expenditures	Cash_and_Cash_Equivalents	Earnings_Before_Interest_and_Tax	Earnings_Before_Tax	Fixed_Assets
	Gross_Margin	Gross_Profit	Liabilities	Net_Cash_Flow	Net_Cash_Flow_Operating	Net_Cash_Flows_Financing	Net_Cash_Flows_Investing
	Net_Income	Operating_Income	Pre_Tax_Margin	Pre_Tax_ROE	Profit_Margin	Retained_Earnings	Sale_and_Purchase_of_Stock	Total_Assets
	Total_Equity	Total_Liabilities	Total_Revenue	Earnings_Per_Share} into zmienne;
read all var {_NAME_} into etykieta;
	i=ginv(zmienne);
	print i [r=etykieta c=etykieta];
	create p.odwmackor from i [colname= {After_Tax_ROE	Capital_Expenditures	Cash_and_Cash_Equivalents	Earnings_Before_Interest_and_Tax	Earnings_Before_Tax	Fixed_Assets
	Gross_Margin	Gross_Profit	Liabilities	Net_Cash_Flow	Net_Cash_Flow_Operating	Net_Cash_Flows_Financing	Net_Cash_Flows_Investing
	Net_Income	Operating_Income	Pre_Tax_Margin	Pre_Tax_ROE	Profit_Margin	Retained_Earnings	Sale_and_Purchase_of_Stock	Total_Assets
	Total_Equity	Total_Liabilities	Total_Revenue	Earnings_Per_Share}];
	append from i;
	close p.odwmackor;
run;

ods rtf close;
/*w modelu pozostawiam zmienne, które cechuje współczynnik autokorelacji niższy niż 10 (wartość na głównej przekątnej odwróconej macierzy korelacji)*/

data p.as;
set p.fun_desc14sss (keep=SYMBOL
NAME
SECTOR 
GROSS_MARGIN
LIABILITIES
PRE_TAX_MARGIN
PROFIT_MARGIN
RETAINED_EARNINGS
SALE_AND_PURCHASE_OF_STOCK
TOTAL_REVENUE
EARNINGS_PER_SHARE);
run;

proc stdize data=p.as out=p.as_std method=range;
var GROSS_MARGIN LIABILITIES PRE_TAX_MARGIN PROFIT_MARGIN RETAINED_EARNINGS SALE_AND_PURCHASE_OF_STOCK TOTAL_REVENUE EARNINGS_PER_SHARE;
run;

%macro fclus(maxcl);

	proc fastclus data=p.as_std mean=p.as_std_fclus out=p.as_std_fclus2 drift cluster=preclus maxclusters=&maxcl maxiter=1000000 converge=0 list;
		var GROSS_MARGIN LIABILITIES PRE_TAX_MARGIN PROFIT_MARGIN RETAINED_EARNINGS SALE_AND_PURCHASE_OF_STOCK TOTAL_REVENUE EARNINGS_PER_SHARE;
		id Symbol;
	run;
%mend;


%macro cluster (method, print);
	ods graphics on;

	proc cluster data=p.as_std_fclus method=&method ccc pseudo plots(unpack)=ccc pseudo print=&print outtree=p.endclusters;
		var GROSS_MARGIN LIABILITIES PRE_TAX_MARGIN PROFIT_MARGIN RETAINED_EARNINGS SALE_AND_PURCHASE_OF_STOCK TOTAL_REVENUE EARNINGS_PER_SHARE;
		copy preclus;
	run;

	ods graphics off;
%mend

ods rtf file='D:\SAS\fastclus.rtf';
proc fastclus data=p.as_std mean=p.as_std_fclus out=p.as_std_fclus2 drift cluster=preclus maxclusters=60 maxiter=1000000 converge=0 list;
var GROSS_MARGIN LIABILITIES PRE_TAX_MARGIN PROFIT_MARGIN RETAINED_EARNINGS SALE_AND_PURCHASE_OF_STOCK TOTAL_REVENUE EARNINGS_PER_SHARE;
id Symbol;
run;
ods rtf close;

%fclus(60);
%cluster(ward,10);

ods rtf file='D:SAS\cluster.rtf';
	ods graphics on;

	proc cluster data=p.as_std_fclus method=ward ccc pseudo plots(unpack)=ccc pseudo print=10 outtree=p.endclusters;
		var GROSS_MARGIN LIABILITIES PRE_TAX_MARGIN PROFIT_MARGIN RETAINED_EARNINGS SALE_AND_PURCHASE_OF_STOCK TOTAL_REVENUE EARNINGS_PER_SHARE;
		copy preclus;
	run;

	ods graphics off;
ods rtf close;

/*%cluster(average,5);*/
/*%cluster(centroid,5);*/
/*%cluster(complete,5);*/
/*%cluster(mcquity,5);*/
/*%cluster(median,5);*/
/*%cluster(single,5);*/
/*%cluster(ward,5);*/
/*%cluster(eml,5);*/
/*%cluster(flexible,5);*/

proc tree data=p.endclusters out=p.tree nclusters=5 horizontal; 
id _name_;
run;


proc freq data=p.tree;
table cluster / nocum ;
run;

proc sql;
create table atree as
select * , substr(_NAME_,3,2) as preclus
from p.tree;
run;

data atree2;
set atree;
npreclus=input(preclus,8.);
drop preclus;
rename npreclus=preclus;
run;

proc sort data=p.as_std_fclus2; by preclus;run;
proc sort data=atree2; by preclus;run;

data p.clusterjoin;
merge p.as_std_fclus2 work.atree2;
by preclus;
run;

proc sort data=p.clusterjoin; by cluster preclus;run;


proc freq data=p.clusterjoin;
table cluster / nocum;
run;



proc freq data=p.clusterjoin;
table Sector*cluster / nocol norow nocum nofreq;
run;

data clusterjoin2;
set p.clusterjoin (keep=Symbol Name Sector preclus CLUSTER);
run;

proc sort data=clusterjoin2; by symbol;run;
proc sort data=p.as; by symbol;run;

data p.clusterjoin3;
merge clusterjoin2 p.as;
by Symbol;
run;

proc print data=p.clusterjoin3;
where cluster=4;
run;

proc means data=p.clusterjoin3 maxdec=0 mean std;
class cluster;
var GROSS_MARGIN LIABILITIES PRE_TAX_MARGIN PROFIT_MARGIN RETAINED_EARNINGS SALE_AND_PURCHASE_OF_STOCK TOTAL_REVENUE EARNINGS_PER_SHARE;
output out=p.ameans mean (GROSS_MARGIN LIABILITIES PRE_TAX_MARGIN PROFIT_MARGIN RETAINED_EARNINGS SALE_AND_PURCHASE_OF_STOCK TOTAL_REVENUE EARNINGS_PER_SHARE)=;
run;

proc candisc data=p.clusterjoin3 out=spolki;
var GROSS_MARGIN LIABILITIES PRE_TAX_MARGIN PROFIT_MARGIN RETAINED_EARNINGS SALE_AND_PURCHASE_OF_STOCK TOTAL_REVENUE EARNINGS_PER_SHARE;
class cluster;
run;

proc sgplot data=spolki;
scatter x=Can1 y=Can2 / group=cluster;
run;

/*
GROSS_MARGIN The ratio of gross profit to revenues

LIABILITIES a firm's obligations to its creditors

PRE_TAX_MARGIN  is the ratio of a company's pre-tax earnings to its total sales 

PROFIT_MARGIN measures the proportion of sales that finds its way into profits (net income/sales) 

RETAINED_EARNINGS  cumulative net earnings or profits of a company after accounting for dividend payments. 

SALE_AND_PURCHASE_OF_STOCK 

TOTAL_REVENUE Total revenue is the total receipts a seller can obtain from selling goods or service to buyers

EARNINGS_PER_SHARE firm's net income divided by the total number of shares outstanding

*/



/*proc sql;*/
/*create table sectors as*/
/*select distinct(strip(sector))*/
/*from p.clusterjoin3*/
/*order by sector desc;*/
/*run;*/