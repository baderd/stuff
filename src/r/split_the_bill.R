#' ---
#' title: Triglav bill
#' author: Daniel
#' output: html_document
#' ---

library(data.table)
file_bill <- '../../Data/bill.tsv'
bill <- fread(file_bill)
head(bill)

#' # Overview
ov <- merge(
    bill[, .(paid=sum(wieviel), person=zahler), zahler],
    bill[, .(debt=sum(wieviel), person=schuldner), schuldner]
    )[,.(person, paid, debt, total=paid-debt)]
ov

#' # Reduce table to bidirectional pairs
minbill <- bill[, .(sum=sum(wieviel)), by=c("zahler", "schuldner")]
minbill


#' 
#' # Reduce transfers
#' 
#' Identify the person with higher debts for each pair
#' 

all_pairs <- minbill[,combn(unique(zahler), 2)]

for(j in 1:ncol(all_pairs)){
    single_pair <- all_pairs[,j]
    fwd <- minbill[zahler==single_pair[1] & schuldner==single_pair[2], sum]
    bwd <- minbill[zahler==single_pair[2] & schuldner==single_pair[1], sum]
    
    if(fwd>bwd){
        minbill[
            zahler==single_pair[1] & schuldner==single_pair[2], 
            diff:= sum-bwd
            ]
        minbill[
            zahler==single_pair[2] & schuldner==single_pair[1], 
            diff:= 0
            ]
    }else{
        minbill[
            zahler==single_pair[1] & schuldner==single_pair[2], 
            diff:= 0
            ]
        minbill[
            zahler==single_pair[2] & schuldner==single_pair[1], 
            diff:= sum-fwd
            ]
    }
}

#+ results='show'
print(minbill)

#'
#' ## Find transitive debts to reduce number of transactions
#'

donors <- minbill[diff>0, unique(zahler)]
debtors <- minbill[diff>0, unique(schuldner)]

#' * find people that spent and owe money
all_dd <- donors[donors %in% debtors]
all_dd

#' If debts are larger than spendings,
#' find largest spender 
#' and transfer own spendings.

for(dd in all_dd){
    act_debt_total <- minbill[schuldner==dd, sum(diff)]
    act_donor_total <- minbill[zahler==dd, sum(diff)]
    act_max_diff <- minbill[schuldner==dd, max(diff)]
    
    if(act_debt_total > act_donor_total){
        act_largest_spender <- minbill[schuldner==dd & diff==act_max_diff, zahler]
        for(act_debtor in minbill[zahler==dd, schuldner]){
            act_spendings <- minbill[zahler==dd & schuldner==act_debtor, diff]
            minbill[
                zahler==act_largest_spender & schuldner==act_debtor, 
                trans_diff:= diff + act_spendings
                ]
            minbill[zahler==dd & schuldner==act_debtor, trans_diff:=0]
            minbill[
                zahler==act_largest_spender & schuldner==dd,
                trans_diff:= diff - act_spendings
                ]
            # check for negative balance
            if(minbill[zahler==act_largest_spender & schuldner==dd,trans_diff]<0){
                warning(
                    "Negative trans_diff for debtor:", dd, 
                    " at spender:",act_largest_spender
                )
            }
        }
    }
}

#' Minimal number of transactions:
minbill[order(trans_diff, decreasing = T)]



