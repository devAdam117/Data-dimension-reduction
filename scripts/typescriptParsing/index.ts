import {Transaction} from "web3-core";
import { SimpleAccountInfo, SimpleTransaction, parseEthereumData, saveToCsv, postCalculateAccounts } from "./utils";



let startBlockNum: number = 17196022;
const existingBlockNumbers: Array<string> = Array(100).fill((e:number)=>e).map(() => {
    return (startBlockNum ++).toString();
});
const finalTransactions: Array<SimpleTransaction> = new Array<SimpleTransaction>();
const accountsInTransactions: Array<SimpleAccountInfo> = new Array<SimpleAccountInfo>();


parseEthereumData(existingBlockNumbers,finalTransactions,accountsInTransactions).then(resp => {
    postCalculateAccounts(accountsInTransactions, existingBlockNumbers.length);
    saveToCsv(finalTransactions, 'ether.csv');
    saveToCsv(accountsInTransactions, 'etherAccounts.csv');
});

