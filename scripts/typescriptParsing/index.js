"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const utils_1 = require("./utils");
let startBlockNum = 17196022;
const existingBlockNumbers = Array(100).fill((e) => e).map(() => {
    return (startBlockNum++).toString();
});
const finalTransactions = new Array();
const accountsInTransactions = new Array();
(0, utils_1.parseEthereumData)(existingBlockNumbers, finalTransactions, accountsInTransactions).then(resp => {
    (0, utils_1.postCalculateAccounts)(accountsInTransactions, existingBlockNumbers.length);
    (0, utils_1.saveToCsv)(finalTransactions, 'ether.csv');
    (0, utils_1.saveToCsv)(accountsInTransactions, 'etherAccounts.csv');
});
