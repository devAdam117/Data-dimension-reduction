"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.postCalculateAccounts = exports.parseEthereumData = exports.saveToCsv = exports.SimpleAccountInfo = exports.SimpleTransaction = void 0;
const fs = __importStar(require("fs"));
const web3_1 = __importDefault(require("web3"));
const objects_to_csv_1 = __importDefault(require("objects-to-csv"));
const HOST_URI = 'https://intensive-solemn-sunset.discover.quiknode.pro/a6f849586fdee72e579394c9373c69cd540f30c0/';
const web3 = new web3_1.default(HOST_URI);
class SimpleTransaction {
    constructor(blockHash, transactionHash, from, to, gas, gasPrice, value) {
        this.blockHash = blockHash;
        this.transactionHash = transactionHash;
        this.from = from;
        this.to = to;
        this.gas = gas;
        this.gasPrice = gasPrice;
        this.value = value;
    }
}
exports.SimpleTransaction = SimpleTransaction;
class SimpleAccountInfo {
    constructor(id, balance) {
        this.blockBalances = new Array();
        this.minBalance = 0;
        this.maxBalance = 0;
        this.minerFlag = false;
        this.countAsSender = 0;
        this.totalSent = 0;
        this.totalReceived = 0;
        this.coununtAsReceiver = 0;
        this.avgSent = 0;
        this.avgReceived = 0;
        this.currentBlock = '';
        this.id = id;
        this.avgBalance = balance;
        this.blockBalances.push(balance);
    }
}
exports.SimpleAccountInfo = SimpleAccountInfo;
function saveToCsv(data, filename) {
    return __awaiter(this, void 0, void 0, function* () {
        const csv = new objects_to_csv_1.default(data);
        const csvString = yield csv.toString();
        fs.appendFileSync(filename, csvString, { encoding: 'utf8' });
    });
}
exports.saveToCsv = saveToCsv;
function parseEthereumData(existingBlockNumbers, finalTransactions, accountsInTransactions) {
    return __awaiter(this, void 0, void 0, function* () {
        for (const index in existingBlockNumbers) {
            let res = yield web3.eth.getBlock(existingBlockNumbers[index]);
            yield updateEtherData(res, finalTransactions, accountsInTransactions, existingBlockNumbers[index], parseInt(index) + 1);
            console.log(`Iteration: ${index} with blocNumber: ${existingBlockNumbers[index]} has passed. Total parsed ETH transaction: ${finalTransactions.length} and acumulated ${accountsInTransactions.length} different accounts info`);
            if (finalTransactions.length > 10000) {
                break;
            }
        }
    });
}
exports.parseEthereumData = parseEthereumData;
function updateEtherData(blockInfo, finalTransactions, accounts, currentBlockNumber, blockIteration) {
    return __awaiter(this, void 0, void 0, function* () {
        for (const tx of blockInfo.transactions) {
            const transaction = yield web3.eth.getTransaction(tx);
            const ethValue = Number.parseFloat(transaction.value) / (Math.pow(10, 18));
            if (transaction.blockHash === null) {
                continue;
            }
            if (transaction.to === null) {
                continue;
            }
            if (ethValue === 0) {
                continue;
            }
            const simpleTransaction = new SimpleTransaction(transaction.blockHash, transaction.hash, transaction.from, transaction.to, transaction.gas, transaction.gasPrice, ethValue);
            const fromIdx = accounts.findIndex(acc => acc.id === simpleTransaction.from);
            const toIdx = accounts.findIndex(acc => acc.id === simpleTransaction.to);
            if (fromIdx === -1) {
                const balance = Number.parseFloat((yield web3.eth.getBalance(simpleTransaction.from, currentBlockNumber))) / (Math.pow(10, 18));
                const acc = new SimpleAccountInfo(simpleTransaction.from, balance);
                acc.countAsSender++;
                acc.minBalance = balance;
                acc.maxBalance = balance;
                acc.totalSent += simpleTransaction.value;
                acc.currentBlock = currentBlockNumber;
                accounts.push(acc);
            }
            if (fromIdx > -1) {
                accounts[fromIdx].countAsSender++;
                const balance = Number.parseFloat((yield web3.eth.getBalance(simpleTransaction.from, currentBlockNumber))) / (Math.pow(10, 18));
                accounts[fromIdx].minBalance = (balance < accounts[fromIdx].minBalance || accounts[fromIdx].maxBalance === 0) ? balance : accounts[fromIdx].minBalance;
                accounts[fromIdx].maxBalance = (balance > accounts[fromIdx].maxBalance || accounts[fromIdx].maxBalance === 0) ? balance : accounts[fromIdx].maxBalance;
                accounts[fromIdx].totalSent += simpleTransaction.value;
                if (accounts[fromIdx].currentBlock !== currentBlockNumber) {
                    accounts[fromIdx].blockBalances.push(balance);
                    accounts[fromIdx].currentBlock = currentBlockNumber;
                }
            }
            if (toIdx === -1) {
                const balance = Number.parseFloat((yield web3.eth.getBalance(simpleTransaction.to, currentBlockNumber))) / (Math.pow(10, 18));
                const acc = new SimpleAccountInfo(simpleTransaction.to, balance);
                acc.coununtAsReceiver++;
                acc.minBalance = balance;
                acc.maxBalance = balance;
                acc.totalReceived += simpleTransaction.value;
                acc.currentBlock = currentBlockNumber;
                accounts.push(acc);
            }
            if (toIdx > -1) {
                accounts[toIdx].coununtAsReceiver++;
                const balance = Number.parseFloat((yield web3.eth.getBalance(simpleTransaction.to, currentBlockNumber))) / (Math.pow(10, 18));
                accounts[toIdx].minBalance = (balance < accounts[toIdx].minBalance || accounts[toIdx].minBalance === 0) ? balance : accounts[toIdx].minBalance;
                accounts[toIdx].maxBalance = (balance > accounts[toIdx].maxBalance || accounts[toIdx].maxBalance === 0) ? balance : accounts[toIdx].maxBalance;
                accounts[toIdx].totalReceived += simpleTransaction.value;
                accounts[toIdx].totalSent += simpleTransaction.value;
                if (accounts[toIdx].currentBlock !== currentBlockNumber) {
                    accounts[toIdx].blockBalances.push(balance);
                    accounts[toIdx].currentBlock = currentBlockNumber;
                }
            }
            finalTransactions.push(simpleTransaction);
        }
        // kto je miner
        const minerIdx = accounts.findIndex(acc => acc.id === blockInfo.miner);
        if (minerIdx > -1) {
            accounts[minerIdx].minerFlag = true;
        }
        accounts.forEach(acc => {
            if (acc.blockBalances[blockIteration - 1]) {
                return;
            }
            acc.blockBalances.push(acc.blockBalances[acc.blockBalances.length - 1]);
        });
    });
}
function postCalculateAccounts(accounts, numOfBlocks) {
    accounts.forEach(acc => {
        acc.avgReceived = acc.coununtAsReceiver !== 0 ? acc.totalReceived / acc.coununtAsReceiver : 0;
        acc.avgSent = acc.countAsSender !== 0 ? acc.totalSent / acc.countAsSender : 0;
        acc.avgBalance = acc.blockBalances.reduce((a, b) => { return a + b; }) / numOfBlocks;
    });
}
exports.postCalculateAccounts = postCalculateAccounts;
// este pridat 
