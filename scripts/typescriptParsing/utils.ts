import * as fs from "fs";
import Web3 from "web3";
import {Transaction} from "web3-core";

import ObjectsToCsv from "objects-to-csv";

const HOST_URI = 'https://intensive-solemn-sunset.discover.quiknode.pro/a6f849586fdee72e579394c9373c69cd540f30c0/'
const web3: Web3 = new Web3(HOST_URI);
export class SimpleTransaction{
    public blockHash: string;
    public transactionHash: string;
    public from: string;
    public to: string;
    public gas: number;
    public gasPrice: string;
    public value: number;
    constructor(blockHash: string, transactionHash: string, from: string, to: string, gas: number, gasPrice: string, value: number){
        this.blockHash = blockHash;
        this.transactionHash = transactionHash;
        this.from = from;
        this.to = to;
        this.gas = gas;
        this.gasPrice = gasPrice;
        this.value = value;
    }
}

export class SimpleAccountInfo{
    public id:string;
    public avgBalance:number;
    public blockBalances: Array<number> = new Array<number>();
    public minBalance: number = 0;
    public maxBalance: number = 0;
    public minerFlag:boolean = false;
    public countAsSender: number = 0;
    public totalSent: number = 0;
    public totalReceived: number = 0;
    public coununtAsReceiver: number = 0;
    public avgSent: number = 0;
    public avgReceived: number = 0;
    public currentBlock: string = '';
    constructor(id:string,balance:number){
        this.id = id;
        this.avgBalance = balance;
        this.blockBalances.push(balance);
    }
}

export async function saveToCsv(data: any[], filename: string) {
    const csv = new ObjectsToCsv(data);
    const csvString = await csv.toString();
    fs.appendFileSync(filename, csvString, { encoding: 'utf8' });
}


export async function parseEthereumData(existingBlockNumbers: Array<string>,finalTransactions: Array<SimpleTransaction>, accountsInTransactions: Array<SimpleAccountInfo>){
    for(const index in existingBlockNumbers){
        let res = await web3.eth.getBlock(existingBlockNumbers[index]);
        await updateEtherData(res,finalTransactions, accountsInTransactions, existingBlockNumbers[index], parseInt(index) + 1);
        console.log(`Iteration: ${index} with blocNumber: ${existingBlockNumbers[index]} has passed. Total parsed ETH transaction: ${finalTransactions.length} and acumulated ${accountsInTransactions.length} different accounts info`)
        if(finalTransactions.length > 10000){
            break;
        }
    }
    
}


async function updateEtherData(blockInfo: any, finalTransactions: Array<SimpleTransaction>, accounts: Array<SimpleAccountInfo>, currentBlockNumber: string, blockIteration: number): Promise<void>{
    for (const tx of blockInfo.transactions) {
        const transaction: Transaction = await web3.eth.getTransaction(tx);
        const ethValue: number = Number.parseFloat(transaction.value)/(Math.pow(10,18));
        
        if(transaction.blockHash === null){
            continue;
        }
        if(transaction.to === null){
            continue;
        }
        if(ethValue  === 0){
            continue;
        }
        
        
        const simpleTransaction = new SimpleTransaction(transaction.blockHash, transaction.hash, transaction.from, transaction.to, transaction.gas, transaction.gasPrice, ethValue);
        const fromIdx: number = accounts.findIndex(acc => acc.id === simpleTransaction.from)
        const toIdx: number = accounts.findIndex(acc => acc.id === simpleTransaction.to)
        if(fromIdx === -1){
            const balance: number = Number.parseFloat((await web3.eth.getBalance(simpleTransaction.from, currentBlockNumber)))/(Math.pow(10,18));
            const acc: SimpleAccountInfo = new SimpleAccountInfo(simpleTransaction.from,balance);
            acc.countAsSender++;
            acc.minBalance = balance;
            acc.maxBalance = balance;
            acc.totalSent += simpleTransaction.value;
            acc.currentBlock = currentBlockNumber;
            accounts.push(acc);
        }
        if(fromIdx > -1){
            accounts[fromIdx].countAsSender++;
            const balance: number = Number.parseFloat((await web3.eth.getBalance(simpleTransaction.from, currentBlockNumber)))/(Math.pow(10,18));
            accounts[fromIdx].minBalance = (balance < accounts[fromIdx].minBalance || accounts[fromIdx].maxBalance === 0)? balance:accounts[fromIdx].minBalance;
            accounts[fromIdx].maxBalance = (balance > accounts[fromIdx].maxBalance || accounts[fromIdx].maxBalance === 0)? balance:accounts[fromIdx].maxBalance;
            accounts[fromIdx].totalSent += simpleTransaction.value;
            if(accounts[fromIdx].currentBlock !== currentBlockNumber) {
                accounts[fromIdx].blockBalances.push(balance);
                accounts[fromIdx].currentBlock = currentBlockNumber;
            }
        }
        if(toIdx === -1){
            const balance: number = Number.parseFloat((await web3.eth.getBalance(simpleTransaction.to, currentBlockNumber)))/(Math.pow(10,18));
            const acc: SimpleAccountInfo = new SimpleAccountInfo(simpleTransaction.to,balance);
            acc.coununtAsReceiver++;
            acc.minBalance = balance;
            acc.maxBalance = balance;
            acc.totalReceived += simpleTransaction.value;
            acc.currentBlock = currentBlockNumber;
            accounts.push(acc);
        }
        if(toIdx > -1){
            accounts[toIdx].coununtAsReceiver++;
            const balance: number = Number.parseFloat((await web3.eth.getBalance(simpleTransaction.to, currentBlockNumber)))/(Math.pow(10,18));
            accounts[toIdx].minBalance = (balance < accounts[toIdx].minBalance || accounts[toIdx].minBalance === 0 )? balance:accounts[toIdx].minBalance;
            accounts[toIdx].maxBalance = (balance > accounts[toIdx].maxBalance || accounts[toIdx].maxBalance === 0)? balance:accounts[toIdx].maxBalance;
            accounts[toIdx].totalReceived += simpleTransaction.value;
            accounts[toIdx].totalSent += simpleTransaction.value;
            if(accounts[toIdx].currentBlock !== currentBlockNumber) {
                accounts[toIdx].blockBalances.push(balance);
                accounts[toIdx].currentBlock = currentBlockNumber;
            }
        }
        finalTransactions.push(simpleTransaction);
      }
      // kto je miner
      const minerIdx = accounts.findIndex(acc => acc.id === blockInfo.miner);
      if(minerIdx > -1){
          accounts[minerIdx].minerFlag = true;
      }

      accounts.forEach(acc => {
        if(acc.blockBalances[blockIteration - 1]){
            return;
        } 
        acc.blockBalances.push(acc.blockBalances[acc.blockBalances.length - 1]);
      });
}

export function postCalculateAccounts(accounts: Array<SimpleAccountInfo>, numOfBlocks: number){
    accounts.forEach(acc => {
        acc.avgReceived = acc.coununtAsReceiver !== 0 ? acc.totalReceived/acc.coununtAsReceiver: 0;
        acc.avgSent = acc.countAsSender !== 0 ? acc.totalSent/acc.countAsSender: 0;
        acc.avgBalance = acc.blockBalances.reduce((a,b) => {return a + b})/ numOfBlocks;
    })

}

// este pridat 


