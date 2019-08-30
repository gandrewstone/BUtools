Bitcoin Unlimited Transaction Generation Scripts
================================================

## Installation

* Clone this repo with the following command
```
git clone --recursive -j8 git://github.com/gandrewstone/BUtools.git
```

* If you've already cloned this repo use
```
cd BUtools
git submodule update --init --recursive
```

* Compile dependencies
sudo apt-get install python-setuptools
cd pythonbitcoinlib
python setup.py build

## Operation

### Create spam
1. Start bitcoind or bitcoin-qt and ensure you can access the cli

2. Get a large balance

3. Create a lot of spendable outputs (~10000), by executing "split" several times

3. Start creating transaction using ./txntest.py <network> spam

### Discover how many spendable outputs you have
```
./txntest.py <network> unspent
```

### Consolidate your spendable outputs
```
./txntest.py <network> join 50 100
```
(join 50 different outputs, 100 times)


## Help

```
./txnTest.py <network> <operation> [operation specific arguments]
  network can be: "testnet", "regtest", "nol", "main"
  operation can be: "split", "join", "spam", "unspent", "info"
    split: create more UTXOs.
      parameters: [nSplits: takes every UTXO that has sufficient balance and splits it into this many more UTXOs, default 25]
      example: ./txnTest.py nol split 10
    join: consolidate UTXOs.
      parameters: <nJoin: take this many UTXOs and join them into 1>  <nRepeat: repeat the join this many times>
      example that joins 50 UTXOs into one output 2 times: ./txnTest.py nol join 50 2
    spam: generate a lot of transactions, by paying to myself.
      example: ./txnTest.py nol spam
```
