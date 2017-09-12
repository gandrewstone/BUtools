#!/usr/bin/env python2
import sys, pdb
import bitcoin
import bitcoin.rpc
import bitcoin.core
import bitcoin.wallet
import time
import types
import datetime
from decimal import *
import httplib
import socket
import random

#bitcoin.SelectParams('testnet')
bitcoin.SelectParams('regtest')

BTC = 100000000
mBTC = 100000
uBTC = 100

DEFAULT_TX_FEE = 10

PerfectFractions = True

cnxn = None

def main(op, params=None):
  global cnxn
  cnxn = bitcoin.rpc.Proxy()

  try:
    print ("Balance: ", cnxn.getbalance())
  except ValueError as v:
    print(str(v))
    pdb.set_trace()

  if op=="unspent":
    wallet = cnxn.listunspent()
    print ("This wallet has %d unspent outputs." % len(wallet))
    spendable = filter(lambda x: x["spendable"], wallet)
    print ("  spendable txos: %d" % len(spendable))
    satSpendable = 0
    for s in spendable:
      satSpendable += s["amount"]
    print ("  spendable satoshis: %d" % satSpendable)

  if op=="join":
    addrs = [cnxn.getnewaddress(),cnxn.getnewaddress()]

    if len(params):
      amt = int(params[0])
    else:
      amt = 100
    if len(params)>1:
      repeat = int(params[1])
    else:
      repeat = 1

    wallet = cnxn.listunspent()
    # print "This wallet has %d unspent outputs.  Joining the %d at offset %d." % (len(wallet),amt, offset)
    # consolidate(wallet[offset:offset+amt],cnxn.getnewaddress(), cnxn)
    print ("This wallet has %d unspent outputs.  Joining %d, %d times." % (len(wallet),amt, repeat))
    offset = 100
    for cnt in range(0,repeat):
      print (cnt)
      bigAmt = wallet[0]
      itr = 0
      idx = 0
      for tx in wallet:  # Find a larger utxo that will pay for a lot of dust
        if tx["spendable"] is True and bigAmt["amount"] < tx["amount"]:
          bigAmt = tx
          idx = itr
        itr += 1
      del wallet[idx]
      print (str(bigAmt))
      consolidate(wallet[offset:offset+amt] + [bigAmt],addrs[0], cnxn)
      del wallet[offset:offset+amt] # delete all the entries I just used
      offset+=amt
      if offset > len(wallet): break

  #wallet = cnxn.listunspent()
  #addrs = [cnxn.getnewaddress() for i in range(0,10)]
  #split([wallet[0]],addrs, cnxn)
  if op=="spamtill":
    if len(params):
      poolSize = int(params[0])
    else:
      poolSize = None
    amt = None
    addrs = [cnxn.getnewaddress() for i in range(0,25)]
    while 1:
      try:
        spamTx(cnxn,50000,addrs, amt,False,mempoolTarget=poolSize)
      except bitcoin.rpc.JSONRPCError as e:
        print ("Out of addresses.  Sleeping")
        time.sleep(60)
      except httplib.BadStatusLine as e:
        cnxn = bitcoin.rpc.Proxy()
      except (socket.error,socket.timeout) as e:  # connection refused.  Sleep and retry
        while 1:
          try:
            time.sleep(30)
            cnxn = bitcoin.rpc.Proxy()
            break
          except:
            pass

  if op=="spam":
    if len(params):
      amt = int(params[0])
    else:
      amt = None
    addrs = [cnxn.getnewaddress() for i in range(0,5)]
    # addrs = cnxn.getaddressesbyaccount("")
    while 1:
      try:
        spamTx(cnxn,50000,addrs, amt,False)
      except bitcoin.rpc.JSONRPCError as e:
        print ("Out of addresses.  Sleeping")
        time.sleep(60)
      except httplib.BadStatusLine as e:
        cnxn = bitcoin.rpc.Proxy()
      except (socket.error,socket.timeout) as e:  # connection refused.  Sleep and retry
        while 1:
          try:
            time.sleep(30)
            cnxn = bitcoin.rpc.Proxy()
            break
          except:
            pass

  if op=="sweep":
    wallet = cnxn.listunspent()
    offset = 100
    spend = []
    while len(spend) < 10:
      if not wallet:
        break
      tx = wallet[0]
      del wallet[0]

      if tx["spendable"] is True and tx["amount"] < 100000 and tx["confirmations"] > 0:
        print (str(tx))
        spend.append(tx)

    if spend:
      consolidate(spend,cnxn.getnewaddress(), cnxn,5000*len(spend))
    else:
      print ("there is nothing to sweep")

  if op=="split":
    if len(params):
      nSplits = int(params[0])
    else:
      nSplits = 25
    if len(params)>1:
      fee = int(params[1])
    else:
      fee = 100

    wallet = cnxn.listunspent()
    j = 0
    addrs = [cnxn.getnewaddress() for i in range(0,nSplits)]
    for w in wallet:
      j+=1
      if w['amount'] > nSplits*(BTC/10000):
        if 1: # try:
          split([w],addrs, cnxn, fee)
          print ("split %d satoshi into %d addrs fee %d %s" % (w['amount'],nSplits, fee, str(addrs)))
        else:  # :except bitcoin.rpc.JSONRPCError as e:
          print ("\n%d: Exception %s" % (j,str(e)))
          pdb.set_trace()
      else:
        print ("address has only %d satoshi" % w['amount'])
      # else: print "Split: %d" % j

  if op=="info":
    blkid = cnxn.getbestblockhash()
    blk = cnxn.getblock(blkid)
    txn = blk.vtx[0]
    print (txn.vin)
    print (txn.vout)
  # cnxn.sendrawtransaction(txn)  # u'transaction already in block chain'  code: -27
  #pdb.set_trace()

def generate(amt=1,cnxn=None):
  if cnxn is None: cnxn = bu
  cnxn._call("generate",amt)

def spamTx(bu, numTx,addrp,amt = None,gen=False, mempoolTarget=None):
  addr = addrp
  print ("SPAM")
  lastGenerate = -1
  start = time.time()
  if amt == None:
    randAmt = True
  else: randAmt = False
  for i in range(0, numTx):
    if (i!=0) and (i & 255) == 0:
      end = time.time()
      interval = end - start
      start = end
      print ("issued 256 payments in %f seconds.  %f payments/sec" % (interval, 256.0/interval))
      if mempoolTarget:  # if the mempool is too big, wait for it to be reduced
        while True:
          time.sleep(10) # give time for other threads to run and sync tx from other nodes
          mempoolData=bu._call("getmempoolinfo")
          mempoolBytes = mempoolData["bytes"]
          if mempoolBytes < mempoolTarget:
            break
          blockNum = bu._call("getblockcount")
          print("mempool is %d bytes, %d tx. block %d.  Waiting..." % (mempoolBytes, mempoolData["size"], blockNum))
    if addrp is None:
      print ("creating new address")
      addr = bu._call('getnewaddress')
    if type(addrp) is types.ListType:
      addr = addrp[i%len(addrp)]
    if type(addrp) is types.ListType:
      change = addrp[(i+3)%len(addrp)]
    else:
      change = None
    if randAmt:
      amt = random.randint(100*uBTC, BTC/2)
    print ("Count ", i, "Send %d to %s" % (amt, str(addr)))
    try:
      bu.sendtoaddress(addr, amt, "", "", False, change)
    except bitcoin.rpc.JSONRPCError as e:
      print("except:", str(e))
      if "Fee is larger" in str(e) and randAmt:
        pass
      else: raise
    except bitcoin.rpc.JSONRPCError as e:
      print("except 2")
      if gen and i > lastGenerate:  # Out of TxOuts in the wallet so commit these txn
        generate()
        print ("\nGenerated at count %d.  Interval %d" % (i, i-lastGenerate))
        lastGenerate = i
      else:
        print ("\n%d: Exception %s" % (i,str(e)))
        raise
    finally:
      pass


def split(frm, toAddrs, cnxn, txfee=DEFAULT_TX_FEE):
  inp = []
  getcontext().prec = 8
  amount = Decimal(0)
  for tx in frm:
#      inp.append({"txid":str(tx["txid"]),"vout":tx["vout"]})
      inp.append({"txid":bitcoin.core.b2lx(tx["outpoint"].hash),"vout":tx["outpoint"].n})
      amount += tx["amount"]

  outp = {} # = { str(toAddr): str((amount-txfee)/BTC) }
  getcontext().prec = 8
  amtPer = (Decimal(amount-txfee)/len(toAddrs)).to_integral_value()
  print ("amount: ", amount, " amount per: ", amtPer, "from :", len(frm), "to: ", len(toAddrs), "tx fee: ", txfee)

  for a in toAddrs[0:-1]:
    if PerfectFractions:
      outp[str(a)] = str(amtPer/BTC)
    else:
      outp[str(a)] = float(amtPer/BTC)

  a = toAddrs[-1]
  amtPer = (amount - ((len(toAddrs)-1)*amtPer)) - txfee
  print ("final amt: ", amtPer)
  if PerfectFractions:
      outp[str(a)] = str(amtPer/BTC)
  else:
      outp[str(a)] = float(amtPer/BTC)

  try:
    txn = cnxn._call("createrawtransaction",inp, outp)
    signedtxn = cnxn._call("signrawtransaction",str(txn))
    if signedtxn["complete"]:
      cnxn._call("sendrawtransaction", signedtxn["hex"])
  except bitcoin.rpc.JSONRPCError as e:
    print (str(e))


def consolidate(frm, toAddr, cnxn, txfee=DEFAULT_TX_FEE):
  #out = bitcoin.core.CTxOut(frm["amount"],toAddr)
  #script = bitcoin.core.CScript()
  # bitcoin.wallet.CBitcoinAddress(toAddr)
  # pdb.set_trace()
  inp = []
  amount = Decimal(0)
  for tx in frm:
      # pdb.set_trace()
      if tx["spendable"] is True and tx["confirmations"] > 0:
        inp.append({"txid":bitcoin.core.b2lx(tx["outpoint"].hash),"vout":tx["outpoint"].n})
        amount += tx["amount"]

  #out = bitcoin.core.CMutableTxOut(frm["amount"],toAddr.to_scriptPubKey())
  if PerfectFractions:
     outamt = str((amount-txfee)/BTC)
  else:
     outamt = float((amount-txfee)/BTC)

  out = { str(toAddr): outamt }
  #txn = bitcoin.core.CMutableTransaction(inp,[out])
  print(inp)
  print(out)
  txn = cnxn._call("createrawtransaction",inp, out)
  signedtxn = cnxn._call("signrawtransaction",str(txn))
  if signedtxn["complete"]:
    cnxn._call("sendrawtransaction", signedtxn["hex"])

def consolidate2(frm, toAddr, cnxn):
  #out = bitcoin.core.CTxOut(frm["amount"],toAddr)
  #script = bitcoin.core.CScript()
  # bitcoin.wallet.CBitcoinAddress(toAddr)

  inp = []
  for tx in frm["txids"]:
    txinfo = cnxn.gettransaction(tx)
    print (txinfo)
    vout = None
    for d in txinfo["details"]:
      if d["address"] == frm["address"]:
        vout = d["vout"]
        break
    if not vout is None:
      inp.append({"txid":str(tx),"vout":vout})

  pdb.set_trace()

  #out = bitcoin.core.CMutableTxOut(frm["amount"],toAddr.to_scriptPubKey())
  out = { str(toAddr): str(frm["amount"]) }
  #txn = bitcoin.core.CMutableTransaction(inp,[out])
  txn = cnxn._call("createrawtransaction",inp, out)
  signedtxn = cnxn._call("signrawtransaction",str(txn))
  cnxn.sendrawtransaction(signedtxn)


def consolidate2(frm, toAddr, cnxn):
  pdb.set_trace()
  #out = bitcoin.core.CTxOut(frm["amount"],toAddr)
  #script = bitcoin.core.CScript()
  # bitcoin.wallet.CBitcoinAddress(toAddr)

  inp = []
  for tx in frm["txids"]:
    txinfo = cnxn.gettransaction(tx)
    print (txinfo)
    vout = None
    for d in txinfo["details"]:
      if d["address"] == frm["address"]:
        vout = d["vout"]
        break
    if not vout is None:
      inp.append(bitcoin.core.CMutableTxIn(bitcoin.core.COutPoint(tx, vout)))


  out = bitcoin.core.CMutableTxOut(frm["amount"],toAddr.to_scriptPubKey())
  txn = bitcoin.core.CMutableTransaction(inp,[out])
  cnxn.sendrawtransaction(txn)


# python txnTest.py nol split
#  645  python txnTest.py nol spam
#  653  python txnTest.py nol unspent
#  654  python txnTest.py nol join 100 1000

if __name__ == "__main__":
  idx = 1
  if len(sys.argv) > 1:
    if sys.argv[1] == "help":
      print("./txnTest.py <network> <operation> [operation specific arguments]")
      print('  network can be: "testnet", "regtest", "nol", "main"')
      print('  operation can be: "split", "join", "spam", "unspent", "info"')
      print("    split: create more UTXOs.")
      print("      parameters: [nSplits: takes every UTXO that has sufficient balance and splits it into this many more UTXOs, default 25]")
      print("      example: ./txnTest.py nol split 10")
      print("    join: consolidate UTXOs.")
      print("      parameters: <nJoin: take this many UTXOs and join them into 1>  <nRepeat: repeat the join this many times>")
      print("      example that joins 50 UTXOs into one output 2 times: ./txnTest.py nol join 50 2")
      print("    spam: generate a lot of transactions, by paying to myself.")
      print("      example: ./txnTest.py nol spam")
      sys.exit(1)
    if sys.argv[idx] == "testnet":
      bitcoin.SelectParams('testnet')
      idx+=1
    elif sys.argv[idx] == "regtest":
      bitcoin.SelectParams('regtest')
      idx+=1
    elif sys.argv[idx] == "nol":
      bitcoin.SelectParams('nol')
      idx+=1
    elif sys.argv[idx] == "main":
      bitcoin.SelectParams('mainnet')
      idx+=1
    else:
      print("Invalid network %s" % sys.argv[idx])
      sys.exit(-1)

  if len(sys.argv) > idx:
    op = sys.argv[idx]
  else: op = "info"
  main(op, sys.argv[idx+1:])

def Test():
  if 1:
      bitcoin.SelectParams('nol')
  main("spam")

