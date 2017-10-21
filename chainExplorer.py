#!/usr/bin/env python
import sys, pdb
import bitcoin
import bitcoin.rpc as btc
import time
import types
import datetime
import binascii

# REGTEST_PORT = 18332

bu = None

BTC = 100000000
mBTC = 100000
uBTC = 100

def generate(amt=1,cnxn=None):
  if cnxn is None: cnxn = bu
  cnxn._call("generate",amt)


def spamTx(bu, numTx,addrp,amt = 1*mBTC,gen=False):
  addr = addrp
  lastGenerate = -1
  for i in range(0, numTx):
    print "Count ", i,
    if addrp is None:
      addr = bu._call('getnewaddress')
      print " ", addr,
    elif type(addrp) is types.ListType:
      addr = addrp[i%len(addrp)]
      print " sent to: ", addr,
    try:
      bu.sendtoaddress(addr,amt)
    except btc.JSONRPCError as e:
      if gen and i > lastGenerate:  # Out of TxOuts in the wallet so commit these txn
        generate()
        print "\nGenerated at count %d.  Interval %d" % (i, i-lastGenerate)
        lastGenerate = i
      else:
        print "\n%d: Exception %s" % (i,str(e))
        raise
    finally:
      print
      pass


def printChainHeader(chain,blk):
  print "Status: %s, height: %d, length: %d, work: %s" % (chain["status"], chain["height"], chain["branchlen"],blk["chainwork"].strip("0") if blk else "NA")

def main(allchains=False,activeDepth=10):
  global bu
  bu = btc.Proxy(timeout=600) # service_port=18332)

  # list transactions
  list_txns = False

  chainTooOld = 10000
  # print bu.getbalance()

  chains = bu._call("getchaintips")
  # print chains
  highest = chains[0]["height"]

  for chain in chains:
   if highest - chainTooOld > chain["height"]:  # Don't show old forks
     continue
   if allchains or chain["status"]=="active":
    blkid = chain["hash"]
    blk=None
    try:
      blk = bu._call("getblock", blkid, True, list_txns)
    except btc.JSONRPCError, e:  # We don't have info about this block
      #print "  " + str(e)
      #continue  # Skip showing old chains
      pass
    printChainHeader(chain,blk)

    if chain["status"]=="active":
      depth = activeDepth
    else:
      depth = min(10,chain["branchlen"])
    for i in range(0,depth):
      print "Block -%d" % i
      try:
        # print blkid
        #blk = bu.getblock(blkid)
        blk = bu._call("getblock", blkid, True, list_txns)
      except btc.JSONRPCError, e:  # We don't have info about this block
        print ("No info on block %d " % i) + str(e)
        continue
      if blk["size"] > 1000000: pfx = "**  "
      else: pfx = "    "
      if list_txns :
        txns_count = len(blk["tx"])
        coinbaseHash = blk["tx"][0]
        try:
          rawcoinbase = bu._call("getrawtransaction", coinbaseHash)
          coinbase = bu._call("decoderawtransaction", rawcoinbase)
          data = binascii.unhexlify(coinbase["vin"][0]["coinbase"][8:])
        except Exception, e:
          data = str(e)
      else:
        txns_count = blk["txcount"]
      print "--- %sDate: %s Height: %6d Size: %8d  NumTx: %6d  Ver: %8x  Hash: %s " % (pfx,datetime.datetime.fromtimestamp(blk["time"]).strftime('%Y-%m-%d %H:%M:%S'),blk['height'],blk["size"],txns_count,blk["version"],blkid)
      if list_txns: print "MSG:", data
      try:
        blkid=blk["previousblockhash"]
      except KeyError, e:  # first block
        print "first block"
        break

if __name__ == "__main__":
  allchains=False
  if len(sys.argv) > 1:
    if sys.argv[1] == "help":
      print("chainExplorer.py <network> <length>")
      print('  network can be: "testnet", "regtest", "nol", "main"')
      print('  length is: numerical number of blocks to show, "all" to show all chain tips, "forever" to repeat every minute')
      print('  Example: ./chainExplorer.py nol 20')
      sys.exit(1)
    idx = 1
    if sys.argv[idx] == "testnet":
      bitcoin.SelectParams('testnet')
      idx+=1
    elif sys.argv[idx] == "regtest":
      bitcoin.SelectParams('regtest')
      idx+=1
    elif sys.argv[idx] == "nol":
      bitcoin.SelectParams('mainnet')
      bitcoin.params.DEFAULT_PORT = 9333
      bitcoin.params.RPC_PORT = 9332
      bitcoin.params.DNS_SEEDS = tuple()
      idx+=1
    elif sys.argv[idx] == "main":
      bitcoin.SelectParams('mainnet')
      idx+=1
    else:
      bitcoin.SelectParams('mainnet')

    if sys.argv[idx] == "all":
      print "Displaying all chains"
      main(True)
    elif sys.argv[idx] == "forever":
      while 1:
        print("\n" + time.strftime("%d/%m/%Y  %H:%M:%S"))
        main(True) 
        time.sleep(60)
    else:
      depth = int(sys.argv[idx])
      main(False,depth)
  else:
    main(allchains)

def Test():
  main(True)
