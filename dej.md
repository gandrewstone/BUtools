# dej

dej is short for de-json.  It picks and converts json text, outputting individual lines for additional processing.

## Usage:
```
dej [selector...] [json]
dej [selector... ] < json.txt
./jsonProducer | dej [selector...]
```

*dej* accepts a JSON document in the standard input or in the optional last command line 
argument, **json**.  If no **selectors** are specified, *dej* assumes the JSON is in a list format and outputs each list item on separate lines.

If **selectors** are specified, *dej* executes each selector outputing each produced 
element on its own line.

**Selectors** are Python code, and the json data is accessed via the variable "j".
For example: `j[0]`, `j['foo']`, `j['bar'][0:3]`
There are a few shortcuts:

To access the nth element of the top-level list, you may simply specify the number.
That is, `dej 1 5` is equivalent to `dej "j[1]" "j[5]"`.

To access the nth element of the top-level dictionary, you may just specify the key.
Therefore `dej foo` is equivalent to `dej "j['foo']"`.


## Examples:

```bash
Basic selection:
$ echo "[1,2,3]" | dej "[j[0],j[2]]" "j[1]"
$ dej "[j[0],j[2]]" "j[1]" "[1,2,3]"
$ echo "[1,2,3]" | dej 0 2 1
1
3
2
```

Repeatedly apply a command to each selected item:
```bash
$ echo "[1,2,3]" | dej | xargs -n1 echo "you said"
you said 1
you said 2
you said 3
```

Select a few items and then pass them as args to a command:
```bash
$ echo "{\"place\":\"world\",\"salu\":\"hello\"}" | dej "[j['salu'],j['place']]" | xargs echo
$ echo "{"place":"world","salu":"hello"}" | dej salu place | xargs echo
hello world
```

## Bitcoin Examples:

Decode every transaction in a block:
```bash
./bitcoin-cli getblock 297 | dej "j['tx']" | xargs -n1 ./bitcoin-cli getrawtransaction | xargs -n1 ./bitcoin-cli decoderawtransaction
```

Decode just the coinbase:
```bash
./bitcoin-cli getblock 297 | dej "j['tx'][0]" | xargs -n1 ./bitcoin-cli getrawtransaction | xargs -n1 ./bitcoin-cli decoderawtransaction
```

Abandon all transactions in your mempool (if your bitcoind supports abandoning mempool transactions):
```bash
./bitcoin-cli getrawmempool | dej | xargs -n1 ./bitcoin-cli abandontransaction
```

Invalidate the block AFTER the one you know is good:
```bash
./bitcoin-cli invalidateblock `./bitcoin-cli getblock 200 | dej nextblockhash`
```
