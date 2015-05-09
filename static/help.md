## Endpoints

There are three REST endopoints, corrensponding to the query of a block, address and a transaction, respectively.

###  ```/query/block?```

- `number`, `maxnumber`, `minnumber`
- `nonce`, `maxnonce`, `minnonce`
- `gas`, `maxgas`, `mingas`   
- `gaslim`, `maxgaslim`, `mingaslim`
- `gasused`, `maxgasused`, `mingasused`
- `diff`, `maxdiff`, `mindiff`
- `time`, `maxtime`, `mintime`
- `txaddress`
- `coinbase`
- `blockid`

### ```/query/address?```

- `balance`, `minbalance`, `maxbalance`  
- `nonce`, `minnonce`, `maxnonce` 
- `address` 
    
### ```/query/transaction?```
- `from`, `to`, `address`
- `blockid`
- `value`, `maxvalue`, `minvalue`
- `gasprice`, `maxgasprice`, `mingasprice`
- `gaslimit`, `maxgaslimit`, `mingaslimit`

## There are also fixed endpoints (to be deprecated)

Pages (where supported) start from 0 and are appended to the string in the end, like `/query/block?address=xxx/2`. This will eventually move into the general query string. They are optional.

- `/query/transaction/address/xxxxxxx` or `/query/transaction/address/xxxxxxx/0`
- `/query/account/address/xxxxxxx` or `/query/account/address/xxxxxxx/12`
- `/query/block/txaddress/xxxxxxxx` or `/query/block/txaddress/xxxxxxxx/2`


## Types of transactions

### FunctionCall

If `toAddress == Null` and `len(code) > 0` we have a `FunctionCall`.

### Contract

If `toAddress != Null` and `len(code) > 0` we have a `Contract`.

### Transaction

If `toAddress != Null` and `len(code) == 0` we have a `Transaction`.

### JustTheSig

If `toAddress == Null` and `len(code) == 0` we have a `JustTheSig`.