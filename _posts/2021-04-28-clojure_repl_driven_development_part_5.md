---
layout:     post
comments:   true
title:      "Clojure REPL Driven Development"
subtitle:   "Building a Toy Digital Bank with Emacs, Mount, Pedestal and Datomic - Part 5"
date:       2021-04-28
author:     "Promesante"
background: "/img/posts/11.jpg"
---

Going on implementing our toy digital bank, as an attempt to get first hand experience in Clojure REPL driven development (RDD), we will now tackle its last endpoint: **transaction create**.

It is handled by means of an HTTP POST. We deviced its JSON body structure according to transaction type. In all of them, amount must be a `double`.

1. **deposit**: positive `amount`, no `account` (attribute exclusive for transfers)
1. **withdrawal**: negative `amount`
1. **transfer**: negative `amount`, setting target account's id in `account`

Examples:

**Deposit**

```json
{
  "amount": 1000.00,
  "description": "appartment rent - march 2021"
}
```

**Withdrawal**

```json
{
  "amount": -1000.00,
  "description": "appartment rent - march 2021"
}
```

**Transfer**

```json
{
  "amount": -1000.00,
  "account": "account-1",
  "description": "appartment rent - march 2021"
}
```
Although in this body there is no explicit indication on transation type, each of them can be distinguished by the following criteria:
* if `:account` attribute is present, transaction is a transfer; otherwise, it is a deposit or withdrawal
* single difference between deposit and withdrawal takes place just in ist amount sign: positive for deposits, and viceversa

Transfer transactions are basically splitted into a deposit into the target account, and a withdrawal from the source one.

To actually handle each of these transactions, several interceptors has two versions, carrying the following sufixes, usually delegating its actual implementation into a common function, with `type` as the identifying parameter:
* **credit**: when money is put into an account
* **debit**: when it is taken from it

---
# Web #

We will first address the data structure deviced for data handling along interceptor chain execution for this endpoint. And then, we will explain those interceptors.

---
## Data Structure ##

The only key new in this endpoint is `:tx-data`: `prepare-update` interceptors leave data there, "prepared" for `update` interceptors to actually run the corresponding Datomic transaction:

```clojure
  {:request
   {:path-params {:account-id "account-1"}
    :json-params {:amount 1000.0 :description "test"}}
   :query-data {:debit {:id "account-1"}}
   :retrieved {:accounts {:credit #:account{:id "account-1", :balance 10000.0}}}
   :tx-data
   {:credit
    {:id "account-1"
     :new-balance 11000.0
     :tx {:amount 1000.0 :description "test" :balance 11000.0}}}
   :result {:amount 1000.0 :description "test" :balance 11000.0}}
```
---
## Interceptors ##

We will now explain interceptors bound to this endpoint. As it has associated  much more than the other endpoints, we will list and briefly describe them below.

---
### Validation ###

We may group them the follwing way:

* [validating HTTP request params](https://github.com/promesante/accounts-api/pull/11/files): as this endpoint is the first HTTP POST, this validation interceptors target its request JSON body
* [retrieving accounts for its validation](https://github.com/promesante/accounts-api/pull/12)
* [validating accounts](https://github.com/promesante/accounts-api/pull/13): 
    * whether they are actually available
	* in the case of executing a debit transaction against it, whether it has sufficient funds

---
### Update Preparation ###

In these interceptors, data is prepared to be handled in the following interceptor set, `update`, in which they will be transacted against our Datomic database. Then, it will be left in data structure's `:tx-data` field.

These interceptors prepare the following entities:

* account's **new balance**
* **new transaction** is deviced

---
### Update ###

Each of these two entities is taken from data structures's `:tx-data` entry, and then transacted against our Datomic database.

---
### Display ###

Transaction just created is left in data structures's `:result` in order to have it ready for the `entity-render` interceptor to set it in `response`.

---
## End-to-end Testing ##

We have just finished the project !

In order to perform the final e2e testing, let's switch to `main` branch.

We will:
2. **deposit** U$S 2,000 into `account-1`
3. **withdraw** U$S 1,000 from it
4. **transfer** U$S 1,000 from `account-1` to `account-2`

Immediately before and after each of these transactions, we query database on each account involved in the transaction as well as that account's transaction log.


```clojure
user> (start)
{:started
 ["#'accounts.conf/config"
  "#'accounts.db.conn/conn"
  "#'accounts.web.server/server"]}
user> (c/load-database)
{:db-before datomic.db.Db@ae9a6f92,
 :db-after datomic.db.Db@a1ccb95e,
 :tx-data
 [#datom[13194139534332 50 #inst "2021-05-07T10:03:22.467-00:00" 13194139534332 true] #datom[17592186045418 64 10000.0 13194139534332 true] #datom[17592186045418 64 9000.0 13194139534332 false] #datom[17592186045419 64 20000.0 13194139534332 true] #datom[17592186045419 64 19000.0 13194139534332 false] #datom[17592186045420 64 26000.0 13194139534332 true] #datom[17592186045420 64 28000.0 13194139534332 false]],
 :tempids {}}
user> (q/pull-account-by-id "account-1")
#:account{:id "account-1", :balance 10000.0}
user> (q/pull-transactions-by-account-id "account-1")
({:db/id 17592186045435,
  :transaction/id "trx-10",
  :transaction/amount 1000.0,
  :transaction/description "thomas' present",
  :transaction/transfer-account-id #:db{:id 17592186045419},
  :transaction/balance 10000.0}
 {:db/id 17592186045427,
  :transaction/id "trx-4",
  :transaction/amount -1000.0,
  :transaction/description "appartment rent - febr 2021",
  :transaction/balance 9000.0}
 {:db/id 17592186045422,
  :transaction/id "trx-1",
  :transaction/amount 10000.0,
  :transaction/description "first deposit",
  :transaction/balance 10000.0})
user> (deposit-1)
{:status 200,
 :body
 "{\"amount\":2000.0,\"description\":\"second deposit\",\"balance\":12000.0}",
 :headers
 {"Strict-Transport-Security" "max-age=31536000; includeSubdomains",
  "X-Frame-Options" "DENY",
  "X-Content-Type-Options" "nosniff",
  "X-XSS-Protection" "1; mode=block",
  "X-Download-Options" "noopen",
  "X-Permitted-Cross-Domain-Policies" "none",
  "Content-Security-Policy"
  "object-src 'none'; script-src 'unsafe-inline' 'unsafe-eval' 'strict-dynamic' https: http:;",
  "Content-Type" "application/json;charset=UTF-8"}}
user> (q/pull-account-by-id "account-1")
#:account{:id "account-1", :balance 12000.0}
user> (q/pull-transactions-by-account-id "account-1")
({:db/id 17592186045439,
  :transaction/id "trx-930809",
  :transaction/amount 2000.0,
  :transaction/description "second deposit",
  :transaction/balance 12000.0}
 {:db/id 17592186045435,
  :transaction/id "trx-10",
  :transaction/amount 1000.0,
  :transaction/description "thomas' present",
  :transaction/transfer-account-id #:db{:id 17592186045419},
  :transaction/balance 10000.0}
 {:db/id 17592186045427,
  :transaction/id "trx-4",
  :transaction/amount -1000.0,
  :transaction/description "appartment rent - febr 2021",
  :transaction/balance 9000.0}
 {:db/id 17592186045422,
  :transaction/id "trx-1",
  :transaction/amount 10000.0,
  :transaction/description "first deposit",
  :transaction/balance 10000.0})
user> (withdrawal)
{:status 200,
 :body
 "{\"amount\":-1000.0,\"description\":\"appartment rent - march 2021\",\"balance\":11000.0}",
 :headers
 {"Strict-Transport-Security" "max-age=31536000; includeSubdomains",
  "X-Frame-Options" "DENY",
  "X-Content-Type-Options" "nosniff",
  "X-XSS-Protection" "1; mode=block",
  "X-Download-Options" "noopen",
  "X-Permitted-Cross-Domain-Policies" "none",
  "Content-Security-Policy"
  "object-src 'none'; script-src 'unsafe-inline' 'unsafe-eval' 'strict-dynamic' https: http:;",
  "Content-Type" "application/json;charset=UTF-8"}}
user> (q/pull-account-by-id "account-1")
#:account{:id "account-1", :balance 11000.0}
user> (q/pull-transactions-by-account-id "account-1")
({:db/id 17592186045442,
  :transaction/id "trx-930817",
  :transaction/amount -1000.0,
  :transaction/description "appartment rent - march 2021",
  :transaction/balance 11000.0}
 {:db/id 17592186045439,
  :transaction/id "trx-930809",
  :transaction/amount 2000.0,
  :transaction/description "second deposit",
  :transaction/balance 12000.0}
 {:db/id 17592186045435,
  :transaction/id "trx-10",
  :transaction/amount 1000.0,
  :transaction/description "thomas' present",
  :transaction/transfer-account-id #:db{:id 17592186045419},
  :transaction/balance 10000.0}
 {:db/id 17592186045427,
  :transaction/id "trx-4",
  :transaction/amount -1000.0,
  :transaction/description "appartment rent - febr 2021",
  :transaction/balance 9000.0}
 {:db/id 17592186045422,
  :transaction/id "trx-1",
  :transaction/amount 10000.0,
  :transaction/description "first deposit",
  :transaction/balance 10000.0})
user> (q/pull-account-by-id "account-2")
#:account{:id "account-2", :balance 20000.0}
user> (q/pull-transactions-by-account-id "account-2")
({:db/id 17592186045434,
  :transaction/id "trx-9",
  :transaction/amount -1000.0,
  :transaction/description "thomas' present",
  :transaction/transfer-account-id #:db{:id 17592186045418},
  :transaction/balance 20000.0}
 {:db/id 17592186045433,
  :transaction/id "trx-8",
  :transaction/amount 2000.0,
  :transaction/description "peter's present",
  :transaction/transfer-account-id #:db{:id 17592186045420},
  :transaction/balance 26000.0}
 {:db/id 17592186045428,
  :transaction/id "trx-5",
  :transaction/amount -1000.0,
  :transaction/description "credit card - febr 2021",
  :transaction/balance 19000.0}
 {:db/id 17592186045423,
  :transaction/id "trx-2",
  :transaction/amount 20000.0,
  :transaction/description "first deposit",
  :transaction/balance 20000.0})
user> (transfer)
{:status 200,
 :body
 "{\"amount\":-1000.0,\"description\":\"anne's present\",\"account-id\":\"account-2\",\"balance\":10000.0}",
 :headers
 {"Strict-Transport-Security" "max-age=31536000; includeSubdomains",
  "X-Frame-Options" "DENY",
  "X-Content-Type-Options" "nosniff",
  "X-XSS-Protection" "1; mode=block",
  "X-Download-Options" "noopen",
  "X-Permitted-Cross-Domain-Policies" "none",
  "Content-Security-Policy"
  "object-src 'none'; script-src 'unsafe-inline' 'unsafe-eval' 'strict-dynamic' https: http:;",
  "Content-Type" "application/json;charset=UTF-8"}}
user> (q/pull-account-by-id "account-1")
#:account{:id "account-1", :balance 10000.0}
user> (q/pull-transactions-by-account-id "account-1")
({:db/id 17592186045446,
  :transaction/id "trx-930830",
  :transaction/amount -1000.0,
  :transaction/description "anne's present",
  :transaction/balance 10000.0}
 {:db/id 17592186045442,
  :transaction/id "trx-930817",
  :transaction/amount -1000.0,
  :transaction/description "appartment rent - march 2021",
  :transaction/balance 11000.0}
 {:db/id 17592186045439,
  :transaction/id "trx-930809",
  :transaction/amount 2000.0,
  :transaction/description "second deposit",
  :transaction/balance 12000.0}
 {:db/id 17592186045435,
  :transaction/id "trx-10",
  :transaction/amount 1000.0,
  :transaction/description "thomas' present",
  :transaction/transfer-account-id #:db{:id 17592186045419},
  :transaction/balance 10000.0}
 {:db/id 17592186045427,
  :transaction/id "trx-4",
  :transaction/amount -1000.0,
  :transaction/description "appartment rent - febr 2021",
  :transaction/balance 9000.0}
 {:db/id 17592186045422,
  :transaction/id "trx-1",
  :transaction/amount 10000.0,
  :transaction/description "first deposit",
  :transaction/balance 10000.0})
user> (q/pull-account-by-id "account-2")
#:account{:id "account-2", :balance 21000.0}
user> (q/pull-transactions-by-account-id "account-2")
({:db/id 17592186045448,
  :transaction/id "trx-930831",
  :transaction/amount 1000.0,
  :transaction/description "anne's present",
  :transaction/balance 21000.0}
 {:db/id 17592186045434,
  :transaction/id "trx-9",
  :transaction/amount -1000.0,
  :transaction/description "thomas' present",
  :transaction/transfer-account-id #:db{:id 17592186045418},
  :transaction/balance 20000.0}
 {:db/id 17592186045433,
  :transaction/id "trx-8",
  :transaction/amount 2000.0,
  :transaction/description "peter's present",
  :transaction/transfer-account-id #:db{:id 17592186045420},
  :transaction/balance 26000.0}
 {:db/id 17592186045428,
  :transaction/id "trx-5",
  :transaction/amount -1000.0,
  :transaction/description "credit card - febr 2021",
  :transaction/balance 19000.0}
 {:db/id 17592186045423,
  :transaction/id "trx-2",
  :transaction/amount 20000.0,
  :transaction/description "first deposit",
  :transaction/balance 20000.0})
user> 
```
---
# Next Steps #

Source code available in my [repo](https://github.com/promesante/accounts-api), at GitHub

As I am now studying Kubernetes, Kafka and Istio, next post will likely go on working with this restful API in the context of those tools.

As I have been studying Haskell for the last couple years, mainly with the ["Haskell Book"](https://haskellbook.com/), I'd like to implement other APIs, first with [Scotty](https://github.com/scotty-web/scotty), which is the Web framewok suggested in that book as a first step for API development, and covered extensively there. And then, perhaps, go on wih [Servant](https://docs.servant.dev/en/stable/)

Not sure yet which of these alternatives will actually be our next step. First option would definitely be much more convenient for my professional career in the short term. On the other hand, Haskell have meant a deep and revealing enlightment I am eager to adopt in practice. In Technology, short term career is always the most "sensible" reason to keep postponing actual technology marvels...

So… stay tuned !
