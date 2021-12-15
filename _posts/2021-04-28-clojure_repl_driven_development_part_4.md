---
layout:     post
comments:   true
title:      "Clojure REPL Driven Development"
subtitle:   "Building a Toy Digital Bank with Emacs, Mount, Pedestal and Datomic - Part 4"
date:       2021-04-28
author:     "Promesante"
background: "/img/posts/18.jpg"
---

Going on implementing our toy digital bank, as an attempt to get hands-on experience in Clojure REPL driven development (RDD), we will now tackle its next endpoint in our implementation strategy: **transaction list**.

It is covered in the following PRs:

* [Database](https://github.com/promesante/accounts-api/pull/6/files)
* **Web** (interceptors):
    * [validate interceptor](https://github.com/promesante/accounts-api/pull/7/files)
	* [display interceptor](https://github.com/promesante/accounts-api/pull/8/files)
	* [end-to-end testing](https://github.com/promesante/accounts-api/pull/9/files)

It is quite similar to the previous one: **account view**. The only relevant differences are some details in the data structure we device to handle data along each endpoint's interceptor chain and introduced in [part 2]({% post_url 2021-04-28-clojure_repl_driven_development_part_2 %}) of this series.

---
# Interceptors #

Regarding interceptor data structure depicted in [part 2]({% post_url 2021-04-28-clojure_repl_driven_development_part_2 %}), the following one is an example, built by this endpoint's interceptor chain. It is basically the same as the one shown in [part 2]({% post_url 2021-04-28-clojure_repl_driven_development_part_2 %}). The only difference is the key inside `:retrieved` data got from the database is bound to:` :txs`.

```clojure
  {:request {:path-params {:account-id "account-1"}}
   :query-data {:report {:id "account-1"}}
   :retrieved
   {:txs
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
      :transaction/balance 10000.0})}
   :result
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
      :transaction/balance 10000.0})}
```

---
# End-to-end Testing #

Let's switch to the branch corresponding to the [last PR](https://github.com/promesante/accounts-api/pull/9) for this endpoint: `transaction-list-e2e-testing`. We show below the session for e2e testing for this endpoint:
1. running the Datomic query
2. invoking the endpoint, by means of `response-for` function from `io.pedestal.test` namespace, wrapped in our own util function, `test-request` and then wrapped in turn in `transaction-list`

It is basically the same as for the previous endpoint.

```clojure
user> (start)
{:started
 ["#'accounts.conf/config"
  "#'accounts.db.conn/conn"
  "#'accounts.web.server/server"]}
user> (c/load-database)
{:db-before datomic.db.Db@46f9a83b,
 :db-after datomic.db.Db@7403a3ef,
 :tx-data
 [#datom[13194139534332 50 #inst "2021-05-06T09:22:22.918-00:00" 13194139534332 true] #datom[17592186045418 64 10000.0 13194139534332 true] #datom[17592186045418 64 9000.0 13194139534332 false] #datom[17592186045419 64 20000.0 13194139534332 true] #datom[17592186045419 64 19000.0 13194139534332 false] #datom[17592186045420 64 26000.0 13194139534332 true] #datom[17592186045420 64 28000.0 13194139534332 false]],
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
user> (transaction-list)
{:status 200,
 :body
 "[{\"db/id\":17592186045435,\"transaction/id\":\"trx-10\",\"transaction/amount\":1000.0,\"transaction/description\":\"thomas' present\",\"transaction/transfer-account-id\":{\"db/id\":17592186045419},\"transaction/balance\":10000.0},{\"db/id\":17592186045427,\"transaction/id\":\"trx-4\",\"transaction/amount\":-1000.0,\"transaction/description\":\"appartment rent - febr 2021\",\"transaction/balance\":9000.0},{\"db/id\":17592186045422,\"transaction/id\":\"trx-1\",\"transaction/amount\":10000.0,\"transaction/description\":\"first deposit\",\"transaction/balance\":10000.0}]",
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
user> (stop)
{:stopped ["#'accounts.web.server/server" "#'accounts.db.conn/conn"]}
user> 
```

---
# Next Step #

Hence, we are now ready to go on with next endpoint: **transaction create**.

So, let's jump to [next part]({% post_url 2021-04-28-clojure_repl_driven_development_part_5 %})  !
