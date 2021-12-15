---
layout:     post
comments:   true
title:      "Clojure REPL Driven Development"
subtitle:   "Building a Toy Digital Bank with Emacs, Mount, Pedestal and Datomic - Part 2"
date:       2021-04-28
author:     "Promesante"
background: "/img/posts/14.jpg"
---

[Previous part]({% post_url 2021-04-28-clojure_repl_driven_development_part_1 %}) of this series left us ready to begin actual implementation of our toy digital bank, as an attempt to **get hands-on experience in Clojure REPL driven development (RDD)**, tackling the first endpoint in our implementation strategy: **account view**.

---
# Database #

Implemented in the following [PR](https://github.com/promesante/accounts-api/pull/2/files).

Assuming you have already read both tutorials on Datomic mentioned in the [previous part]({% post_url 2021-04-28-clojure_repl_driven_development_part_1 %}), section "References", this PR wouldn't demand any further explanation.

In that PR, the part that, in my opinion, does require explanation is setup related with REPL, exposed in the following section.

---
## Default Namespace on REPL Startup ##

The default namespace Clojure REPLs target on startup is `user`. So, a common practice in RDD is implementing it explicitly in `dev` environment, as to avoid including it with the rest of our application when we build and deploy it.

To do so, we:
1. set `dev` alias in `deps.edn` file, `:aliases` section, and within it, `:extra-paths ["dev"]`
2. create a `.dir-locals.el` Emacs project config file in the project's root directory with the following contents:

```elisp
((nil
  (cider-clojure-cli-global-options . "-A:dev")))
```

To do so, press `C-x d` to open Dired, navigate to the project's root directory and once there, press `C-x C-f` to create a file named `.dir-locals.el`, and copy the config just shown into it.

---
## State Management ##

State managed by this PR is:

* **Datomic connection**: handled in `src/accounts/db/conn.clj`

```clojure
(defstate conn :start (new-connection config)
               :stop (disconnect config conn))
```

* **Configs**: loaded from EDN files in `resources` directory in `src/accounts/conf.clj`

```clojure
(ns accounts.conf
  (:require [mount.core :as mount :refer [defstate]]
            [clojure.edn :as edn]))

(defn load-config [path]
  (-> path
      slurp
      edn/read-string))

(defstate config
  :start (load-config "resources/config.edn"))
```

Those operations, `start` and `stop`, are invoked precisely from the `user` namespace we have just been dealing with in the previous section:

```clojure
(ns user
  (:require [mount.core :as mount]
            [accounts.db.conn :as c]
            [accounts.db.queries :as q]))

(defn start []
  (mount/start))

(defn stop []
  (mount/stop))
```

Ahead in the implementation path, they will be invoked from `main` function in `accounts.accounts` namespace as well:

```clojure
(ns accounts.accounts
  (:require [mount.core :as mount])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "\nCreating your server...")
  (mount/start))
```

At this point, these are the only states declared and managed by Mount: a couple more will be added ahead in the implementation path.

---
## End-to-end Testing ##

In order to get the branch of this PR running, check it out:

```shell
$:(main) git checkout account-detail-db
Switched to branch 'account-detail-db'
$:(account-detail-db) 
```

Then, in Emacs, from any Clojure source file, type `C-c M-j` and REPL should be started up inside Emacs. With `C-x o` you can switch from source code buffer (or window, in Emacs lingo) to the REPL one, back and forth; from any buffer rendering a Clojure source code file, we can go straight to REPL typing `C-c C-z`. The REPL loads the `user` namespace as explained above and, in that context, we:
1. startup our API by means of mount state manager
2. invoke `load-database` function in order to load the database's schema and run the migrations to load data into the database bound to that schema
3. query the account created and updated by the migrations
4. stop the API again by means of mount

These tasks are shown below in the context of an actual REPL session:

```clojure
user> (start)
{:started ["#'accounts.conf/config" "#'accounts.db.conn/conn"]}
user> (c/load-database)
{:db-before datomic.db.Db@4eac34a8,
 :db-after datomic.db.Db@72d65ed1,
 :tx-data
 [#datom[13194139534317 50 #inst "2021-05-03T22:29:57.745-00:00" 13194139534317 true] #datom[17592186045418 64 10000.0 13194139534317 true] #datom[17592186045418 64 0.0 13194139534317 false] #datom[17592186045419 64 20000.0 13194139534317 true] #datom[17592186045419 64 0.0 13194139534317 false] #datom[17592186045420 64 30000.0 13194139534317 true] #datom[17592186045420 64 0.0 13194139534317 false]],
 :tempids {}}
user> (q/pull-account-by-id "account-1")
#:account{:id "account-1", :balance 10000.0}
user> (stop)
{:stopped ["#'accounts.db.conn/conn"]}
user>
```
---
# Web #

Coded in the following PRs, involving almost exclusively Pedestal interceptors implementation:
1. [validate interceptor set](https://github.com/promesante/accounts-api/pull/2/files)
2. [display interceptor set](https://github.com/promesante/accounts-api/pull/4/files)
3. [e2e testing](https://github.com/promesante/accounts-api/pull/5/files)

As suggested in Pedestal documentation, we embraced interceptors as much as possible, and organized them as shown below:

1. **validate** HTTP `request` parameters
2. **retrieve** data from database
3. **update** data into database
2. **prepare** (**retrieve** or **update**) data for each of the corresponding operations just mentioned
4. **display** data in `response` as the result of the interceptor chain execution

To handle data, making it flow step by step along the interceptor chain bound to every endpoint, we have to device a data structure to store data in, or take it from, the following way:

1. `:request`: this is the data which comes with the HTTP `GET request`, that is bound to this key in the interceptor chain's Pedestal context
2. `:query-data`: `prepare-retrieve` interceptors bind data to this key, leaving it prepared for `retrieve` interceptors
3. `:retrieved`: `retrieve` interceptors store retrieved data here
4. `:result`: `display` interceptors store data here in order to have it ready for the `entity-render` interceptor to set it in `response`

The following is an example of this data structure:

```clojure
  {:request {:path-params {:account-id "account-1"}}
   :query-data {:debit {:id "account-1"}}
   :retrieved {:accounts {:report #:account{:id "account-1", :balance 10000.0}}}
   :result {#:account{:id "account-1", :balance 10000.0}}}
```

This data structure might be built by the end of the execution of the interceptor chain bound to this endpoint: **account view**.

---
## End-to-end Testing ##

Let's switch to the branch corresponding to the [last PR](https://github.com/promesante/accounts-api/pull/5) for this endpoint: `account-detail-e2e-testing`.

This e2e testing session will begin and end in exactly the same way as our previous one on the last interceptor set, branch `account-detail-db`.

Functionality added in this interceptor set can be tested with:
1. `response-for` function from `io.pedestal.test` namespace, wrapped in our own util function, `test-request`
2. in order to type a bit less in this task, that we will have to run quite repetitively, we can wrap it in our own function, `account-view`, in our `user` namespace, which has basically that purpose: holding development utils.

```clojure
accounts.web.interceptors.validate-test> 
user> (start)
{:started
 ["#'accounts.conf/config"
  "#'accounts.db.conn/conn"
  "#'accounts.web.server/server"]}
user> (c/load-database)
{:db-before datomic.db.Db@8cbce164,
 :db-after datomic.db.Db@c4fe9953,
 :tx-data
 [#datom[13194139534317 50 #inst "2021-05-05T09:11:57.819-00:00" 13194139534317 true] #datom[17592186045418 64 10000.0 13194139534317 true] #datom[17592186045418 64 0.0 13194139534317 false] #datom[17592186045419 64 20000.0 13194139534317 true] #datom[17592186045419 64 0.0 13194139534317 false] #datom[17592186045420 64 30000.0 13194139534317 true] #datom[17592186045420 64 0.0 13194139534317 false]],
 :tempids {}}
user> (test-request :get "/accounts/account-1")
{:status 200,
 :body "{\"account/id\":\"account-1\",\"account/balance\":10000.0}",
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
user> (account-view)
{:status 200,
 :body "{\"account/id\":\"account-1\",\"account/balance\":10000.0}",
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

Hence, we are now ready to go on with the **REPL-driven Development (RDD) Session Demo**.

So, let's jump to [next part]({% post_url 2021-04-28-clojure_repl_driven_development_part_3 %})  !
