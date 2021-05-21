---
layout:     post
comments:   true
title:      "Clojure REPL Driven Development"
subtitle:   "Building a Toy Digital Bank with Emacs, Mount, Pedestal and Datomic - Part 1"
date:       2021-04-28
author:     "Promesante"
background: "/img/posts/10.jpg"
---

Clojure, as programming language, has notably powerful features.

However, they are far from a random list, but make a very powerful and cohesive set, due to the [synergy](https://en.wikipedia.org/wiki/Synergy) between them: "interaction or cooperation giving rise to a whole that is greater than the simple sum of its parts".

There are plenty of resources available in the Web exposing them, i.e.:
* [Clojure Rationale](https://clojure.org/about/rationale), and the additional explanation given there on echa particular feature
* [Why Developers Should Look into Clojure](https://auth0.com/blog/why-developers-should-look-into-clojure/)
* [Why Clojure? I’ll tell you why](https://medium.com/@ertu.ctn/why-clojure-seriously-why-9f5e6f24dc29#:~:text=The%20creator%20of%20the%20language,Clojure%20code%2C%20or%20vice%20versa.)

Among them, we will take as reference [Clojure Rationale](https://clojure.org/about/rationale), which enumerates:
* [Dynamic Development: the REPL](https://clojure.org/about/dynamic), aka REPL-driven Development, or **RDD**
* [Functional Programming](https://clojure.org/about/functional_programming)
* [Clojure as a Dialect of Lisp](https://clojure.org/about/lisp)
* [Runtime Polymorphism](https://clojure.org/about/runtime_polymorphism)
* [Concurrent Programming](https://clojure.org/about/concurrent_programming)
* [Hosted on the JVM](https://clojure.org/about/jvm_hosted)

The best way to realize the power of this synergy is actually putting those features into practice. And precisely that is actually the purpose of this post series:

**Getting first hand experience in Clojure REPL driven development (RDD)**

---
# Reloaded Workflow #

Regarding RDD, one problem of Clojure is the time demanded for the REPL to get started.

It is due partially to time demanded by the JVM, but partially to Clojure itself as well. This time, and just having to restart the REPL, is incompatible with the "flow" you get working with RDD.

Therefore, several "reloaded workflows" have sprung out in Clojure ecosystem in order to fix this situation, giving us the chance to restart just the application under development, instead of the REPL. They use to complement "state managers", meaning by "state" how the components required by our application are loaded. The "classic" on these subjects is Stuart Sierra's [Clojure Workflow Reloaded](https://cognitect.com/blog/2013/06/04/clojure-workflow-reloaded).

The most widely adopted state managers / reloaded workflows in Clojure ecosystem are:
* [component](https://github.com/stuartsierra/component) / [reloaded.repl](https://github.com/weavejester/reloaded.repl)
* [integrant](https://github.com/weavejester/integrant) / [integrant-rep](https://github.com/weavejester/integrant-repl)
* [mount](https://github.com/tolitius/mount)

One of the best articles I happened to read on RDD, and on reloaded workflows in particular, is the [Guide to the Duct Framework](https://github.com/duct-framework/docs/blob/master/GUIDE.rst), which shows a demo of a reloaded workflow in a strictly practical fashion, building a REST API from the ground up, and showing step-by-step the whole path. This approach is particularly illuminating for getting a first hand experience in RDD.

A project I've worked on was based on [Luminus](https://luminusweb.com/), a well known project template in the Clojure ecosystem, which tidily packages and integrates a vast set of tools widely adopted in the Clojure ecosystem. As its state manager, Luminus chose mount, and from then on I have adopted it as well.

The best tutorial explaining an example project on mount I have been able to find out so far is the official [tutorial](https://github.com/tolitius/mount#mount-and-develop) and [example](https://github.com/tolitius/mount/tree/master/dev/clj/app) supplied with it. 

In order to get a tutorial like that Duct's Guide, but for a project based on mount instead of integrant, I have built an API similar to the one shown in mount's tutorial and example: the [accounts API of a toy digital bank](https://github.com/promesante/accounts-api). And this post will show how its implementation evolved, step by step, in a typical RDD way.

Instead of explaining each of those steps here, in the post series, we have kept track of them by means of git branches and [pull requests](https://github.com/promesante/accounts-api/pulls?q=is%3Apr+is%3Aclosed), and then depict here just what, in our opinion, wouldn't be clear enough by just taking a look at each of those PRs.

In contrast with that mount example, this post:
* is based on [Deps and CLI Tools](https://clojure.org/guides/deps_and_cli) instead of [Leiningen](https://leiningen.org/)
* deals with a particular IDE, [Emacs](https://www.gnu.org/software/emacs/), because one of its most outstanding features is its natural integration with any Lisp dialect's REPL, what is in turn a basic premise of the main subject of this article: RDD
* adopts as Web framework [Pedestal](http://pedestal.io/) instead of [Compojure](https://github.com/weavejester/compojure)

---
# Endpoints #

We will implement de following ones:

1. **account view**, `GET /accounts/:account-id`: details of the single account with the `account-id` set
2. **transaction list**, `GET /accounts/:account-id/transactions`: list of transactions already performed on the account with the `account-id` set
3. **transaction create**, `POST /accounts/:account-id`: creating (executing) a transaction on the account with the `account-id` set;

We will implement de following transactions:

1. **deposit**: positive `amount`, no `account` (attribute exclusive for transfers)
1. **withdrawal**: negative `amount`
1. **transfer**: negative `amount`, setting target account's id in `account`

Examples:

Deposit:

```json
{
  "amount": 1000.00,
  "description": "appartment rent - march 2021"
}
```

Withdrawal:

```json
{
  "amount": -1000.00,
  "description": "appartment rent - march 2021"
}
```

Transfer:

```json
{
  "amount": -1000.00,
  "account": "account-1",
  "description": "appartment rent - march 2021"
}
```

---
# Application Structure #

Each of those endpoints will be implemented with the following two modules:
1. `db`: Datomic database management
2. `web`: REST API; mainly, Pedestal interceptors

---
# Implementation Strategy and Post Series Structure #

The aspects exposed in the previous two sections, **Endpoints**, and **Application Structure**, will determine this post series' structure, as well as the implementation path exposed below, along [pull request](https://github.com/promesante/accounts-api/pulls?q=is%3Apr+is%3Aclosed) sequence, in the following iterative approch:

1. **account view**: [part 2]({% post_url 2021-04-28-clojure_repl_driven_development_part_2 %}) of this series
    1. database
	2. web
	3. end-to-end testing
	4. **RDD session demo**: [part 3]({% post_url 2021-04-28-clojure_repl_driven_development_part_3 %})
2. **transaction list**: [part 4]({% post_url 2021-04-28-clojure_repl_driven_development_part_4 %})
    1. database
	2. web
	3. end-to-end testing
3. **transaction create**: [part 5]({% post_url 2021-04-28-clojure_repl_driven_development_part_5 %})
    1. database
	2. web
	3. end-to-end testing

Among these parts of the series, the most **important** one is [part 3]({% post_url 2021-04-28-clojure_repl_driven_development_part_3 %}), **1.4, RDD session demo**, as it actually fulfills the most the whole series goal: **getting first hand experience in Clojure REPL driven development (RDD)**.

But before beginning those steps, let's tackle initial project setup which, due to its very own nature, is not reflected in any PR.

---
# Initial Project Setup #

We will use [clj-new](https://github.com/seancorfield/clj-new).

We need to add the following alias inside your `:aliases` map in  `~/.clojure/deps.edn`:

```clojure
    ;; add this inside your :aliases map:
    :new {:extra-deps {com.github.seancorfield/clj-new
                         {:mvn/version "1.1.297"}}
            :exec-fn clj-new/create
            :exec-args {:template "app"}}
```

Then, let's create the project:


```shell
$ clojure -X:new :name accounts/accounts
```

We are thus ready to begin coding.

---
# References #

We will take as reference the following articles. Each of them has been an excellent tutorial for me, for its corresponding tool below. So, if you don't have experience in any of them, I'd suggest to read them before going on with this series, as it assumes that level of understanding about each:

* **Emacs Setup**: [My Optimal GNU Emacs Settings for Developing Clojure (Revised)](http://fgiasson.com/blog/index.php/2016/06/14/my-optimal-gnu-emacs-settings-for-developing-clojure-revised/)
* **Mount**: [project README](https://github.com/tolitius/mount#mount-and-develop)
* **Pedestal**:
    * [Your First API](http://pedestal.io/guides/your-first-api)
	* [Unit testing](http://pedestal.io/reference/unit-testing)
* **Datomic**: 
    * [Datomic Official Tutorial](https://docs.datomic.com/on-prem/tutorial/introduction.html)
	* [Datomic Missing Link Tutorial](https://github.com/ftravers/datomic-tutorial)

---
# Next Step #

Hence, we are now ready to begin coding !

Come, join us to  [next part]({% post_url 2021-04-28-clojure_repl_driven_development_part_2 %})  , where we will implement **account view** endpoint !

Till then !
