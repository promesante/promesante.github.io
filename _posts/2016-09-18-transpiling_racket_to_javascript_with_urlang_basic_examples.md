---
layout:     post
comments:   true
title:      "Transpiling Racket to Javascript with Urlang"
subtitle:   "Basic Examples"
date:       2016-09-18
author:     "Promesante"
background: "/img/posts/05.jpg"
---
# Basic Examples

As a first step to learn [Urlang](https://github.com/soegaard/urlang), I've coded my own basic [examples](https://github.com/promesante/urlang-examples/), based on the most basic one supplied with Urlang's distribution: [factorial](https://github.com/soegaard/urlang/blob/master/urlang-examples/demo-fact.rkt):

* [sum](https://github.com/promesante/urlang-examples/blob/master/demo-sum.rkt)
* [fibonacci](https://github.com/promesante/urlang-examples/blob/master/demo-fibo.rkt)

Generated Javascript code can be run the following ways:

* **in browser**, by means of the [index.html](https://github.com/promesante/urlang-examples/blob/master/index.html) page (check log in browser dev tool console)
* **from the command line interface**, by means of [driver.js](https://github.com/promesante/urlang-examples/blob/master/driver.js) script

In the latter case, you can run the script by means of the Node.js, as shown below:

```
> node driver.js
```

My next step here will be similar, but based on another, slightly more sophisticated example, among the supplied with Urlang; most likely, [the one](https://github.com/soegaard/urlang/tree/master/urlang-examples/ractive) interoperating with [Ractive](http://www.ractivejs.org/).

Till then !
