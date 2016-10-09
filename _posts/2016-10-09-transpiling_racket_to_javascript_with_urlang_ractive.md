---
layout:     post
comments:   true
title:      "Transpiling Racket to Javascript with Urlang"
subtitle:   "Ractive Tutorial"
date:       2016-10-09
author:     "Promesante"
header-img: "img/palmar.jpg"
---
# Porting Ractive Tutorial from Javascript into Urlang

As proposed in [previous post]({% post_url 2016-09-18-transpiling_racket_to_javascript_with_urlang_basic_examples %})...

> My next step here will be similar, but based on another, slightly more sophisticated example, among the ones supplied with [Urlang](https://github.com/soegaard/urlang); most likely, [the one](https://github.com/soegaard/urlang/tree/master/urlang-examples/ractive) interoperating with [Ractive](http://www.ractivejs.org/).

So, I've ported a tutorial on Ractive, from Javascript to Urlang, and will share that experience in this post, and the resulting code in my GitHub [repo](https://github.com/promesante/urlang-examples/tree/master/ractive/tutorial/2).

Picked tutorial is the second example supplied in the [excellent tutorial](http://learn.ractivejs.org/hello-world/) available in Ractive's site: the one on [nested properties](http://learn.ractivejs.org/nested-properties/). Starting with the HTML page supplied in the [60 second setup](http://www.ractivejs.org/60-second-setup) section, I kept adding the code snippets given in each step of the tutorial, and ended up with the [HTML file](https://github.com/promesante/urlang-examples/blob/master/ractive/tutorial/2/index.html) available in this post's GitHub repo mentioned above.

Among the [examples](https://github.com/soegaard/urlang/tree/master/urlang-examples/ractive) related with Ractive supplied with Urlang, I've picked two as starting points on Urlang's side:

* [ractive original](https://github.com/soegaard/urlang/blob/master/urlang-examples/ractive/ractive-original.rkt)
* [ractive](https://github.com/soegaard/urlang/blob/master/urlang-examples/ractive/ractive.rkt)

For each of them I've coded a counterpart, listed below, ported from the [Ractive HTML file](https://github.com/promesante/urlang-examples/blob/master/ractive/tutorial/2/index.html) mentioned above:

* [ractive original](https://github.com/promesante/urlang-examples/blob/master/ractive/tutorial/2/nested-properties-original.rkt)
* [ractive](https://github.com/promesante/urlang-examples/blob/master/ractive/tutorial/2/nested-properties.rkt)

My next step will be similar, but based on another one of the Urlang examples: the [one](https://github.com/soegaard/urlang/tree/master/urlang-examples/raphael) targeting [Raphael](http://dmitrybaranovskiy.github.io/raphael/). My plan is to cross-fertilize it with another [example](https://annekjohnson.com/blog/2014/10/using-clojurescript-to-make-charts-with-raphael/) of Raphael, but implemented in Clojurescript: I will try to port each of them to the other platform, in order to compare Urlang with Clojurescript.

Then, I will try to begin to interoperate with Facebook's [React](https://facebook.github.io/react/index.html). In a similar fashion with the step depicted in the previous paragraph, I will follow the guidance of wrappers around React implemented in Clojurescript:

* [Reagent](http://reagent-project.github.io/)
* [Om](https://github.com/omcljs/om)
* [Quiescent](https://github.com/levand/quiescent)

Till then !
