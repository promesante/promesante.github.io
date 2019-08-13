---
layout:     post
comments:   true
title:      "Schedulers with Optimistic Concurrency"
subtitle:   "Evaluating schedulers for microservices"
date:       2016-08-30
author:     "Promesante"
background: "/img/posts/02.jpg"
---

For the development of the eCommerce platform for [Garbarino.com](https://www.garbarino.com/), Argentina's largest home appliance retailer chain, we have fully embraced [Docker](https://www.docker.com/). Hence, I have studied about it, mainly Karl Matthias' excellent [Docker: Up and Running](http://www.amazon.com/Docker-Up-Running-Karl-Matthias/dp/1491917571).

In its chapter on "The Path to Production Containers", Karl states:

> At DockerCon EU, December 2014, Solomon Hykes, founder and CEO of dotCloud, described Apache Mesos as [the gold standard for clustered containers](https://www.youtube.com/watch?v=sGWQ8WiGN8Y&feature=youtu.be&t=35m10s)

Of course, I began studying it, with another excellent book: Dharmesh Kakadia's [Apache Mesos Essentials](http://www.amazon.com/Apache-Mesos-Essentials-Dharmesh-Kakadia/dp/1783288760/). Mainly, its chapter on "Running Services on Mesos". And then began playing with [Mesosphere Marathon](https://mesosphere.github.io/marathon/).

Then, I stumbled upon "optimistic concurrency":

* [Omega: flexible, scalable schedulers for large compute clusters](http://static.googleusercontent.com/media/research.google.com/es//pubs/archive/41684.pdf)
* [Quincy: Fair Scheduling for Distributed Computing Clusters](http://kunaltalwar.org/papers/quincy-sosp09.pdf)

And about open source schedulers designed thay way, whereas Mesos currently implementing pessimistic concurrency:

* [Hashicorp's Nomad](https://www.nomadproject.io/)
* [Firmament](http://www.firmament.io/)

I've begun exploring Nomad: in upcoming posts, I will share findings under way.

Till then !
