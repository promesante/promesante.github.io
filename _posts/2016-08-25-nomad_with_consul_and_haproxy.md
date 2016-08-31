---
layout:     post
comments:   true
title:      "Nomad with Consul and HAProxy"
subtitle:   "Putting HAProxy in front of a Nomad cluster in order to use it to run microservices on it"
date:       2016-08-31
author:     "Promesante"
header-img: "img/02.jpg"
---

# Introduction

For the reasons exposed in the previous post, I've begun exploring [Hashicorp's Nomad](https://www.nomadproject.io/).

I am mostly interested in long running services (microservices), and the most useful article I have found out about Nomad dealing with web apps is [Introducing cluster schedulers](http://sysadvent.blogspot.com.ar/2015/12/day-12-introduction-to-nomad.html). Unfortunately, this article does not consider any load balancer in front of them, which is mandatory in order to serve several instances of the same app / service. I was not then able to find out any article adding a load balancer in front of a Nomad cluster, but several combining Docker, Consul, Consul Template, Registrator, and Nginx or HAProxy, listed below:

* [Using HAProxy and Consul for dynamic service discovery on Docker](http://sirile.github.io/2015/05/18/using-haproxy-and-consul-for-dynamic-service-discovery-on-docker.html)
* [Automatic container registration with Consul and Registrator](https://jlordiales.me/2015/02/03/registrator/)
* [Where are my containers? Dockerized service discovery with Consul](https://jlordiales.me/2015/01/23/docker-consul/)
* [Scalable Architecture DR CoN: Docker, Registrator, Consul, Consul Template and Nginx](http://www.maori.geek.nz/scalable_architecture_dr_con_docker_registrator_consul_nginx/)
* [Service discovery with consul and consul-template](http://agiletesting.blogspot.com.ar/2014/11/service-discovery-with-consul-and.html)
* [Load-balancing Docker containers with Nginx and Consul-Template](https://tech.bellycard.com/blog/load-balancing-docker-containers-with-nginx-and-consul-template/)

Among them, I've chosen [Consul Template and Registrator](http://fstn.hateblo.jp/entry/2014/10/26/153247), as its environment is configured and run by means of [Vagrant](https://www.vagrantup.com/), like the environment used in the target article on Nomad; it does not matter it's written in Japanese, as we are currently interested just in a couple commands exposed there. *WARNING*: Google Translator capitalizes the first letter of every command...

Basically, I have added the VM configured in the latter article, with an HAProxy load balancer, into the set configured in the former one.

# Explanation

I will depict those configurations, but just providing the raw commands, as the whole explanation on them is supplied in those two excellent articles. Hence, I'd suggest taking a look at both of them before running the following tasks. Commands to run are shown below grouped in sections, each after which of these two articles they were taken from:

## Introducing cluster schedulers

```
cd nomad-intro
vagrant up
vagrant status
vagrant ssh nomad
nomad run /vagrant/microbot.nomad
```

## Consul Template and Registrator

```
vagrant ssh haproxy1
cat /etc/haproxy/haproxy.cfg
```

HAProxy config file will then lack any entry at its very bottom for any Web app.

```
sudo consul-template\
 -consul=10.7.0.15:8500\
 -template=/vagrant/haproxy.ctmpl:/etc/haproxy/haproxy.cfg:"/etc/init.d/haproxy reload"

cat /etc/haproxy/haproxy.cfg
```

Now, HAProxy config file includes an entry for each of the nine Web apps started by Nomad in the three "Docker" nodes. And invoking `curl` succesively, it will show the different Docker container IDs; for example:

```
curl 10.7.0.31
```

## Introducing cluster schedulers

Open the `microbot.nomad` file with your favorite text editor and change the instance value from 9 to 50. Once you're done, tell Nomad to about our changes by running:

```
vagrant ssh nomad
nomad run /vagrant/microbot.nomad
```

## Consul Template and Registrator

Hence, HAProxy config file includes an entry for each of these fifty Web apps started by Nomad in the three "Docker" nodes. And invoking `curl` succesively, it will show the different Docker container IDs, such as:

```
curl 10.7.0.31
```

You can check running services and their corresponding instances by means of Consul:

List running services:

```
curl -s http://10.7.0.15:8500/v1/catalog/services
```

List information about our `microbot` service:

```
curl -s http://10.7.0.15:8500/v1/catalog/service/microbot-web-microbot
```

Or straight with the browser at:

[http://10.7.0.15:8500/ui/](http://10.7.0.15:8500/ui/)

And that's it !

# Next Steps

From this base config, I plan to explore in the near future the following paths, sharing the experience here with the corresponding articles:

* Load balance with Nginx
* Provision with Puppet
* Deploy in AWS

I hope this information to be useful for you ! And till next post !
