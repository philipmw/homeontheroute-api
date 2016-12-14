homeontheroute
=====

An OTP application providing a public HTTP API that powers [Home On The Route](http://homeontheroute.com), a web app helping you find the perfect neighborhood for your car-free lifestyle.

Architecture
------------

                    [Application supervisor]
                                |
               /                |               \
              v                 v                v
    [Visitor counter]      [Webserver]      [Transit Server]
                                |
                                |
                                v
                             [Cowboy]

Add GTFS data
-------------

This application GTFS data.  (Tested only with King County Metro's datasets.)
Because GTFS data is large, I don't include it in the source repository, so download it yourself and add it to `./priv`.
Download it, unzip it, and update `transit_server` to point to the right directory.

Here's the intended directory structure:

    ebin/...
    priv/metro-gtfs-2016-11-09/stops.txt
    priv/metro-gtfs-2016-11-09/...
    src/...

Build
-----

    $ rebar3 compile

Test
----

    $ rebar3 eunit

Deploy
------

    $ rebar3 release -n [dev|prod]
    $ rebar3 as prod tar -n [dev|prod]
    
Server setup
------------

I use a C1 instance from [Scaleway](http://www.scaleway.com) with the Gentoo image.  Log in as root with the pre-configured SSH key.

First, edit `/etc/portage/make.conf` to have these lines:

    USE="<...any existing flags...> smp"
    MAKEOPTS=-j5
    PYTHON_TARGETS=python3_4
    FEATURES=nostrip

General OS setup, as root:

    emerge --sync
    emerge --oneshot portage
    emerge -v tmux vim
    emerge -upDv --newuse world
    rm /etc/localtime && ln -s /usr/share/zoneinfo/UTC /etc/localtime

Project-specific setup, as root:

    echo "dev-lang/erlang ~arm" >> /etc/portage/package.accept_keywords
    emerge -v dev-python/pip
    useradd hotr
    iptables -A PREROUTING -t nat -i eth0 -p tcp --dport 80 -j REDIRECT --to-port 8080

As `hotr`:

    pip install --user awscli
    echo 'PATH=$PATH:$HOME/.local/bin' > ~/.bash_profile
    
    # set AWS credentials
    aws s3 cp s3://homeontheroute-api-releases/prod-0.0.1.tar.gz .