homeontheroute
=====

An OTP application providing a public HTTP API that powers [Home On The Route](http://homeontheroute.com), a web app helping you find the perfect neighborhood for your car-free lifestyle.

Build
-----

    $ rebar3 compile

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

Project-specific setup, as root:

    echo "dev-lang/erlang ~arm" >> /etc/portage/package.accept_keywords
    emerge -v dev-python/pip
    useradd hotr
    iptables -A PREROUTING -t nat -i eth0 -p tcp --dport 80 -j REDIRECT --to-port 8080

As `hotr`:

    pip install --user awscli
    echo 'PATH=$PATH:$HOME/.local/bin' > ~/.bashrc
