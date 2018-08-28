---
title: Deploying Yesod applications with Keter
published: 2017-05-04
...

![](/img/keter/thumbnail.png){#thumbnail}\

[Keter](https://github.com/snoyberg/keter) is the
[Yesod](http://www.yesodweb.com/)'s deployment system, fully featured and a joy
to use, but the are some pitfalls that the documentation doesn't cover, and
that the user has to find out for her self; So I'll try to give them away here
together with a walk-through tutorial.

Although Keter is flexible and general enough to be used with various kind of
applications and web frameworks, here I'm going to assume you're using it to
deploy Yesod applications. Moreover, I'll assume you're using Yesod's
scaffolding, as it is the preferred way to write production ready applications.

<!--more-->

I'm also taking for granted that you've already installed on your server system
whatever DBMS that your Yesod app needs, and have also created the app's
databases.

# Installing Keter on the server

## Keter binary

It is always advisable to compile on the development machine rather than the
production server, to avoid utilising its resources for building (specially
considering that GHC can make use of a fair amount of them). So, assuming the
architectures match, you can just install `keter` on you local machine:

    $ stack install keter

And then put the binary on the server (*example.com*):

    $ scp ~/.local/bin/keter root@example.com:/root/

## Keter user

It's a good practice to have a dedicated *keter* user, so you don't have to
deploy as root each time:

    # useradd keter
    # passwd keter


## Directory tree

The directory tree needed on the server is as follows:

```
keter
├── bin
│   └── keter
├── etc
│   └── keter-config.yaml
├── incoming
    └── app.keter
```

So create it, copy the binary to `/opt/keter/bin`, and make sure
`/opt/keter/incoming` it's owned by the *keter* user (we'll take care of the
`keter-config.yaml` configuration later):

    # mkdir -p /opt/keter /opt/keter/bin /opt/keter/etc /opt/keter/incoming
    # cp /root/keter /opt/keter/bin
    # touch /opt/keter/keter-config.yaml
    # chown -R keter:keter /opt/keter/icoming


## Init System

While you could just execute `/opt/keter/bin/keter` directly, it's better to
register it as a job in your Init System.


#### Sysmted (RedHat, Fedora, CentOS, Arch, openSUSE, etc)

Create a file `/etc/systemd/system/keter.service`, with the contents:

```
[Unit]
Description=Keter
After=network.service

[Service]
Type=simple
ExecStart=/opt/keter/bin/keter /opt/keter/etc/keter-config.yaml

[Install]
WantedBy=multi-user.target
```

Enable the service:

    $ sudo systemctl enable keter

Now you can start *keter* with (don't do it just yet, as we still need to write
the *keter* configuration file):

    $ sudo systemctl start keter


#### Upstart (Debian, Ubuntu, etc)

Create a file `/etc/init/keter.con`, with the contents:

```
start on (net-device-up and local-filesystems and runlevel [2345])
stop on runlevel [016]
respawn
console output
exec /opt/keter/bin/keter /opt/keter/etc/keter-config.yaml
```

Now you can start *keter* with (don't do it just yet, as we still need to write
the *keter* configuration file):

    $ sudo start keter



# Configuration

## Server Side

The *Keter* configuration at `/opt/keter/etc/keter-config.yaml` is pretty
straight forward:

```
root: ..

listeners:
    # HTTP
    - host: "*4" # Listen on all IPv4 hosts
      port: 80
    # HTTPS
    #- host: "*4"
      #port: 443
      #key: key.pem
      #certificate: certificate.pem

# env:
#    key: value
```

The `root` option points, as expected, to `/opt/keter`.

Make sure to change the `port` option if you're reverse forwarding from a
fronted server like *Nginx* or *Apache* (more on this later).

If you're serving your application over SSL (and you should), uncomment the
*HTTPS* section, then point the `key` option to your `privkey.pem` file, and the
`certificate` option to your `fullchain.pem` file.

The `env` option, keeps pairs of *keys* and *values*. The main set of values
you'll need here are your Database credentials. You've probably already
configured database credentials in the `database` section in the
`config/settings.yaml` file, so you'll notice you need some environment
variables like `MYSQL_USER`, `MYSQL_PASSWORD`, etc. If you're using
MySQL/MariaDB; Or `PGUSER`, `PGPASS`, etc. If you're using PostgreSQL. You
get the idea.

This is how it will look like for a PostgreSQL Database where only the user and
password are different between the development and production servers (be sure
to keep the quotes!).

```
env:
    PGUSER: "user"
    PGPASS: "password"
```

## Yesod application side

The Keter configuration file for your Yesod application lives in
`config/keter.yml`. Set `user-edited` to `true`, so you're able to execute
`yesod keter` later on.

Locate the `copy-to` option and configure it to use the `keter` user and your
server domain (or IP address):

    copy-to: keter@example.com:/opt/keter/incoming/

This will allow you to deploy your application with:

    $ stack -- exec yesod keter

#### Hosts Configuration

The most important part of the Keter configuration is perhaps the `hosts`
option of the `webapp` stanza, the hosts you declare here are the ones that your
application is going to respond to. Unless you're using a separate domain for
serving static files, be sure to keep the `hosts` option of the `static-files`
stanza in sync with the `webapp` one.

This one here is a pretty common error message when trying to deploy a Yesod
application (and failing miserably):

![](/img/keter/shot1.png){.img-responsive}

There is more than one reason for this, but the main one is that the domain name
or IP address doesn't exactly match one of the hosts provided in the `hosts`
option.

If you're serving only one application and using *Keter* as the main server
listening on port `80`, then having your domain name in `hosts` will pretty much
suffice, BUT most of the time, even if your serving only one application, you're
probably using a frontend server like *Nginx* or *Apache*, in which case you
have to consider the port the reverse proxy is pointing to.

Take for instance this *Nginx* reverse proxy configuration for an app that lives
on `blog.example.com`

```
server {
    listen 80;
    server_name blog.example.com;
    location / {
            proxy_pass http://127.0.0.1:4321;
    }
```

With a *Keter* configuration that has:

```
listeners:
    - host: "*4" # Listen on all IPv4 hosts
      port: 4321
```

Then you have a problem. If you try to connect to `http://blog.example.com`
you'll get the aforementioned error message, telling you that "127.0.0.1:4321,
is not recognized". It makes sense if you think about it, *Nginx* will redirect
the connection to `127.0.0.1:4321` so *Keter* can handle it, but there is no
application that responds to `127.0.0.1:4321`, and notice the port number here,
as it is significant for *Keter* when trying to find a corresponding
application!

To fix this, we must allow our application to respond to `127.0.0.1:4321` as
well:

```
hosts:
    - blog.example.com
    - blog.example.com:4321
    - 127.0.0.1:4321
```

Restart *Ngnix* and *Keter* on the server to allow the configuration to take
effect and redeploy the application:

    $ stack -- exec yesod keter


#### Redirections

If you're going to use the `redirect` stanza to automatically redirect any
connection to, lets say `example.com` to `wwww.example.com`:

```
- type: redirect
  hosts:
      - example.com
  actions:
      - host: www.example.com
```

Then be completely sure to have `www.example.com` in the `hosts` option of the
`webapp` stanza as well, failing to do this will take you to the same error
message.


# Other sources of error

## "Welcome to Keter"

If you're still reaching this error:

![](/img/keter/shot1.png){.img-responsive}

Unfortunately, the same error message appears if an application that responds to
that host is actually found, but is failing to start.

Check the `/opt/keter/log/app-yourapp/current.log` log file, chances are you
have changes in your persistent models that can't be reflected in your database
without user intervention, so be sure to manually fix them the same way you have
to do in your development database.

## Changes in new deployed version not taking effect

It is pretty common to forget changes in persistent models that need manual user
intervention after deploying, similarly to the error above, this will prevent
the app to start. If you currently have a version of your app working, *Keter*
will use it instead of the new one if it fails to start, so if the latest
deployed changes seem to not be taking effect, this can also be the source of
the problem.
