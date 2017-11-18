FROM rroemhild/ejabberd

USER root

RUN apt-get update -y && apt-get install nano
ADD ./ejabberd.yml.tpl /opt/ejabberd/conf/ejabberd.yml.tpl

USER ejabberd