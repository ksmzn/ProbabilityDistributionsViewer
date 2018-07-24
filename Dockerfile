FROM ksmzn/shinydistributionsapp:latest

COPY  /ShinyDistributionsApp/ /srv/shiny-server/

CMD exec shiny-server >> /var/log/shiny-server.log 2>&1
