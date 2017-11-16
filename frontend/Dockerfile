FROM nginx

COPY frontend/build/ /app/
COPY nginx/ClozeCards.nginx.conf /etc/nginx/conf.d/
RUN rm /etc/nginx/conf.d/default.conf

# ADD ssl/ /etc/ssl/

EXPOSE 80
EXPOSE 443
