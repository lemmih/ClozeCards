# Stage 1, build application and do server side rendering using production data.
FROM zenato/puppeteer

USER root

# Install dependencies before copying source files. This way, dependencies are
# not reinstalled unnecessarily.
COPY app/package.json app/yarn.lock ./
RUN yarn install

COPY app/ .
RUN rm -fr public/static/
RUN yarn build

# Stage 2, configure nginx to serve app and static content.
FROM nginx

COPY app/public/static /app/static
COPY --from=0 build /app/
COPY nginx/ClozeCards.nginx.conf /etc/nginx/conf.d/
RUN rm /etc/nginx/conf.d/default.conf

# ADD ssl/ /etc/ssl/

EXPOSE 80
EXPOSE 443
