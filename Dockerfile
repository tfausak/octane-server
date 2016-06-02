FROM node:6
WORKDIR /code
COPY . /code
RUN npm install
ENV PORT 80
EXPOSE 80
CMD node .
