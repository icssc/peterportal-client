FROM node:14

# Install dependencies
WORKDIR /usr/src/app
COPY package*.json ./
RUN npm install --production=false

# Copy source code
COPY . ./
# Change proxy setting in docker container
RUN sed -i "s|http://localhost:5000|http://api:5000|g" ./package.json
RUN cat ./package.json

# Start local
CMD ["npm", "start"]