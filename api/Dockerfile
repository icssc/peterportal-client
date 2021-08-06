FROM node:14

# Install dependencies
WORKDIR /usr/src/app
COPY package*.json ./
RUN npm install --production=false

# Copy source code
COPY . ./

# Start local (using legacy -L flag for hot reloading in docker)
CMD ["npm", "run", "docker-dev"]