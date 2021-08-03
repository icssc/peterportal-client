# Temporary image to build client
FROM node:14 AS clientBuilder

# Install client dependencies
WORKDIR /usr/src/app
COPY site/package*.json ./
RUN npm install --production=false

# Copy client source code
COPY ./site ./

# Build client
RUN npm run build

# Final image to store server and client build
FROM node:14

# Install server dependencies
WORKDIR /usr/src/app
COPY api/package*.json ./
RUN npm install --production=false

# Copy server source code
COPY ./api ./

# Build server
RUN npm run build

# Copy client build to server
COPY --from=clientBuilder /usr/src/app/build /usr/src/app/build

# Start server
CMD ["npm", "start"]