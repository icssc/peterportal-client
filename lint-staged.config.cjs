/** @type {import('lint-staged').Config} */
const config = {
  '*.{scss,js,ts,tsx}': ['prettier --write'],
};

module.exports = config;
