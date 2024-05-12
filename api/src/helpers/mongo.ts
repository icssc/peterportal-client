/**
 @module MongoHelper
*/

/**
 * Database name to use in mongo
 */
const DB_NAME = process.env.NODE_ENV == 'production' ? 'peterPortalDB' : 'peterPortalDevDB';

/**
 * Collection names to use in mongo
 */
const COLLECTION_NAMES = {
  SESSIONS: 'sessions',
  REVIEWS: 'reviews',
  ROADMAPS: 'roadmaps',
  VOTES: 'votes',
  REPORTS: 'reports',
  PREFERENCES: 'preferences',
};

export { DB_NAME, COLLECTION_NAMES };
