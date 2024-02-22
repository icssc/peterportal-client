/**
 @module MongoHelper
*/

import mongoose from 'mongoose';

/**
 * Database name to use in mongo
 */
const DB_NAME = process.env.NODE_ENV == 'production' ? 'peterPortalDB' : 'peterPortalDevDB';

/**
 * Client used to connect to mongo
 */
(async () => {
  try {
    await mongoose.connect(process.env.MONGO_URL, {
      dbName: DB_NAME,
    });
  } catch (err) {
    console.log(err);
  }
})();
/**
 * Collection names that we are using
 */
const COLLECTION_NAMES = {
  SESSIONS: 'sessions',
  REVIEWS: 'reviews',
  ROADMAPS: 'roadmaps',
  VOTES: 'votes',
  REPORTS: 'reports',
  PREFERENCES: 'preferences',
};
/**
 * Global reference to database
 */

/**
 * Get a mongo database object
 * @returns Mongo database object
 */

// TODO: most of these helpers are unnecessary, just use the collection directly
// also there's gotta be a better way to initialize the client and get the db then what we're doing
// we should consider just switching to mongoose for the type safety and schema validation

// /**
//  * Gets mongo collection object by name
//  * @param collectionName Name of collection to retreive
//  * @returns Mongo collection object
//  */

// /**
//  * Checks if id exists in result collection
//  * @param collectionName Name of collection
//  * @param id ID of the document
//  * @returns True if document ID exists in the given collection
//  */
// async function containsID(collectionName: string, id: string) {
//   const collection = await getCollection(collectionName);
//   return (await collection.countDocuments({ _id: id })) > 0;
// }

// /**
//  * Adds a document to a collection
//  * @param collectionName Name of collection
//  * @param document Document object to add
//  * @returns Promise that is resolved when document is added
//  */
// async function addDocument(collectionName: string, document: GenericObject) {
//   const collection = await getCollection(collectionName);
//   collection.insertOne(document);
// }

// /**
//  * Gets a document from a collection
//  * @param collectionName Name of collection
//  * @param query Query object
//  * @returns Mongo Document objects returned from the query
//  */
// async function getDocuments(collectionName: string, query: object): Promise<object[]> {
//   const collection = await getCollection(collectionName);
//   const results = collection.find(query);
//   const documents = await results.toArray();
//   return documents;
// }

// /**
//  * Updates a document from a collection
//  * @param collectionName Name of collection
//  * @param query Query object
//  * @param update Update object
//  * @returns
//  */
// async function updateDocument(collectionName: string, query: GenericObject, update: GenericObject) {
//   const collection = await getCollection(collectionName);
//   collection.updateOne(query, update);
// }

// /**
//  * Replaces a document from a collection
//  * @param collectionName Name of collection
//  * @param query Query object
//  * @param update Update object
//  * @returns
//  */
// async function replaceDocument(collectionName: string, query: GenericObject, update: GenericObject) {
//   const collection = await getCollection(collectionName);
//   collection.replaceOne(query, update);
// }

// /**
//  * Deletes a document from a collection
//  * @param collectionName Name of collection
//  * @param query Query object
//  * @returns
//  */
// async function deleteDocument(collectionName: string, query: GenericObject) {
//   const collection = await getCollection(collectionName);
//   await collection.deleteOne(query);
// }

// /**
//  * Deletes multiple documents from a collection
//  * @param collectionName Name of collection
//  * @param query Query object
//  * @returns
//  */
// async function deleteDocuments(collectionName: string, query: GenericObject) {
//   const collection = await getCollection(collectionName);
//   await collection.deleteMany(query);
// }

export {
  DB_NAME,
  COLLECTION_NAMES,
  // getCollection,
  // getDB,
  // containsID,
  // addDocument,
  // getDocuments,
  // updateDocument,
  // replaceDocument,
  // deleteDocument,
  // deleteDocuments,
};
