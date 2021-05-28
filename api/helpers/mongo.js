const MongoClient = require('mongodb').MongoClient;
const client = new MongoClient(process.env.MONGO_URL, { useNewUrlParser: true, useUnifiedTopology: true });

const DB_NAME = 'peterPortalDB';
const COLLECTION_NAMES = {
    SESSIONS: 'sessions',
    REVIEWS: 'reviews'
}
let db = undefined;

function getDB() {
    return new Promise((resolve, reject) => {
        // if not connected yet, initiate connection
        if (!db) {
            client.connect(async err => {
                if (err) {
                    reject(err);
                }
                else {
                    db = client.db(DB_NAME);

                    // get existing mongo collection
                    let collectionNames = Object.values(COLLECTION_NAMES);
                    let collections = await db.listCollections().toArray();
                    let existingCollectionNames = [];
                    collections.forEach(collection => {
                        existingCollectionNames.push(collection["name"])
                    })

                    // create collections that dont exist
                    for (let i = 0; i < collectionNames.length; ++i) {
                        let collectionName = collectionNames[i];
                        if (!existingCollectionNames.includes(collectionName)) {
                            await db.createCollection(collectionName);
                        }
                    }
                    resolve(db);
                }
            });
        }
        else {
            resolve(db);
        }
    })
}

// gets collection object by name
function getCollection(collectionName) {
    return new Promise(async (resolve, reject) => {
        await getDB();
        // check if collection exists
        db.listCollections({ name: collectionName })
            .next(function (err, collection) {
                if (err) reject(err);
                if (collection) {
                    resolve(db.collection(collectionName));
                }
                else {
                    reject(`Collection ${collectionName} does not exist!`);
                }
            });
    });
}

// adds a document to a collection
function addDocument(collectionName, document) {
    return new Promise(async (resolve, reject) => {
        await getDB();
        // get collection
        getCollection(collectionName)
            .then(async (collection) => {
                // Add the document
                collection.insertOne(document, (error) => {
                    if (error) {
                        reject(error);
                    }
                    else {
                        resolve();
                    }
                });
            });
    });
}

// gets a document from a collection
function getDocuments(collectionName, query) {
    return new Promise(async (resolve, reject) => {
        await getDB();
        // get collection
        getCollection(collectionName)
            .then(async (collection) => {
                // get document
                let results = await collection.find(query);
                let documents = await results.toArray();
                resolve(documents);
            });
    })
}

// updates a document from a collection
function updateDocument(collectionName, query, update) {
    return new Promise(async (resolve, reject) => {
        await getDB();
        getCollection(collectionName)
            .then(async (collection) => {
                await collection.updateOne(query, update, (err) => {
                    if (err) console.log(err)
                });
                resolve();
            })
    });
}

module.exports = { DB_NAME, COLLECTION_NAMES, getDB, addDocument, getDocuments, updateDocument };