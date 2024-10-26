import Session from '../models/session';
import { db } from '.';
import { reviews, users } from './schema';
import Review from '../models/review';
import Roadmap from '../models/roadmap';
import Preference from '../models/preference';

// get all possible users from sessions, then reviews, then roadmaps
const sessions = await Session.find({});
const sessionsUserRecords = sessions.map((session) => ({
  id: Number(String(session.session!.passport!.user!.id)),
  displayName: session.session!.passport!.user!.name,
}));
await db.insert(users).values(sessionsUserRecords);

const reviewDocs = await Review.find({});
for (const review of reviewDocs) {
  await db
    .insert(users)
    .values({ id: Number(review.userID), displayName: review.userDisplay })
    .onConflictDoNothing();
}

const roadmaps = await Roadmap.find({});
for (const roadmap of roadmaps) {
  await db
    .insert(users)
    .values({ id: Number(roadmap.userID), displayName: 'Anonymous Peter' })
    .onConflictDoNothing();
}

// transfer preferences + add any new users
const preferences = await Preference.find({});
for (const preference of preferences) {
  await db
    .insert(users)
    .values({ id: Number(preference.userID), displayName: 'Anonymous Peter', theme: preference.theme })
    .onConflictDoUpdate({ target: users.id, set: { theme: preference.theme } });
}

// transfer roadmaps + separate planners, transfered courses, coursebag

// transfer reviews + assign new id and make mapping of old id to new id
const reviewIdMapping: Record<string, number> = {};
for (const review of reviewDocs) {
  const id = await db
    .insert(reviews)
    .values({
      professorId: review.professorID,
      courseId: review.courseID,
      userId: Number(review.userID),
      anonymous: review.userDisplay === 'Anonymous Peter',
      content: review.reviewContent,
      rating: review.rating,
      difficulty: review.difficulty,
      createdAt: review.timestamp,
      forCredit: review.forCredit as boolean,
      quarter: review.quarter,
      // score: review.score,
      takeAgain: review.takeAgain as boolean,
      textbook: review.textbook as boolean,
      attendance: review.attendance as boolean,
      tags: review.tags,
      verified: review.verified,
    })
    .returning({ newId: reviews.id });
  reviewIdMapping[String(review.id)] = id[0].newId;
}

// transfer reports + votes, use new review id, discard old report id and let postgres generate new one
// const reports = await Report.find({});

// const votes = await Vote.find({});

// transfer sessions (note: user should be able to update later on future logins though)
