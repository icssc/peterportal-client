import Session from '../models/session';
import { db } from '.';
import { reports, reviews, users, votes } from './schema';
import Review from '../models/review';
import Roadmap from '../models/roadmap';
import Preference from '../models/preference';
import Report from '../models/report';
import Vote from '../models/vote';

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
const newIds = await db
  .insert(reviews)
  .values(
    reviewDocs.map((review) => ({
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
    })),
  )
  .returning({ newId: reviews.id });

for (let i = 0; i < reviewDocs.length; i++) {
  reviewIdMapping[String(reviewDocs[i].id)] = newIds[i].newId;
}

// transfer reports + votes, use new review id, discard old report id and let postgres generate new one
const reportDocs = await Report.find({});
await db.insert(reports).values(
  reportDocs.map((report) => ({
    reviewId: reviewIdMapping[String(report.reviewID)],
    reason: report.reason,
    createdAt: report.timestamp,
  })),
);

const voteDocs = await Vote.find({});
await db.insert(votes).values(
  voteDocs.map((vote) => ({
    reviewId: reviewIdMapping[String(vote.reviewID)],
    userId: Number(vote.userID),
    vote: vote.score,
    createdAt: vote.timestamp,
  })),
);

// transfer sessions? (note: user should be able to update later on future logins though)
