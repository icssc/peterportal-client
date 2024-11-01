import Session from '../models/session';
import { db } from '.';
import { planner, report, review, savedCourse, transferredCourse, user, vote } from './schema';
import Review from '../models/review';
import Roadmap from '../models/roadmap';
import Preference from '../models/preference';
import Report from '../models/report';
import Vote from '../models/vote';
import { anonymousName, MongoRoadmap, SavedPlannerData, SavedPlannerYearData } from '@peterportal/types';
import mongoose from 'mongoose';

const uri = process.env.MONGO_URL;
const conn = await mongoose.connect(uri!, {
  dbName: 'peterPortalDB',
  serverSelectionTimeoutMS: 5000,
});

// get all possible users from sessions, then reviews, then roadmaps, then votes
const sessions = await Session.find();
// need to filter out bad sessions (never signed in with google, just clicked log in)
const sessionsUserRecords = sessions
  .filter((session) => session.session?.passport?.user?.id != null)
  .map((session) => ({
    googleId: session.session!.passport!.user!.id!,
    displayName: session.session!.passport!.user!.name,
    email: session.session!.passport!.user!.email,
    picture: session.session!.passport!.user!.picture!,
  }));

const userIdMapping: Record<string, number> = {};

const newIdsFromSessions = await db
  .insert(user)
  .values(sessionsUserRecords)
  .onConflictDoNothing()
  .returning({ id: user.id, googleId: user.googleId });
for (const newId of newIdsFromSessions) {
  userIdMapping[newId.googleId] = newId.id;
}

const reviewDocs = await Review.find();
for (const review of reviewDocs) {
  const newId = await db
    .insert(user)
    .values({ googleId: review.userID, displayName: review.userDisplay, email: '', picture: '' })
    .onConflictDoNothing()
    .returning({ id: user.id });
  if (newId.length > 0) {
    userIdMapping[review.userID] = newId[0].id;
  }
}

const roadmaps = await Roadmap.find<Omit<MongoRoadmap, 'userId'> & { userID: string }>();
for (const roadmap of roadmaps) {
  const newId = await db
    .insert(user)
    .values({ googleId: roadmap.userID, displayName: anonymousName, email: '', picture: '' })
    .onConflictDoNothing()
    .returning({ id: user.id, googleId: user.googleId });
  if (newId.length > 0) {
    userIdMapping[roadmap.userID] = newId[0].id;
  }
}

const voteDocs = await Vote.find();
for (const vote of voteDocs) {
  const newId = await db
    .insert(user)
    .values({ googleId: vote.userID, displayName: anonymousName, email: '', picture: '' })
    .onConflictDoNothing()
    .returning({ id: user.id, googleId: user.googleId });
  if (newId.length > 0) {
    userIdMapping[vote.userID] = newId[0].id;
  }
}

// transfer preferences + add any new users
const preferences = await Preference.find();
for (const preference of preferences) {
  const newId = await db
    .insert(user)
    .values({
      googleId: preference.userID,
      displayName: anonymousName,
      theme: preference.theme,
      email: '',
      picture: '',
    })
    .onConflictDoUpdate({ target: user.googleId, set: { theme: preference.theme } })
    .returning({ id: user.id });
  if (newId.length > 0) {
    userIdMapping[preference.userID] = newId[0].id;
  }
}

type LegacyRoadmap = Omit<MongoRoadmap, 'userId'> & {
  userID: string;
  roadmap: { planner: SavedPlannerData['content'] };
};

const quarterNameMapping: Record<string, string> = {
  fall: 'Fall',
  winter: 'Winter',
  spring: 'Spring',
  // Old Lowercase Display Names
  'summer I': 'Summer1',
  'summer II': 'Summer2',
  'summer 10 Week': 'Summer10wk',
  // Transcript Names
  'First Summer': 'Summer1',
  'Second Summer': 'Summer2',
  'Special / 10-Week Summer': 'Summer10wk',
};

/**
 * replace legacy quarter names with standard ones
 * @param planner years
 * @returns planner years
 */
function normalizeQuarterNames(planner: SavedPlannerYearData[]) {
  return planner.map((year) => ({
    startYear: year.startYear,
    name: year.name,
    quarters: year.quarters.map((quarter) => ({ name: quarterNameMapping[quarter.name], courses: quarter.courses })),
  }));
}

// transfer roadmaps + separate planners, transfered courses, coursebag
// normalize quarter names as well
await db.insert(planner).values(
  roadmaps.flatMap((roadmap) => {
    if ((roadmap as LegacyRoadmap).roadmap.planner != null) {
      // old roadmap format (before multi-planner)
      const planner = (roadmap as LegacyRoadmap).roadmap.planner;
      return {
        userId: userIdMapping[roadmap.userID],
        name: "Peter's Roadmap",
        years: normalizeQuarterNames(planner),
      };
    }

    return roadmap.roadmap.planners.map((planner) => ({
      userId: userIdMapping[roadmap.userID],
      name: planner.name,
      years: normalizeQuarterNames(planner.content),
    }));
  }),
);

await db.insert(transferredCourse).values(
  roadmaps.flatMap((roadmap) =>
    roadmap.roadmap.transfers.map((transfer) => ({
      userId: userIdMapping[roadmap.userID],
      courseName: transfer.name,
      units: transfer.units,
    })),
  ),
);

await db
  .insert(savedCourse)
  .values(
    roadmaps.flatMap((roadmap) =>
      roadmap.coursebag.map((course) => ({ userId: userIdMapping[roadmap.userID], courseId: course })),
    ),
  );

// transfer reviews + assign new id and make mapping of old id to new id
const reviewIdMapping: Record<string, number> = {};
const newIds = await db
  .insert(review)
  .values(
    reviewDocs.map((review) => ({
      professorId: review.professorID,
      courseId: review.courseID,
      userId: userIdMapping[review.userID],
      anonymous: review.userDisplay === anonymousName,
      content: review.reviewContent,
      rating: review.rating,
      difficulty: review.difficulty,
      gradeReceived: review.gradeReceived,
      createdAt: review.timestamp,
      updatedAt: null,
      forCredit: review.forCredit ?? false,
      quarter: review.quarter,
      // score: review.score,
      takeAgain: review.takeAgain ?? false,
      textbook: review.textbook ?? false,
      attendance: review.attendance ?? false,
      tags: review.tags,
      verified: review.verified,
    })),
  )
  .returning({ newId: review.id });

for (let i = 0; i < reviewDocs.length; i++) {
  reviewIdMapping[String(reviewDocs[i].id)] = newIds[i].newId;
}

// transfer reports + votes, use new review id, discard old report id and let postgres generate new one
const reportDocs = await Report.find();
if (reportDocs.length > 0) {
  await db.insert(report).values(
    reportDocs.map((report) => ({
      reviewId: reviewIdMapping[String(report.reviewID)],
      reason: report.reason,
      createdAt: report.timestamp,
    })),
  );
}

const filteredVoteDocs = voteDocs.filter((vote) => vote.reviewID in reviewIdMapping); // we have some bad data here (floating votes with no review)
await db.insert(vote).values(
  filteredVoteDocs.map((vote) => ({
    reviewId: reviewIdMapping[vote.reviewID],
    userId: userIdMapping[vote.userID],
    vote: vote.score,
    createdAt: vote.timestamp,
  })),
);

// transfer sessions? (note: user should be able to update later on future logins though)

conn.disconnect();
