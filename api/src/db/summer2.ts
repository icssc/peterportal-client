import mongoose from 'mongoose';
import { db } from '.';
import { planner, user } from './schema';
import { MongoRoadmap, SavedPlannerData, SavedPlannerYearData } from '@peterportal/types/src/roadmap';
import { inArray } from 'drizzle-orm';
import Roadmap from '../models/roadmap';

const uri = process.env.MONGO_URL;
const conn = await mongoose.connect(uri!, {
  dbName: 'peterPortalDB',
  serverSelectionTimeoutMS: 5000,
});

const planners = await db.select().from(planner);

const affectedPlanners = planners.filter((planner) =>
  (planner.years as SavedPlannerYearData[]).some((year) => year.quarters.some((quarter) => quarter.name === undefined)),
);

const affectedUsersIds = affectedPlanners.map((planner) => planner.userId!);

console.log(
  'affected planners:',
  affectedPlanners.map((planner) => planner.id),
);

const affectedUsers = await db.select().from(user).where(inArray(user.id, affectedUsersIds));

console.log("affected users' ids", affectedUsersIds);
console.log(
  "affected users' names",
  affectedUsers.map((user) => user.name),
);

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
  Fall: 'Fall',
  Winter: 'Winter',
  Spring: 'Spring',
  Summer1: 'Summer1',
  Summer10wk: 'Summer10wk',
  Summer2: 'Summer2',
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

if (affectedUsers.length === 0) {
  console.log('no affected users');
} else {
  type LegacyRoadmap = Omit<MongoRoadmap, 'userId'> & {
    userID: string;
    roadmap: { planner: SavedPlannerData['content'] };
  };

  const roadmaps = await Roadmap.aggregate<Omit<MongoRoadmap, 'userId'> & { userID: string }>([
    {
      $match: {
        userID: {
          $in: affectedUsers.map((user) => user.googleId),
        },
      },
    },
  ]);

  console.log('assert same', roadmaps.length, affectedUsers.length);

  const userIdMapping = Object.fromEntries(affectedUsers.map((user) => [user.googleId, user.id]));

  await db.delete(planner).where(inArray(planner.userId, affectedUsersIds));
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
}

conn.disconnect();

/**
 * 
 * affected planners: [
   27,  37,  64,  87,  99, 102, 103, 104, 110,
  114, 115, 119, 120, 124, 127, 130, 138, 140,
  141, 154, 157, 160, 162, 163, 164, 167, 172,
  174, 183, 189, 195, 196, 203, 205, 209, 211,
  213, 214, 217, 220, 221, 227, 228, 230, 234,
  236, 241, 246, 250, 251, 252, 257, 262, 275,
  276, 282, 285, 296, 309
]
affected users' ids [
  196, 206,  47, 256, 267, 270, 271, 271, 117,   7,   7,
  285, 286,  73, 293, 296,  81,  64,   1, 318,  42, 324,
  326, 327,  45, 331,  37, 338,  63, 352, 358, 359, 366,
    4, 372, 374,  74, 377, 380, 382, 382, 388, 389, 391,
   11, 397,  43, 162,  13,  13,  87,  68, 419, 430,  30,
   85, 440, 449,  50
]

 */
