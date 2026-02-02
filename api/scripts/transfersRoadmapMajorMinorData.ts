import dotenv from 'dotenv-flow';
import { planner, plannerMajor, plannerMinor /*userMajor, userMinor*/ } from '../src/db/schema';
import { db } from '../src/db';

// load env (because this is a separate script)
dotenv.config();

// Script for moving items from plannerMajor and plannerMinor into
// new tables userMajor and userMinor so that major and minor are linked to
// the user instead of a roadmap

// NOTE: you can use `npx tsx ./scripts/transfersRoadmapMajorMinorData.ts` to run

async function transferUserMajorMinors() {
  const allPlanners = await db.select().from(planner);
  const allPlannerMajors = await db.select().from(plannerMajor);
  const allPlannerMinors = await db.select().from(plannerMinor);

  // get the first (smallest) plannerId for each userId
  const firstPlannerId: { [key: number]: number } = {};
  for (const planner of allPlanners) {
    if (!(planner.userId in firstPlannerId) || planner.id < firstPlannerId[planner.userId]) {
      firstPlannerId[planner.userId] = planner.id;
    }
  }
  // extract first planner for each userId
  const firstPlanners = allPlanners.filter((planner) => planner.id === firstPlannerId[planner.userId]);

  // take the majors from first planner
  const userMajorsToAdd = allPlannerMajors
    .map((plannerMajor) => {
      const userId = firstPlanners.find((planner) => planner.id === plannerMajor.plannerId)?.userId;
      if (userId === undefined) {
        return null;
      } else {
        return {
          userId: userId,
          majorId: plannerMajor.majorId,
          specializationId: plannerMajor.specializationId,
        };
      }
    })
    .filter((plannerMajor) => plannerMajor != null);

  console.log(userMajorsToAdd);

  // take the minors from first planner
  const userMinorsToAdd = allPlannerMinors
    .map((plannerMinor) => {
      const userId = firstPlanners.find((planner) => planner.id === plannerMinor.plannerId)?.userId;
      if (userId === undefined) {
        return null;
      } else {
        return {
          userId: userId,
          minorId: plannerMinor.minorId,
        };
      }
    })
    .filter((plannerMinor) => plannerMinor != null);

  console.log(userMinorsToAdd);

  // NEW VERSION
  // try {
  //   await db.transaction(async (tx) => {
  //     if (userMajorsToAdd.length > 0) {
  //       await tx.insert(userMajor).values(userMajorsToAdd);
  //     }
  //     console.log(`Inserted ${userMajorsToAdd.length} majors`);
  //   });
  // } catch (error) {
  //   console.error('Failed to insert majors:', error);
  //   throw error;
  // }

  // try {
  //   await db.transaction(async (tx) => {
  //     if (userMinorsToAdd.length > 0) {
  //       await tx.insert(userMinor).values(userMinorsToAdd);
  //     }
  //     console.log(`Inserted ${userMinorsToAdd.length} minors`);
  //   });
  // } catch (error) {
  //   console.error('Failed to insert majors:', error);
  //   throw error;
  // }

  // execute queries to add rows into userMajor and userMinor
  // COMMENTED OUT SO IT WON'T TRIGGER
  // await db.transaction(async (tx) => {
  //   const addUserMajorQueries = userMajorsToAdd.map(async (major) => {
  //     await tx.insert(userMajor).values(major);
  //     console.log(`Inserted ${major.majorId} with specialization ${major.specializationId} for ${major.userId}`);
  //   });
  //   await Promise.all(addUserMajorQueries);
  // });
  // await db.transaction(async (tx) => {
  //   const addUserMinorQueries = userMinorsToAdd.map(async (minor) => {
  //     await tx.insert(userMinor).values(minor);
  //     console.log(`Inserted ${minor.minorId} for ${minor.userId}`);
  //   });
  //   await Promise.all(addUserMinorQueries);
  // });

  // firstPlanner should be an array of FirstPlannerRow types
  // for (const { userId, plannerId } of firstPlanner) {
  //   const majors = await db
  //     .select({
  //       majorId: plannerMajor.majorId,
  //       specializationId: plannerMajor.specializationId,
  //     })
  //     .from(plannerMajor)
  //     .where(eq(plannerMajor.plannerId, plannerId));

  //   const minors = await db
  //     .select({
  //       minorId: plannerMinor.minorId,
  //     })
  //     .from(plannerMinor)
  //     .where(and(eq(plannerMinor.plannerId, plannerId), isNotNull(plannerMinor.minorId)));

  //   console.log(`User ${userId} (planner ${plannerId})`);
  //   console.log('  majors:', majors);
  //   console.log('  minors:', minors);
  // }
}

transferUserMajorMinors();
