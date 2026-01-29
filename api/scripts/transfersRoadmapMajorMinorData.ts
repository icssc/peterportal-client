import dotenv from 'dotenv-flow';
import { planner, plannerMajor /*plannerMinor, userMajor, userMinor*/ } from '../src/db/schema';
import { db } from '../src/db';

// load env (because this is a separate script)
dotenv.config();

// Script for moving items from plannerMajor and plannerMinor into
// new tables userMajor and userMinor so that major and minor are linked to
// the user instead of a roadmap

// NOTE: you can use `npx tsx ./scripts/transfersRoadmapMajorMinorData.ts` to run

async function transferUserMajor() {
  const allPlanners = await db.select().from(planner);
  const allPlannerMajors = await db.select().from(plannerMajor);

  // might delete
  // // sort planners by userId to categorize and then by plannerId
  // const sortedPlanners = allPlanners.sort((planA, planB) => {
  //   if(planA.userId == planB.userId) {
  //     return planA.id - planB.id;
  //   }
  //   else {
  //     return planA.userId - planB.userId;
  //   }
  // });
  // console.log(sortedPlanners);

  const userMajorsToAdd = allPlannerMajors.map((plannerMajor) => {
    return {
      id: plannerMajor.id,
      userId: allPlanners.find((planner) => planner.id === plannerMajor.plannerId)?.userId,
      majorId: plannerMajor.majorId,
      specializationId: plannerMajor.specializationId,
    };
  });
  console.log(userMajorsToAdd);
}

transferUserMajor();
