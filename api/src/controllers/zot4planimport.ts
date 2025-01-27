/**
 @module Zot4PlanImportRoute
*/

import { z } from 'zod';
import { publicProcedure, router } from '../helpers/trpc';
import { TRPCError } from '@trpc/server';
import { SavedRoadmap, SavedPlannerQuarterData, QuarterName } from '@peterportal/types';

type Zot4PlanSchedule = {
  years: string[][][];
  selectedPrograms: {
    value: number;
    label: string;
    is_major: boolean;
  }[][];
  addedCourses: [];
  courses: [];
  apExam: {
    id: number;
    name: string;
    score: number;
    courses: string[];
    GE: [];
    units: number;
  }[];
};

/**
 * Get a JSON schedule from Zot4Plan by name; return an empty {} if it does not exist
 */
const getFromZot4Plan = async (scheduleName: string) => {
  let res = {};
  await fetch('https://api.zot4plan.com/api/loadSchedule/' + scheduleName, {
    method: 'PUT',
  })
    .then((response) => {
      if (!response.ok) {
        throw new TRPCError({
          code: 'BAD_REQUEST',
          message: 'Schedule name could not be obtained from Zot4Plan',
        });
      }
      return response.json();
    })
    .then((json) => {
      res = json;
    });
  return res as Zot4PlanSchedule;
};

const zot4PlanImportRouter = router({
  /**
   * Get a roadmap formatted for PeterPortal based on a Zot4Plan schedule by name
   * and labeled with years based on the current year and the student's year
   */
  getScheduleFormatted: publicProcedure
    .input(z.object({ scheduleName: z.string(), studentYear: z.string() }))
    .query(async ({ input }) => {
      // Get the raw schedule data
      const originalScheduleRaw = await getFromZot4Plan(input.scheduleName);

      // Convert it to the PeterPortal roadmap format
      const converted: SavedRoadmap = {
        planners: [
          {
            name: input.scheduleName,
            content: [],
          },
        ],
        transfers: [],
      };
      // Determine the start year of the first quarter
      let startYear = new Date().getFullYear();
      startYear -= parseInt(input.studentYear);
      if (new Date().getMonth() >= 7) startYear += 1; // First-years in Fall start this year
      // Add courses
      for (let i = 0; i < originalScheduleRaw.years.length; i++) {
        // Convert year
        const year = originalScheduleRaw.years[i];
        const quartersList: SavedPlannerQuarterData[] = [];
        for (let j = 0; j < year.length; j++) {
          // Convert quarter
          const quarter = year[j];
          const courses: string[] = [];
          for (let k = 0; k < quarter.length; k++) {
            // Convert course
            // (PeterPortal course IDs are the same as Zot4Plan course IDs except all spaces are removed)
            const originalCourseName = quarter[k];
            const transformedCourseName = originalCourseName.replace(/\s/g, '');
            courses.push(transformedCourseName);
          }
          if (j >= 3 && courses.length == 0) {
            // Do not include the summer quarter if it has no courses (it is irrelevant)
            continue;
          }
          quartersList.push({
            name: ['Fall', 'Winter', 'Spring', 'Summer1', 'Summer2', 'Summer10wk'][Math.min(j, 5)] as QuarterName,
            courses: courses,
          });
        }
        converted.planners[0].content.push({
          startYear: startYear + i,
          name: 'Year ' + (i + 1),
          quarters: quartersList,
        });
      }
      // Trim trailing empty years other than the first year
      // (do not trim empty years in the middle because that makes it hard to add years there)
      while (converted.planners[0].content.length > 1) {
        let yearHasCourses = false;
        for (const quarter of converted.planners[0].content[converted.planners[0].content.length - 1].quarters) {
          if (quarter.courses.length != 0) {
            yearHasCourses = true;
          }
        }
        if (!yearHasCourses) {
          // The year does not have courses, so trim it
          converted.planners[0].content.pop();
        } else {
          // The year does have courses, so we are done
          break;
        }
      }

      return converted as SavedRoadmap;
    }),
});

export default zot4PlanImportRouter;
