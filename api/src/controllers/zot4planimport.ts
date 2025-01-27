/**
 @module Zot4PlanImportRoute
*/

import { z } from 'zod';
import { publicProcedure, router } from '../helpers/trpc';
import { TRPCError } from '@trpc/server';
import { SavedRoadmap, SavedPlannerData, SavedPlannerQuarterData, QuarterName } from '@peterportal/types';

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
 * Get a JSON schedule from Zot4Plan by name
 * Throw an error if it does not exist
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

/**
 * Convert a Zot4Plan course name into a PeterPortal course ID
 */
const convertIntoCourseID = (zot4PlanCourse: string): string => {
  // PeterPortal course IDs are the same as Zot4Plan course IDs except all spaces are removed
  return zot4PlanCourse.replace(/\s/g, '');
};

/**
 * Trim the empty years off the end of a saved roadmap planner
 * (Other than the first year)
 */
const trimEmptyYears = (planner: SavedPlannerData) => {
  // Empty years in the middle aren't trimmed because that makes it hard to add years there
  while (planner.content.length > 1) {
    let yearHasCourses = false;
    for (const quarter of planner.content[planner.content.length - 1].quarters) {
      if (quarter.courses.length != 0) {
        yearHasCourses = true;
      }
    }
    if (!yearHasCourses) {
      // The year does not have courses, so trim it
      planner.content.pop();
    } else {
      // The year does have courses, so we are done
      break;
    }
  }
};

/**
 * Determine a student's start year based on their current year in school
 * (ex. a first-year's current year is "1")
 */
const getStartYear = (studentYear: string): number => {
  let startYear = new Date().getFullYear();
  startYear -= parseInt(studentYear);
  // First-years in Fall start this year, not the previous year
  if (new Date().getMonth() >= 7) startYear += 1;
  return startYear;
};

/**
 * Convert a Zot4Plan schedule into a saved roadmap planner
 */
const convertIntoRoadmapPlanner = (
  originalSchedule: Zot4PlanSchedule,
  scheduleName: string,
  startYear: number,
): SavedPlannerData => {
  const converted: SavedPlannerData = {
    name: scheduleName,
    content: [],
  };

  // Add courses
  for (let i = 0; i < originalSchedule.years.length; i++) {
    // Convert year
    const year = originalSchedule.years[i];
    const quartersList: SavedPlannerQuarterData[] = [];
    for (let j = 0; j < year.length; j++) {
      // Convert quarter
      const quarter = year[j];
      const courses: string[] = [];
      for (let k = 0; k < quarter.length; k++) {
        // Convert course
        courses.push(convertIntoCourseID(quarter[k]));
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
    converted.content.push({
      startYear: startYear + i,
      name: 'Year ' + (i + 1),
      quarters: quartersList,
    });
  }
  // Trim trailing years
  trimEmptyYears(converted);

  return converted;
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
      const convertedPlanner = convertIntoRoadmapPlanner(
        originalScheduleRaw,
        input.scheduleName,
        getStartYear(input.studentYear),
      );
      const res: SavedRoadmap = {
        planners: [convertedPlanner],
        transfers: [],
      };

      return res;
    }),
});

export default zot4PlanImportRouter;
