import {
  LegacyRoadmap,
  Prerequisite,
  PrerequisiteTree,
  QuarterName,
  quarters,
  SavedPlannerData,
  SavedPlannerQuarterData,
  SavedPlannerYearData,
  SavedRoadmap,
  TransferData,
} from '@peterportal/types';
import { searchAPIResults } from './util';
import { RoadmapPlan, defaultPlan } from '../store/slices/roadmapSlice';
import {
  BatchCourseData,
  CourseGQLData,
  InvalidCourseData,
  PlannerData,
  PlannerQuarterData,
  PlannerYearData,
} from '../types/types';
import trpc from '../trpc';
import { TransferredCourse, UserAPExam } from '../store/slices/transferCreditsSlice';
import { LocalTransferSaveKey, saveLocalTransfers } from './transferCredits';
import { UncategorizedCourseEntry } from '../pages/RoadmapPage/transfers/UncategorizedCreditsSection';
import spawnToast from './toastify';

export function defaultYear() {
  const quarterNames: QuarterName[] = ['Fall', 'Winter', 'Spring'];
  return {
    startYear: new Date().getFullYear(),
    name: 'Year 1',
    quarters: quarterNames.map((quarter) => {
      return { name: quarter, courses: [] };
    }),
  } as PlannerYearData | SavedPlannerYearData;
}

export const quarterDisplayNames: Record<QuarterName, string> = {
  Fall: 'Fall',
  Winter: 'Winter',
  Spring: 'Spring',
  Summer1: 'Summer I',
  Summer2: 'Summer II',
  Summer10wk: 'Summer 10 Week',
};

export function normalizeQuarterName(name: string): QuarterName {
  if (quarters.includes(name as QuarterName)) return name as QuarterName;
  const lookup: { [k: string]: QuarterName } = {
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
  if (!lookup[name]) throw TypeError('Invalid Quarter Name: ' + name);
  return lookup[name];
}

export const makeUniquePlanName = (plannerName: string, allPlans: RoadmapPlan[]): string => {
  let newName = plannerName;
  while (allPlans.find((p) => p.name === newName)) {
    // The regex matches an integer number at the end
    const counter = newName.match(/\d+$/);
    if (counter != null) {
      const numberValue = newName.substring(counter.index!);
      newName = newName.substring(0, counter.index) + (parseInt(numberValue) + 1);
    } else {
      // No number exists at the end, so default with 2
      newName += ' 2';
    }
  }
  return newName;
};

// remove all unecessary data to store into the database
export const collapsePlanner = (planner: PlannerData): SavedPlannerYearData[] => {
  const savedPlanner: SavedPlannerYearData[] = [];
  planner.forEach((year) => {
    const savedYear: SavedPlannerYearData = { startYear: year.startYear, name: year.name, quarters: [] };
    year.quarters.forEach((quarter) => {
      const savedQuarter: SavedPlannerQuarterData = { name: quarter.name, courses: [] };
      savedQuarter.courses = quarter.courses.map((course) => course.id);
      savedYear.quarters.push(savedQuarter);
    });
    savedPlanner.push(savedYear);
  });
  return savedPlanner;
};

export const collapseAllPlanners = (plans: RoadmapPlan[]): SavedPlannerData[] => {
  return plans.map((p) => ({
    ...(p.id ? { id: p.id } : {}),
    name: p.name,
    content: collapsePlanner(p.content.yearPlans),
  }));
};

// query the lost information from collapsing

export const expandPlanner = async (savedPlanner: SavedPlannerYearData[]): Promise<PlannerData> => {
  let courses: string[] = [];
  // get all courses in the planner
  savedPlanner.forEach((year) =>
    year.quarters.forEach((quarter) => {
      courses = courses.concat(quarter.courses);
    }),
  );
  // get the course data for all courses
  let courseLookup: BatchCourseData = {};
  // only send request if there are courses
  if (courses.length > 0) {
    courseLookup = await searchAPIResults('courses', courses);
  }

  return new Promise((resolve) => {
    const planner: PlannerData = [];
    savedPlanner.forEach((savedYear) => {
      const year: PlannerYearData = { startYear: savedYear.startYear, name: savedYear.name, quarters: [] };
      savedYear.quarters.forEach((savedQuarter) => {
        const quarter: PlannerQuarterData = { name: savedQuarter.name, courses: [] };
        quarter.courses = savedQuarter.courses.map((course) => courseLookup[course]);
        year.quarters.push(quarter);
      });
      planner.push(year);
    });
    resolve(planner);
  });
};

export const expandAllPlanners = async (plans: SavedPlannerData[]): Promise<RoadmapPlan[]> => {
  return await Promise.all(
    plans.map(async (p) => {
      const content = await expandPlanner(p.content);
      return { ...(p.id ? { id: p.id } : {}), name: p.name, content: { yearPlans: content, invalidCourses: [] } };
    }),
  );
};

function loadLocalRoadmap(): SavedRoadmap | LegacyRoadmap {
  let localRoadmap: SavedRoadmap | LegacyRoadmap | null = null;
  try {
    localRoadmap = JSON.parse(localStorage.roadmap);
  } catch {
    /* ignore */
  }
  const EMPTY_ROADMAP_STR = JSON.stringify({
    planners: [{ name: defaultPlan.name, content: [defaultYear()] }],
    transfers: [],
  } as Omit<SavedRoadmap, 'timestamp'>);

  return localRoadmap ?? JSON.parse(EMPTY_ROADMAP_STR);
}

export const loadRoadmap = async (isLoggedIn: boolean) => {
  const accountRoadmap = isLoggedIn ? ((await trpc.roadmaps.get.query()) ?? null) : null;
  const localRoadmap = loadLocalRoadmap() as SavedRoadmap;
  return { accountRoadmap, localRoadmap };
};

// Adding Multiplan
function addMultiPlanToRoadmap(roadmap: SavedRoadmap | LegacyRoadmap): SavedRoadmap {
  if ('planners' in roadmap) {
    // if already in multiplanner format, everything is good
    return roadmap;
  } else {
    // if not, convert to multiplanner format, also normalize quarter names
    return {
      planners: [
        {
          name: defaultPlan.name,
          content: normalizePlannerQuarterNames((roadmap as { planner: SavedPlannerYearData[] }).planner),
        },
      ],
      transfers: roadmap.transfers,
      timestamp: roadmap.timestamp,
    };
  }
}

enum RoadmapVersionKey {
  SinglePlanner = '1',
  MultiPlanner = '2',
  RedesignedTransfers = '3',
}

// Upgrading Transfers
async function saveUpgradedTransfers(roadmapToSave: SavedRoadmap, transfers: TransferData[]) {
  // Avoid issues with double first render
  if (!transfers.length || localStorage.roadmap__versionKey === RoadmapVersionKey.RedesignedTransfers) {
    return false; // nothing to convert
  }

  localStorage.roadmap__versionKey = RoadmapVersionKey.RedesignedTransfers;
  const response = await trpc.transferCredits.convertUserLegacyTransfers.query(transfers);
  const { courses, ap, other } = response;

  const scoredAPs = ap.map(({ score, ...other }) => ({ ...other, score: score ?? 1 }));
  const formattedOther = other.map(({ courseName: name, units }) => ({ name, units }));

  saveLocalTransfers<TransferredCourse>(LocalTransferSaveKey.Course, courses);
  saveLocalTransfers<UserAPExam>(LocalTransferSaveKey.AP, scoredAPs);
  saveLocalTransfers<UncategorizedCourseEntry>(LocalTransferSaveKey.Uncategorized, formattedOther);

  // immediately update localStorage to not have transfers, now that we've converted them
  localStorage.setItem('roadmap', JSON.stringify(roadmapToSave));
  return true;
}

/**
 * Updates the format of transferred credits in localStorage before data is used by other parts of the app
 * @param roadmap The roadmap whose transfers to upgrade
 */
async function upgradeLegacyTransfers(roadmap: SavedRoadmap): Promise<SavedRoadmap> {
  const legacyTransfers = roadmap.transfers;
  const updatedRoadmap = { ...roadmap, transfers: [] };
  const complete = await saveUpgradedTransfers(updatedRoadmap, legacyTransfers);
  return complete ? updatedRoadmap : roadmap;
}

// Upgrading Entire Roadmap
/**
 * Function to upgrade a local roadmap. Gets called BEFORE user data is loaded
 */
export async function upgradeLocalRoadmap(): Promise<SavedRoadmap> {
  const localRoadmap = loadLocalRoadmap();
  const roadmapWithMultiPlan = addMultiPlanToRoadmap(localRoadmap);
  const roadmapWithoutLegacyTransfers = await upgradeLegacyTransfers(roadmapWithMultiPlan);
  return roadmapWithoutLegacyTransfers;
}

export const saveRoadmap = async (isLoggedIn: boolean, planners: SavedPlannerData[], showToasts: boolean = true) => {
  const roadmap: SavedRoadmap = { timestamp: new Date().toISOString(), planners, transfers: [] };
  localStorage.setItem('roadmap', JSON.stringify(roadmap));

  const showMessage = showToasts ? spawnToast : (str: string) => str;

  const SAVED_LOCALLY_MESSAGE = 'Roadmap saved locally! Log in to save it to your account.';
  if (!isLoggedIn) return showMessage(SAVED_LOCALLY_MESSAGE);

  await trpc.roadmaps.save
    .mutate(roadmap)
    .then(() => showMessage('Roadmap saved to your account!'))
    .catch(() => showMessage(SAVED_LOCALLY_MESSAGE));
};

function normalizePlannerQuarterNames(yearPlans: SavedPlannerYearData[]) {
  return yearPlans.map((year) => ({
    ...year,
    quarters: year.quarters.map((quarter) => ({ ...quarter, name: normalizeQuarterName(quarter.name) })),
  }));
}

type PrerequisiteNode = Prerequisite | PrerequisiteTree;

type plannerCallback = (missing: Set<string>, invalidCourses: InvalidCourseData[]) => void;

export const validatePlanner = (transferNames: string[], currentPlanData: PlannerData, handler: plannerCallback) => {
  const { missing, invalidCourses } = validatePlannerV2(transferNames, currentPlanData);
  handler(missing, invalidCourses);
};

export const validatePlannerV2 = (transferNames: string[], currentPlanData: PlannerData) => {
  // store courses that have been taken
  // Transferred courses use ID (no spaces), AP Exams use Catalogue Name
  const taken: Set<string> = new Set(transferNames);
  const invalidCourses: InvalidCourseData[] = [];
  const missing = new Set<string>();
  currentPlanData.forEach((year, yi) => {
    year.quarters.forEach((quarter, qi) => {
      const taking: Set<string> = new Set(
        quarter.courses.map((course) => course.department + ' ' + course.courseNumber),
      );
      quarter.courses.forEach((course, ci) => {
        // if has prerequisite
        if (course.prerequisiteTree) {
          const required = validateCourse(taken, course.prerequisiteTree, taking, course.corequisites);
          // prerequisite not fulfilled, has some required classes to take
          if (required.size > 0) {
            invalidCourses.push({
              location: {
                yearIndex: yi,
                quarterIndex: qi,
                courseIndex: ci,
              },
              required: Array.from(required),
            });

            required.forEach((course) => {
              missing.add(course);
            });
          }
        }
      });
      // after the quarter is over, add the courses into taken
      taking.forEach((course) => taken.add(course));
    });
  });

  return { missing, invalidCourses };
};

export const getAllCoursesFromPlan = (plan: RoadmapPlan['content']) => {
  return plan.yearPlans.flatMap((yearPlan) =>
    yearPlan.quarters.flatMap((quarter) =>
      quarter.courses.map((course) => course.department + ' ' + course.courseNumber),
    ),
  );
};

// returns set of courses that need to be taken to fulfill requirements
export const validateCourse = (
  taken: Set<string>,
  prerequisite: PrerequisiteNode,
  taking: Set<string>,
  corequisite: string,
): Set<string> => {
  // base case just a course
  if ('prereqType' in prerequisite) {
    const id = prerequisite.prereqType === 'course' ? prerequisite.courseId : prerequisite.examName;
    // already taken prerequisite or is currently taking the corequisite
    if (taken.has(id) || (corequisite.includes(id) && taking.has(id))) {
      return new Set();
    }
    // need to take this prerequisite still
    else {
      return new Set([id]);
    }
  }
  // has nested prerequisites
  else {
    // needs to satisfy all nested
    if (prerequisite.AND) {
      const required: Set<string> = new Set();
      prerequisite.AND.forEach((nested) => {
        // combine all the courses that are required
        validateCourse(taken, nested, taking, corequisite).forEach((course) => required.add(course));
      });
      return required;
    }
    // only need to satisfy one nested
    else if (prerequisite.OR) {
      const required: Set<string> = new Set();
      let satisfied = false;
      prerequisite.OR.forEach((nested) => {
        // combine all the courses that are required
        const courses = validateCourse(taken, nested, taking, corequisite);
        // if one is satisfied, no other courses are required
        if (courses.size == 0) {
          satisfied = true;
          return;
        }
        courses.forEach((course) => required.add(course));
      });
      return satisfied ? new Set() : required;
    } else {
      // should never reach here
      return new Set();
    }
  }
};

export const getMissingPrerequisites = (clearedCourses: Set<string>, course: CourseGQLData) => {
  const missingPrerequisites = Array.from(
    validateCourse(clearedCourses, course.prerequisiteTree, new Set(), course.corequisites),
  );
  return missingPrerequisites.length ? missingPrerequisites : undefined;
};
