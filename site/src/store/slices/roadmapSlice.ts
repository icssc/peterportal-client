import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { defaultYear } from '../../helpers/planner';
import {
  CourseGQLData,
  CourseIdentifier,
  InvalidCourseData,
  PlannerQuarterData,
  PlannerYearData,
  QuarterIdentifier,
  RoadmapPlan,
  RoadmapPlanState,
  RoadmapRevision,
  YearIdentifier,
} from '../../types/types';
import type { RootState } from '../store';
import spawnToast from '../../helpers/toastify';
import { quarters } from '@peterportal/types';
import { restoreRevision } from '../../helpers/roadmap';

// Define the initial state using that type
export const initialPlanState: RoadmapPlanState = {
  yearPlans: [defaultYear() as PlannerYearData],
  invalidCourses: [],
};

interface RoadmapPlanIdentifier {
  planIndex: number;
}

// default plan to display for uesr
export const defaultPlan: RoadmapPlan = {
  id: -1,
  name: "Peter's Roadmap",
  content: initialPlanState,
};

/** added for multiple planner */

// Payload to pass in to move a course
export interface MoveCoursePayload {
  from: CourseIdentifier;
  to: CourseIdentifier;
}

// Payload to pass in to add a year
interface AddYearPayload {
  yearData: PlannerYearData;
}

// Payload to padd in to edit a year
interface EditYearPayload {
  startYear: number;
  index: number;
}

// Payload to pass in to edit a year name
interface EditNamePayload {
  name: string;
  index: number;
}

// Payload to pass in to add a quarter
interface AddQuarterPayload {
  startYear: number;
  quarterData: PlannerQuarterData;
}

const baseRevision: RoadmapRevision = {
  timestamp: Date.now(),
  edits: [],
};

// onbeforeunload event listener
const alertUnsaved = (event: BeforeUnloadEvent) => event.preventDefault();

export const roadmapSlice = createSlice({
  name: 'roadmap',
  initialState: {
    plans: [defaultPlan],
    revisions: [baseRevision],
    currentRevisionIndex: 0,
    currentPlanIndex: 0,
    /** Whether to alert the user of unsaved changes before leaving */
    unsavedChanges: false,
    /** Selected quarter and year for adding a course on mobile */
    currentYearAndQuarter: null as { year: number; quarter: number } | null,
    /** Whether to show the search bar on mobile */
    showSearch: false,
    /** Whether to show the add course modal on mobile */
    showAddCourse: false,
    showSavedCourses: true,
    /** Store the course data of the active dragging item */
    activeCourse: undefined as CourseGQLData | undefined,
    /** true if we start dragging a course whose info hasn't fully loaded yet, i.e. from Degree Requirements */
    activeCourseLoading: false,
    /** Store missing prerequisites for courses when adding on mobile */
    activeMissingPrerequisites: undefined as string[] | undefined,
    /** Whether the roadmap is loading */
    roadmapLoading: false,
  },
  reducers: {
    // Use the PayloadAction type to declare the contents of `action.payload`
    moveCourse: (state, action: PayloadAction<MoveCoursePayload>) => {
      const toYear = action.payload.to.yearIndex;
      const toQuarter = action.payload.to.quarterIndex;
      const toCourse = action.payload.to.courseIndex;
      const fromYear = action.payload.from.yearIndex;
      const fromQuarter = action.payload.from.quarterIndex;
      const fromCourse = action.payload.from.courseIndex;

      let removed: CourseGQLData | undefined;
      // not from the searchbar
      if (fromYear != -1) {
        // remove course from list
        const courseList =
          state.plans[state.currentPlanIndex].content.yearPlans[fromYear].quarters[fromQuarter].courses;
        [removed] = courseList.splice(fromCourse, 1);
      }
      // from the searchbar
      else {
        // active course has the current dragging course
        removed = state.activeCourse;
      }

      // add course to list
      const courseList = state.plans[state.currentPlanIndex].content.yearPlans[toYear].quarters[toQuarter].courses;
      courseList.splice(toCourse, 0, removed!);
    },
    deleteCourse: (state, action: PayloadAction<CourseIdentifier>) => {
      const yearPlan = state.plans[state.currentPlanIndex].content.yearPlans[action.payload.yearIndex];
      const quarter = yearPlan.quarters[action.payload.quarterIndex];
      quarter.courses.splice(action.payload.courseIndex, 1);
    },
    addQuarter: (state, action: PayloadAction<AddQuarterPayload>) => {
      const yearPlans = state.plans[state.currentPlanIndex].content.yearPlans;
      const yearPlan = yearPlans.find((plan) => plan.startYear === action.payload.startYear);

      if (!yearPlan) return;

      const newQuarter = action.payload.quarterData;
      const currentQuarters = yearPlan.quarters.map((quarter) => quarter.name);
      const addBefore = currentQuarters.findIndex((name) => quarters.indexOf(newQuarter.name) < quarters.indexOf(name));
      const index = addBefore === -1 ? currentQuarters.length : addBefore;

      yearPlan.quarters.splice(index, 0, newQuarter);
    },
    deleteQuarter: (state, action: PayloadAction<QuarterIdentifier>) => {
      const yearPlan = state.plans[state.currentPlanIndex].content.yearPlans[action.payload.yearIndex];
      yearPlan.quarters.splice(action.payload.quarterIndex, 1);
    },
    addYear: (state, action: PayloadAction<AddYearPayload>) => {
      const currentYears = state.plans[state.currentPlanIndex].content.yearPlans.map((e) => e.startYear);
      const newYear = action.payload.yearData.startYear;
      const currentNames = state.plans[state.currentPlanIndex].content.yearPlans.map((e) => e.name);
      const newName = action.payload.yearData.name;
      // if duplicate year
      if (currentYears.includes(newYear)) {
        spawnToast(
          `${newYear}-${newYear + 1} has already been added as Year ${currentYears.indexOf(newYear) + 1}!`,
          true,
        );
        return;
      }
      // if duplicate name
      if (currentNames.includes(newName)) {
        const year = state.plans[state.currentPlanIndex].content.yearPlans[currentNames.indexOf(newName)].startYear;
        spawnToast(`${newName} already exists from ${year} - ${year + 1}!`, true);
        return;
      }

      // check if where to put newYear
      let index = currentYears.length;
      for (let i = 0; i < currentYears.length; i++) {
        if (currentYears[i] > newYear) {
          index = i;
          break;
        }
      }

      state.plans[state.currentPlanIndex].content.yearPlans.splice(index, 0, action.payload.yearData);
    },
    editYear: (state, action: PayloadAction<EditYearPayload>) => {
      const currentYears = state.plans[state.currentPlanIndex].content.yearPlans.map((e) => e.startYear);
      const newYear = action.payload.startYear;
      const yearIndex = action.payload.index;
      // if duplicate year
      if (currentYears.includes(newYear)) {
        spawnToast(`${newYear}-${newYear + 1} already exists as Year ${currentYears.indexOf(newYear) + 1}!`, true);
        return;
      }

      // edit year & sort years
      state.plans[state.currentPlanIndex].content.yearPlans[yearIndex].startYear = newYear;
      state.plans[state.currentPlanIndex].content.yearPlans.sort((a, b) => a.startYear - b.startYear);
    },
    deleteYear: (state, action: PayloadAction<YearIdentifier>) => {
      state.plans[state.currentPlanIndex].content.yearPlans.splice(action.payload.yearIndex, 1);
    },
    editName: (state, action: PayloadAction<EditNamePayload>) => {
      const currentNames = state.plans[state.currentPlanIndex].content.yearPlans.map((e) => e.name);
      const newName = action.payload.name;
      const yearIndex = action.payload.index;
      // if duplicate name
      if (currentNames.includes(newName)) {
        const year = state.plans[state.currentPlanIndex].content.yearPlans[yearIndex].startYear;
        spawnToast(`${newName} already exists from ${year} - ${year + 1}!`, true);
        return;
      }

      // edit name
      state.plans[state.currentPlanIndex].content.yearPlans[yearIndex].name = newName;
    },
    setActiveCourse: (state, action: PayloadAction<CourseGQLData | undefined>) => {
      state.activeCourse = action.payload;
    },
    setActiveCourseLoading: (state, action: PayloadAction<boolean>) => {
      state.activeCourseLoading = action.payload;
    },
    setActiveMissingPrerequisites: (state, action: PayloadAction<string[] | undefined>) => {
      state.activeMissingPrerequisites = action.payload;
    },
    setAllPlans: (state, action: PayloadAction<RoadmapPlan[]>) => {
      state.plans = action.payload;
    },
    setInvalidCourses: (state, action: PayloadAction<InvalidCourseData[]>) => {
      state.plans[state.currentPlanIndex].content.invalidCourses = action.payload;
    },
    setShowSearch: (state, action: PayloadAction<{ show: boolean; year?: number; quarter?: number }>) => {
      state.showSearch = action.payload.show;
      if (action.payload.year !== undefined && action.payload.quarter !== undefined) {
        state.currentYearAndQuarter = { year: action.payload.year, quarter: action.payload.quarter };
      }
    },
    setShowAddCourse: (state, action: PayloadAction<boolean>) => {
      state.showAddCourse = action.payload;
    },
    deleteRoadmapPlan: (state, action: PayloadAction<RoadmapPlanIdentifier>) => {
      state.plans.splice(action.payload.planIndex, 1);
      if (state.plans.length === 0) {
        state.plans.push(defaultPlan);
      }
    },
    setPlanIndex: (state, action: PayloadAction<number>) => {
      state.currentPlanIndex = action.payload;
    },
    setShowSavedCourses: (state, action: PayloadAction<boolean>) => {
      state.showSavedCourses = action.payload;
    },
    setUnsavedChanges: (state, action: PayloadAction<boolean>) => {
      state.unsavedChanges = action.payload;

      // when there are unsaved changes, add event listener for alert on page leave
      if (state.unsavedChanges) {
        window.addEventListener('beforeunload', alertUnsaved);
      } else {
        // remove listener after saving changes
        window.removeEventListener('beforeunload', alertUnsaved);
      }
    },
    setRoadmapLoading: (state, action: PayloadAction<boolean>) => {
      state.roadmapLoading = action.payload;
    },
    // create a revision
    reviseRoadmap: (state, action: PayloadAction<RoadmapRevision>) => {
      const currentIndex = state.currentRevisionIndex;
      state.revisions.splice(currentIndex + 1, state.revisions.length, action.payload);
      restoreRevision(state.plans, state.revisions, currentIndex, currentIndex + 1);
      state.currentRevisionIndex++;
    },
    undoRoadmapRevision: (state) => {
      restoreRevision(state.plans, state.revisions, state.currentRevisionIndex, state.currentRevisionIndex - 1);
      state.currentRevisionIndex--;
    },
    redoRoadmapRevision: (state) => {
      restoreRevision(state.plans, state.revisions, state.currentRevisionIndex, state.currentRevisionIndex + 1);
      state.currentRevisionIndex++;
    },
  },
});

export const {
  moveCourse,
  deleteCourse,
  addQuarter,
  deleteQuarter,
  addYear,
  editYear,
  editName,
  deleteYear,
  setActiveCourse,
  setActiveCourseLoading,
  setActiveMissingPrerequisites,
  setAllPlans,
  setInvalidCourses,
  setShowSearch,
  setShowAddCourse,
  deleteRoadmapPlan,
  setPlanIndex,
  setUnsavedChanges,
  setShowSavedCourses,
  setRoadmapLoading,
  reviseRoadmap,
  undoRoadmapRevision,
  redoRoadmapRevision,
} = roadmapSlice.actions;

// Other code such as selectors can use the imported `RootState` type
export const selectYearPlans = (state: RootState) =>
  state.roadmap.plans[state.roadmap.currentPlanIndex].content.yearPlans;

export const selectAllPlans = (state: RootState) => state.roadmap.plans;

export const getNextPlannerTempId = (state: RootState) => {
  return Math.min(0, ...state.roadmap.plans.map((p) => p.id)) - 1;
};

export default roadmapSlice.reducer;
