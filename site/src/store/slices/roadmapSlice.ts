import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { defaultYear } from '../../helpers/planner';
import {
  CourseGQLData,
  CourseIdentifier,
  InvalidCourseData,
  PlannerData,
  PlannerQuarterData,
  PlannerYearData,
  QuarterIdentifier,
  YearIdentifier,
} from '../../types/types';
import type { RootState } from '../store';
import spawnToast from '../../helpers/toastify';
import { quarters } from '@peterportal/types';

// Define a type for the slice state
interface RoadmapPlanState {
  // Store planner data
  yearPlans: PlannerData;
  // Store the location of invalid courses (do not meet prerequisites)
  invalidCourses: InvalidCourseData[];
}

// Define the initial state using that type
export const initialPlanState: RoadmapPlanState = {
  yearPlans: [defaultYear() as PlannerYearData],
  invalidCourses: [],
};

/** added for multiple planner */
// create roadmap plan object
export interface RoadmapPlan {
  id?: number;
  name: string;
  content: RoadmapPlanState;
}

interface RoadmapPlanIdentifier {
  planIndex: number;
}

interface SetPlanNamePayload {
  index: number;
  name: string;
}

// default plan to display for uesr
export const defaultPlan: RoadmapPlan = {
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

// onbeforeunload event listener
const alertUnsaved = (event: BeforeUnloadEvent) => event.preventDefault();

export const roadmapSlice = createSlice({
  name: 'roadmap',
  initialState: {
    plans: [defaultPlan],
    currentPlanIndex: 0,
    /** Whether to alert the user of unsaved changes before leaving */
    unsavedChanges: false,
    /** Selected quarter and year for adding a course on mobile */
    currentYearAndQuarter: null as { year: number; quarter: number } | null,
    /** Whether to show the search bar on mobile */
    showSearch: false,
    /** Whether to show the add course modal on mobile */
    showAddCourse: false,
    showCourseBag: true,
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
    clearQuarter: (state, action: PayloadAction<QuarterIdentifier>) => {
      const yearPlan = state.plans[state.currentPlanIndex].content.yearPlans[action.payload.yearIndex];
      const quarter = yearPlan.quarters[action.payload.quarterIndex];
      quarter.courses = [];
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
    clearYear: (state, action: PayloadAction<YearIdentifier>) => {
      const yearPlan = state.plans[state.currentPlanIndex].content.yearPlans[action.payload.yearIndex];
      yearPlan.quarters.forEach((q) => {
        q.courses = [];
      });
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
    setYearPlans: (state, action: PayloadAction<PlannerData>) => {
      state.plans[state.currentPlanIndex].content.yearPlans = action.payload;
    },
    setAllPlans: (state, action: PayloadAction<RoadmapPlan[]>) => {
      state.plans = action.payload;
    },
    setInvalidCourses: (state, action: PayloadAction<InvalidCourseData[]>) => {
      state.plans[state.currentPlanIndex].content.invalidCourses = action.payload;
    },
    setShowSearch: (state, action: PayloadAction<{ show: boolean; year?: number; quarter?: number }>) => {
      state.showSearch = action.payload.show;
      if (action.payload.year && action.payload.quarter) {
        state.currentYearAndQuarter = { year: action.payload.year, quarter: action.payload.quarter };
      }
    },
    setShowAddCourse: (state, action: PayloadAction<boolean>) => {
      state.showAddCourse = action.payload;
    },
    /** added for multiple plans */
    setRoadmapPlan: (state, action: PayloadAction<{ plans: RoadmapPlan[] }>) => {
      state.plans = action.payload.plans;
    },
    addRoadmapPlan: (state, action: PayloadAction<RoadmapPlan>) => {
      state.plans.push(action.payload);
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
    setPlanName: (state, action: PayloadAction<SetPlanNamePayload>) => {
      const index = action.payload.index;
      state.plans[index].name = action.payload.name;
    },
    setShowCourseBag: (state, action: PayloadAction<boolean>) => {
      state.showCourseBag = action.payload;
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
  },
});

export const {
  moveCourse,
  deleteCourse,
  addQuarter,
  deleteQuarter,
  clearQuarter,
  clearYear,
  addYear,
  editYear,
  editName,
  deleteYear,
  setActiveCourse,
  setActiveCourseLoading,
  setActiveMissingPrerequisites,
  setYearPlans,
  setAllPlans,
  setInvalidCourses,
  setShowSearch,
  setShowAddCourse,
  setRoadmapPlan,
  addRoadmapPlan,
  deleteRoadmapPlan,
  setPlanIndex,
  setPlanName,
  setUnsavedChanges,
  setShowCourseBag,
  setRoadmapLoading,
} = roadmapSlice.actions;

// Other code such as selectors can use the imported `RootState` type
export const selectYearPlans = (state: RootState) =>
  state.roadmap.plans[state.roadmap.currentPlanIndex].content.yearPlans;

export const selectAllPlans = (state: RootState) => state.roadmap.plans;

export default roadmapSlice.reducer;
