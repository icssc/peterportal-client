import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { defaultYear } from '../../helpers/planner';
import {
  CourseGQLData,
  CourseIdentifier,
  InvalidCourseData,
  PlannerQuarterData,
  PlannerYearData,
  RoadmapPlan,
  RoadmapPlanState,
  RoadmapRevision,
} from '../../types/types';
import type { RootState } from '../store';
import { restoreRevision } from '../../helpers/roadmap';
import { LOADING_COURSE_PLACEHOLDER } from '../../helpers/courseRequirements';

// Define the initial state using that type
export const initialPlanState: RoadmapPlanState = {
  yearPlans: [defaultYear() as PlannerYearData],
  invalidCourses: [],
};

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

const baseRevision: RoadmapRevision = {
  timestamp: Date.now(),
  edits: [],
};

interface SetActiveCoursePayload {
  course: CourseGQLData;
  startYear?: number;
  quarter?: PlannerQuarterData;
  courseIndex?: number;
}

// onbeforeunload event listener
const alertUnsaved = (event: BeforeUnloadEvent) => event.preventDefault();

export const roadmapSlice = createSlice({
  name: 'roadmap',
  initialState: {
    plans: [defaultPlan],
    revisions: [baseRevision],
    currentRevisionIndex: 0,
    /** The index of the revision where the user last saved the roadmap */
    savedRevisionIndex: 0,
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
    activeCourse: null as CourseGQLData | null,
    /** true if we start dragging a course whose info hasn't fully loaded yet, i.e. from Degree Requirements */
    activeCourseLoading: false,
    /** Store missing prerequisites for courses when adding on mobile */
    activeMissingPrerequisites: undefined as string[] | undefined,
    /** Where the active course is being dragged from */
    activeCourseDragSource: null as Omit<SetActiveCoursePayload, 'course'> | null,
    /** Whether the roadmap is loading */
    roadmapLoading: false,
  },
  reducers: {
    // Roadmap Window State

    setInitialPlannerData: (state, action: PayloadAction<RoadmapPlan[]>) => {
      state.plans = action.payload;
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

    // Modifying the Roadmap

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
    setSavedRevisionIndex: (state, action: PayloadAction<number>) => {
      state.savedRevisionIndex = action.payload;
    },

    // Intermediate States When Adding Courses

    setActiveCourse: (state, action: PayloadAction<SetActiveCoursePayload | null>) => {
      if (!action.payload) {
        state.activeCourse = state.activeCourseDragSource = null;
        return;
      }
      const { course, ...dragSource } = action.payload;
      state.activeCourse = course;
      state.activeCourseDragSource = dragSource.quarter ? dragSource : null;
    },
    setActiveCourseLoading: (state, action: PayloadAction<boolean>) => {
      state.activeCourseLoading = action.payload;
    },
    setActiveMissingPrerequisites: (state, action: PayloadAction<string[] | undefined>) => {
      state.activeMissingPrerequisites = action.payload;
    },
    /**
     * Creates a loading placeholder in the specified position. This placeholder automatically gets
     * removed when creating a revision to add a course to the user's roadmap.
     */
    createQuarterCourseLoadingPlaceholder: (state, action: PayloadAction<CourseIdentifier>) => {
      const target = action.payload;
      const yearPlans = state.plans[state.currentPlanIndex].content.yearPlans;
      const quarter = yearPlans[target.yearIndex].quarters[target.quarterIndex];
      quarter.courses.splice(target.courseIndex, 0, LOADING_COURSE_PLACEHOLDER);
    },
    setInvalidCourses: (state, action: PayloadAction<InvalidCourseData[]>) => {
      state.plans[state.currentPlanIndex].content.invalidCourses = action.payload;
    },

    // Controlling Visibility of UI Elements

    setShowSearch: (state, action: PayloadAction<{ show: boolean; year?: number; quarter?: number }>) => {
      state.showSearch = action.payload.show;
      if (action.payload.year !== undefined && action.payload.quarter !== undefined) {
        state.currentYearAndQuarter = { year: action.payload.year, quarter: action.payload.quarter };
      }
    },
    setShowAddCourse: (state, action: PayloadAction<boolean>) => {
      state.showAddCourse = action.payload;
    },
    setPlanIndex: (state, action: PayloadAction<number>) => {
      state.currentPlanIndex = action.payload;
    },
    setShowSavedCourses: (state, action: PayloadAction<boolean>) => {
      state.showSavedCourses = action.payload;
    },
  },
});

export const {
  createQuarterCourseLoadingPlaceholder,
  setActiveCourse,
  setActiveCourseLoading,
  setActiveMissingPrerequisites,
  setInitialPlannerData,
  setInvalidCourses,
  setShowSearch,
  setShowAddCourse,
  setPlanIndex,
  setUnsavedChanges,
  setShowSavedCourses,
  setRoadmapLoading,
  reviseRoadmap,
  undoRoadmapRevision,
  redoRoadmapRevision,
  setSavedRevisionIndex,
} = roadmapSlice.actions;

// Other code such as selectors can use the imported `RootState` type
export const selectYearPlans = (state: RootState) =>
  state.roadmap.plans[state.roadmap.currentPlanIndex].content.yearPlans;

export const selectAllPlans = (state: RootState) => state.roadmap.plans;

export const selectCurrentPlan = (state: RootState) => state.roadmap.plans[state.roadmap.currentPlanIndex];

export const getNextPlannerTempId = (state: RootState) => {
  return Math.min(0, ...state.roadmap.plans.map((p) => p.id)) - 1;
};

export default roadmapSlice.reducer;
