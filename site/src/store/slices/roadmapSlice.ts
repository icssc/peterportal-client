import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import type { RootState } from '../store';
import {
  PlannerData,
  PlannerYearData,
  CourseGQLData,
  YearIdentifier,
  QuarterIdentifier,
  CourseIdentifier,
  InvalidCourseData,
  TransferData,
  PlannerQuarterData,
} from '../../types/types';
import { defaultYear } from '../../helpers/planner';

// Define a type for the slice state
interface RoadmapState {
  // Store planner data
  yearPlans: PlannerData;
  // Store the course data of the active dragging item
  activeCourse: CourseGQLData;
  // Store the location of invalid courses (do not meet prerequisites)
  invalidCourses: InvalidCourseData[];
  // Whether or not to show the transfer modal
  showTransfer: boolean;
  // Store transfer course data
  transfers: TransferData[];
  // Whether or not to show the search bar on mobile
  showSearch: boolean;
  // Whether or not to show the add course modal on mobile
  showAddCourse: boolean;
  // Whether or not to alert the user of unsaved changes before leaving
}

// Define the initial state using that type
export const initialState: RoadmapState = {
  yearPlans: [defaultYear() as PlannerYearData],
  activeCourse: null!,
  invalidCourses: [],
  showTransfer: false,
  transfers: [],
  showSearch: false,
  showAddCourse: false,
};

/** added for multiple planner */
// create roadmap plan object
interface RoadmapPlan {
  name: string;
  content: RoadmapState;
}

interface RoadmapPlanIdentifier {
  planIndex: number;
}

interface SetPlanNamePayload {
  index: number;
  name: string;
}

// default plan to display for uesr
const defaultPlan: RoadmapPlan = {
  name: 'Schedule 1',
  content: initialState,
};

// have an array of RoadmapPlan; use index to access them later
interface RoadmapPlans {
  plans: RoadmapPlan[];
  currentPlanIndex: number;
  unsavedChanges: boolean;
}

// define initial empty plans
const initialPlans: RoadmapPlans = {
  plans: [defaultPlan],
  currentPlanIndex: 0,
  unsavedChanges: false,
};
/** added for multiple planner */

// Payload to pass in to move a course
interface MoveCoursePayload {
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

// Payload to set the trasnfer data at a specific index
interface SetTransferPayload {
  index: number;
  transfer: TransferData;
}

// onbeforeunload event listener
const alertUnsaved = (event: BeforeUnloadEvent) => event.preventDefault();

export const roadmapSlice = createSlice({
  name: 'roadmap',
  // `createSlice` will infer the state type from the `initialState` argument
  initialState: initialPlans,
  reducers: {
    // Use the PayloadAction type to declare the contents of `action.payload`
    moveCourse: (state, action: PayloadAction<MoveCoursePayload>) => {
      const toYear = action.payload.to.yearIndex;
      const toQuarter = action.payload.to.quarterIndex;
      const toCourse = action.payload.to.courseIndex;
      const fromYear = action.payload.from.yearIndex;
      const fromQuarter = action.payload.from.quarterIndex;
      const fromCourse = action.payload.from.courseIndex;

      let removed: CourseGQLData = null!;
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
        removed = state.plans[state.currentPlanIndex].content.activeCourse;
      }

      // add course to list
      const courseList = state.plans[state.currentPlanIndex].content.yearPlans[toYear].quarters[toQuarter].courses;
      courseList.splice(toCourse, 0, removed!);
    },
    deleteCourse: (state, action: PayloadAction<CourseIdentifier>) => {
      state.plans[state.currentPlanIndex].content.yearPlans[action.payload.yearIndex].quarters[
        action.payload.quarterIndex
      ].courses.splice(action.payload.courseIndex, 1);
    },
    addQuarter: (state, action: PayloadAction<AddQuarterPayload>) => {
      const startYear = action.payload.startYear;
      const currentYears = state.plans[state.currentPlanIndex].content.yearPlans.map((e) => e.startYear);
      const newQuarter = action.payload.quarterData;

      // if year doesn't exist
      if (!currentYears.includes(startYear)) {
        alert(`${startYear}-${startYear + 1} has not yet been added!`);
        return;
      }

      const yearIndex: number = currentYears.indexOf(startYear);
      const currentQuarters = state.plans[state.currentPlanIndex].content.yearPlans[yearIndex].quarters.map(
        (e) => e.name,
      );

      // if duplicate quarter
      if (currentQuarters.includes(newQuarter.name)) {
        alert(
          `${
            newQuarter.name.charAt(0).toUpperCase() + newQuarter.name.slice(1)
          } has already been added to Year ${yearIndex}!`,
        );
        return;
      }

      // check if where to put newQuarter
      let index = currentQuarters.length;
      if (currentQuarters) {
        for (let i = 0; i < currentQuarters.length; i++) {
          if (
            currentQuarters[i].length > newQuarter.name.length ||
            (currentQuarters[i].length == newQuarter.name.length && newQuarter.name === 'winter')
          ) {
            // only scenario where name length can't distinguish ordering is spring vs winter
            index = i;
            break;
          }
        }
      }

      state.plans[state.currentPlanIndex].content.yearPlans[yearIndex].quarters.splice(index, 0, newQuarter);
    },
    deleteQuarter: (state, action: PayloadAction<QuarterIdentifier>) => {
      state.plans[state.currentPlanIndex].content.yearPlans[action.payload.yearIndex].quarters.splice(
        action.payload.quarterIndex,
        1,
      );
    },
    clearQuarter: (state, action: PayloadAction<QuarterIdentifier>) => {
      state.plans[state.currentPlanIndex].content.yearPlans[action.payload.yearIndex].quarters[
        action.payload.quarterIndex
      ].courses = [];
    },
    addYear: (state, action: PayloadAction<AddYearPayload>) => {
      const currentYears = state.plans[state.currentPlanIndex].content.yearPlans.map((e) => e.startYear);
      const newYear = action.payload.yearData.startYear;
      const currentNames = state.plans[state.currentPlanIndex].content.yearPlans.map((e) => e.name);
      const newName = action.payload.yearData.name;

      // if duplicate year
      if (currentYears.includes(newYear)) {
        alert(`${newYear}-${newYear + 1} has already been added as Year ${currentYears.indexOf(newYear) + 1}!`);
        return;
      }
      // if duplicate name
      if (currentNames.includes(newName)) {
        const year = state.plans[state.currentPlanIndex].content.yearPlans[currentNames.indexOf(newName)].startYear;
        alert(`${newName} already exists from ${year} - ${year + 1}!`);
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
        alert(`${newYear}-${newYear + 1} already exists as Year ${currentYears.indexOf(newYear) + 1}!`);
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
      for (
        let i = 0;
        i < state.plans[state.currentPlanIndex].content.yearPlans[action.payload.yearIndex].quarters.length;
        i++
      ) {
        state.plans[state.currentPlanIndex].content.yearPlans[action.payload.yearIndex].quarters[i].courses = [];
      }
    },
    editName: (state, action: PayloadAction<EditNamePayload>) => {
      const currentNames = state.plans[state.currentPlanIndex].content.yearPlans.map((e) => e.name);
      const newName = action.payload.name;
      const yearIndex = action.payload.index;

      // if duplicate name
      if (currentNames.includes(newName)) {
        const year = state.plans[state.currentPlanIndex].content.yearPlans[yearIndex].startYear;
        alert(`${newName} already exists from ${year} - ${year + 1}!`);
        return;
      }

      // edit name
      state.plans[state.currentPlanIndex].content.yearPlans[yearIndex].name = newName;
    },
    clearPlanner: (state) => {
      if (window.confirm('Are you sure you want to clear your Roadmap?')) {
        state.plans[state.currentPlanIndex].content.yearPlans = [];
      }
    },
    setActiveCourse: (state, action: PayloadAction<CourseGQLData>) => {
      state.plans[state.currentPlanIndex].content.activeCourse = action.payload;
    },
    setYearPlans: (state, action: PayloadAction<PlannerData>) => {
      state.plans[state.currentPlanIndex].content.yearPlans = action.payload;
    },
    setInvalidCourses: (state, action: PayloadAction<InvalidCourseData[]>) => {
      state.plans[state.currentPlanIndex].content.invalidCourses = action.payload;
    },
    setShowTransfer: (state, action: PayloadAction<boolean>) => {
      state.plans[state.currentPlanIndex].content.showTransfer = action.payload;
    },
    addTransfer: (state, action: PayloadAction<TransferData>) => {
      state.plans[state.currentPlanIndex].content.transfers.push(action.payload);
    },
    setTransfer: (state, action: PayloadAction<SetTransferPayload>) => {
      state.plans[state.currentPlanIndex].content.transfers[action.payload.index] = action.payload.transfer;
    },
    setTransfers: (state, action: PayloadAction<TransferData[]>) => {
      state.plans[state.currentPlanIndex].content.transfers = action.payload;
    },
    deleteTransfer: (state, action: PayloadAction<number>) => {
      state.plans[state.currentPlanIndex].content.transfers.splice(action.payload, 1);
    },
    setShowSearch: (state, action: PayloadAction<boolean>) => {
      state.plans[state.currentPlanIndex].content.showSearch = action.payload;
    },
    setShowAddCourse: (state, action: PayloadAction<boolean>) => {
      state.plans[state.currentPlanIndex].content.showAddCourse = action.payload;
    },
    /** added for multiple plans */
    setRoadmapPlan: (state, action: PayloadAction<RoadmapPlans>) => {
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
    /** added for multiple plans */
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
  clearPlanner,
  setActiveCourse,
  setYearPlans,
  setInvalidCourses,
  setShowTransfer,
  addTransfer,
  setTransfer,
  setTransfers,
  deleteTransfer,
  setShowSearch,
  setShowAddCourse,
  setRoadmapPlan,
  addRoadmapPlan,
  deleteRoadmapPlan,
  setPlanIndex,
  setPlanName,
  setUnsavedChanges,
} = roadmapSlice.actions;

// export const { setRoadmapPlan, addRoadmapPlan, deleteRoadmapPlan } = roadmapMultiplePlanSlice.actions;

// Other code such as selectors can use the imported `RootState` type
export const selectYearPlans = (state: RootState) =>
  state.roadmap.plans[state.roadmap.currentPlanIndex].content.yearPlans;

export default roadmapSlice.reducer;
