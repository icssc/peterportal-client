import { createSlice, PayloadAction } from '@reduxjs/toolkit'
import type { RootState } from '../store'
import { PlannerData, PlannerYearData, CourseData } from '../../types/types';

// Define a type for the slice state
interface RoadmapState {
    // Store planner data
    yearPlans: PlannerData
    // Store the course data of the active dragging item
    activeCourse: CourseData
}

// Define the initial state using that type
const initialState: RoadmapState = {
    yearPlans: [],
    activeCourse: null!
}

// Payload to pass in to move a course
interface MoveCoursePayload {
    from: CourseIdentifier;
    to: CourseIdentifier;
}

// Paylaod to pass in to add a year
interface AddYearPayload {
    yearData: PlannerYearData;
}

// Specify the location of a year
interface YearIdentifier {
    yearIndex: number;
}

// Specify the location of a quarter
interface QuarterIdentifier extends YearIdentifier {
    quarterIndex: number;
}

// Specify the location of a course
interface CourseIdentifier extends QuarterIdentifier {
    courseIndex: number;
}

export const roadmapSlice = createSlice({
    name: 'roadmap',
    // `createSlice` will infer the state type from the `initialState` argument
    initialState,
    reducers: {
        // Use the PayloadAction type to declare the contents of `action.payload`
        moveCourse: (state, action: PayloadAction<MoveCoursePayload>) => {
            let toYear = action.payload.to.yearIndex;
            let toQuarter = action.payload.to.quarterIndex;
            let toCourse = action.payload.from.courseIndex;
            let fromYear = action.payload.from.yearIndex;
            let fromQuarter = action.payload.from.quarterIndex;
            let fromCourse = action.payload.from.courseIndex;

            let removed: CourseData = null!;
            // not from the searchbar
            if (fromYear != -1) {
                // remove course from list
                let courseList = state.yearPlans[fromYear].quarters[fromQuarter].courses;
                [removed] = courseList.splice(fromCourse, 1);
            }
            // from the searchbar
            else {
                // active course has the current dragging course
                removed = state.activeCourse;
            }

            // add course to list
            let courseList = state.yearPlans[toYear].quarters[toQuarter].courses;
            courseList.splice(toCourse, 0, removed!);
        },
        addYear: (state, action: PayloadAction<AddYearPayload>) => {
            state.yearPlans.splice(state.yearPlans.length, 0, action.payload.yearData);
        },
        deleteYear: (state, action: PayloadAction<YearIdentifier>) => {
            delete state.yearPlans[action.payload.yearIndex];
        },
        setActiveCourse: (state, action: PayloadAction<CourseData>) => {
            state.activeCourse = action.payload;
        },
        setYearPlans: (state, action: PayloadAction<PlannerData>) => {
            state.yearPlans = action.payload;
        },
    },
})

export const { moveCourse, addYear, deleteYear, setActiveCourse, setYearPlans } = roadmapSlice.actions

// Other code such as selectors can use the imported `RootState` type
export const selectYearPlans = (state: RootState) => state.roadmap.yearPlans;

export default roadmapSlice.reducer