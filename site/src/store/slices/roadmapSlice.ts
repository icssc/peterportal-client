import { createSlice, PayloadAction } from '@reduxjs/toolkit'
import type { RootState } from '../store'
import { PlannerData, PlannerYearData, CourseData, YearIdentifier, QuarterIdentifier, CourseIdentifier, InvalidCourseData, TransferData } from '../../types/types';

// Define a type for the slice state
interface RoadmapState {
    // Store planner data
    yearPlans: PlannerData;
    // Store the course data of the active dragging item
    activeCourse: CourseData;
    // Store the location of invalid courses (do not meet prerequisites)
    invalidCourses: InvalidCourseData[];
    // Whether or not to show the transfer modal
    showTransfer: boolean;
    // Store transfer course data
    transfers: TransferData[];
}

// Define the initial state using that type
const initialState: RoadmapState = {
    yearPlans: [],
    activeCourse: null!,
    invalidCourses: [],
    showTransfer: false,
    transfers: []
}

// Payload to pass in to move a course
interface MoveCoursePayload {
    from: CourseIdentifier;
    to: CourseIdentifier;
}

// Payload to pass in to add a year
interface AddYearPayload {
    yearData: PlannerYearData;
}

// Payload to set the trasnfer data at a specific index
interface SetTransferPayload {
    index: number;
    transfer: TransferData;
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
        deleteCourse: (state, action: PayloadAction<CourseIdentifier>) => {
            state.yearPlans[action.payload.yearIndex].quarters[action.payload.quarterIndex].courses.splice(action.payload.courseIndex, 1);
        },
        addYear: (state, action: PayloadAction<AddYearPayload>) => {
            let currentYears = state.yearPlans.map(e => e.startYear);
            let newYear = action.payload.yearData.startYear;

            // if duplicate year
            if (currentYears.includes(newYear)) {
                alert(`${newYear}-${newYear+1} has already been added as Year ${currentYears.indexOf(newYear)+1}!`);
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

            state.yearPlans.splice(index, 0, action.payload.yearData);
        },
        deleteYear: (state, action: PayloadAction<YearIdentifier>) => {
            state.yearPlans.splice(action.payload.yearIndex, 1);
        },
        setActiveCourse: (state, action: PayloadAction<CourseData>) => {
            state.activeCourse = action.payload;
        },
        setYearPlans: (state, action: PayloadAction<PlannerData>) => {
            state.yearPlans = action.payload;
        },
        setInvalidCourses: (state, action: PayloadAction<InvalidCourseData[]>) => {
            state.invalidCourses = action.payload;
        },
        setShowTransfer: (state, action: PayloadAction<boolean>) => {
            state.showTransfer = action.payload;
        },
        addTransfer: (state, action: PayloadAction<TransferData>) => {
            state.transfers.push(action.payload);
        },
        setTransfer: (state, action: PayloadAction<SetTransferPayload>) => {
            state.transfers[action.payload.index] = action.payload.transfer;
        },
        setTransfers: (state, action: PayloadAction<TransferData[]>) => {
            state.transfers = action.payload;
        },
        deleteTransfer: (state, action: PayloadAction<number>) => {
            state.transfers.splice(action.payload, 1);
        },
    },
})

export const { moveCourse, deleteCourse, addYear, deleteYear, setActiveCourse, setYearPlans, setInvalidCourses, setShowTransfer, addTransfer, setTransfer, setTransfers, deleteTransfer } = roadmapSlice.actions

// Other code such as selectors can use the imported `RootState` type
export const selectYearPlans = (state: RootState) => state.roadmap.yearPlans;

export default roadmapSlice.reducer