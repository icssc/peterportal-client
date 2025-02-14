import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { MajorProgram, MajorSpecialization, ProgramRequirement, MinorProgram } from '@peterportal/types';

export type RequirementsTabName = 'Major' | 'Minor' | 'GE' | 'All Courses';

export interface MajorWithSpecialization {
  major: MajorProgram;
  specialization: MajorSpecialization | null;
  requirements: ProgramRequirement[];
}

const courseRequirementsSlice = createSlice({
  name: 'courseRequirements',
  initialState: {
    selectedTab: 'Major' as RequirementsTabName,
    majorList: [] as MajorProgram[],
    selectedMajors: [] as MajorWithSpecialization[],
    specialization: null as MajorSpecialization | null,
    minor: null as MinorProgram | null,
    minorList: [] as MinorProgram[],
  },
  reducers: {
    setSelectedTab: (state, action: PayloadAction<RequirementsTabName>) => {
      state.selectedTab = action.payload;
    },
    setMajorList: (state, action: PayloadAction<MajorProgram[]>) => {
      state.majorList = action.payload;
    },
    addMajor: (state, action: PayloadAction<MajorProgram>) => {
      if (!state.selectedMajors.find((m) => m.major.id === action.payload.id)) {
        state.selectedMajors.push({
          major: action.payload,
          specialization: null,
          requirements: [],
        });
      }
    },
    removeMajor: (state, action: PayloadAction<string>) => {
      state.selectedMajors = state.selectedMajors.filter((m) => m.major.id !== action.payload);
    },
    setSpecialization: (
      state,
      action: PayloadAction<{ majorId: string; specialization: MajorSpecialization | null }>,
    ) => {
      const major = state.selectedMajors.find((m) => m.major.id === action.payload.majorId);
      if (major) {
        major.specialization = action.payload.specialization;
      }
    },
    setRequirements: (state, action: PayloadAction<{ majorId: string; requirements: ProgramRequirement[] }>) => {
      const major = state.selectedMajors.find((m) => m.major.id === action.payload.majorId);
      if (major) {
        major.requirements = action.payload.requirements;
      }
    },
    setMinor: (state, action: PayloadAction<MinorProgram | null>) => {
      state.minor = action.payload;
    },
    setMinorList: (state, action: PayloadAction<MinorProgram[]>) => {
      state.minorList = action.payload;
    },
  },
});

export const {
  setSelectedTab,
  setMajorList,
  addMajor,
  removeMajor,
  setSpecialization,
  setRequirements,
  setMinor,
  setMinorList,
} = courseRequirementsSlice.actions;

export default courseRequirementsSlice.reducer;
