import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { MajorProgram, MajorSpecialization, ProgramRequirement, MinorProgram } from '@peterportal/types';

export type RequirementsTabName = 'Major' | 'Minor' | 'GE' | 'Search';

const courseRequirementsSlice = createSlice({
  name: 'courseRequirements',
  initialState: {
    selectedTab: 'Major' as RequirementsTabName,
    majorList: [] as MajorProgram[],
    specializationList: [] as MajorSpecialization[],
    major: null as MajorProgram | null,
    specialization: null as MajorSpecialization | null,
    currentRequirements: [] as ProgramRequirement[],
    minor: null as MinorProgram | null,
    minorList: [] as MinorProgram[],
    geRequirements: [] as ProgramRequirement[],
  },
  reducers: {
    setSelectedTab: (state, action: PayloadAction<RequirementsTabName>) => {
      state.selectedTab = action.payload;
    },
    setMajorList: (state, action: PayloadAction<MajorProgram[]>) => {
      state.majorList = action.payload;
    },
    setSpecializationList: (state, action: PayloadAction<MajorSpecialization[]>) => {
      state.specializationList = action.payload;
    },
    setMajor: (state, action: PayloadAction<MajorProgram | null>) => {
      state.major = action.payload;
    },
    setSpecialization: (state, action: PayloadAction<MajorSpecialization | null>) => {
      state.specialization = action.payload;
    },
    setRequirements: (state, action: PayloadAction<ProgramRequirement[]>) => {
      state.currentRequirements = action.payload;
    },
    setMinorList: (state, action: PayloadAction<MinorProgram[]>) => {
      state.minorList = action.payload;
    },
    setMinor: (state, action: PayloadAction<MinorProgram | null>) => {
      state.minor = action.payload;
    },
    setGERequirements: (state, action: PayloadAction<ProgramRequirement[]>) => {
      state.geRequirements = action.payload;
    },
  },
});

export const {
  setSelectedTab,
  setMajorList,
  setSpecializationList,
  setMajor,
  setSpecialization,
  setRequirements,
  setMinorList,
  setMinor,
  setGERequirements,
} = courseRequirementsSlice.actions;

export default courseRequirementsSlice.reducer;
