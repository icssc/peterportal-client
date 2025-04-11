import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { MajorProgram, MajorSpecialization, ProgramRequirement, MinorProgram } from '@peterportal/types';

export type RequirementsTabName = 'Major' | 'Minor' | 'GE' | 'Search';

export interface MajorWithSpecialization {
  major: MajorProgram;
  specialization: MajorSpecialization | null;
  requirements: ProgramRequirement[];
}

type ExpandedGroupsList = { [key: string]: boolean | undefined };

const courseRequirementsSlice = createSlice({
  name: 'courseRequirements',
  initialState: {
    selectedTab: 'Major' as RequirementsTabName,
    majorList: [] as MajorProgram[],
    selectedMajors: [] as MajorWithSpecialization[],
    specialization: null as MajorSpecialization | null,
    minor: null as MinorProgram | null,
    minorList: [] as MinorProgram[],
    minorRequirements: [] as ProgramRequirement[],
    geRequirements: [] as ProgramRequirement[],
    expandedGroups: {} as ExpandedGroupsList,
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
    setMinorRequirements: (state, action: PayloadAction<ProgramRequirement[]>) => {
      state.minorRequirements = action.payload;
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
    setGroupExpanded: (state, action: PayloadAction<{ storeKey: string; expanded: boolean }>) => {
      if (action.payload.expanded) {
        state.expandedGroups[action.payload.storeKey] = true;
      } else {
        delete state.expandedGroups[action.payload.storeKey];
      }
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
  setMinorList,
  setMinor,
  setMinorRequirements,
  setGERequirements,
  setGroupExpanded,
} = courseRequirementsSlice.actions;

export default courseRequirementsSlice.reducer;
