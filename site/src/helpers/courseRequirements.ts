import {
  MajorProgram,
  MajorSpecialization,
  MinorProgram,
  ProgramRequirement,
  TypedProgramRequirement,
} from '@peterportal/types';
import { CourseGQLData } from '../types/types';
import { Theme } from 'react-select';

export const COMPLETE_ALL_TEXT = 'Complete all of the following';
export const LOADING_COURSE_PLACEHOLDER: CourseGQLData = {
  id: 'Loading...',
  department: 'Loading...',
  courseNumber: '',
  courseNumeric: 0,
  school: '',
  title: 'Loading...',
  courseLevel: 'Lower Division (1-99)',
  minUnits: 0,
  maxUnits: 0,
  description: '',
  departmentName: '',
  prerequisiteTree: {
    AND: undefined,
    OR: undefined,
    NOT: undefined,
  },
  prerequisiteText: '',
  repeatability: '',
  gradingOption: '',
  concurrent: '',
  sameAs: '',
  restriction: '',
  overlap: '',
  corequisites: '',
  geList: [],
  geText: '',
  terms: [],
  instructors: {},
  prerequisites: {},
  dependents: {},
};

export const comboboxTheme = (theme: Theme, darkMode: boolean) => {
  if (!darkMode) return theme;

  const themeCopy = { ...theme, colors: { ...theme.colors } };
  const neutralIncrements = [0, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90];
  Object.entries(theme.colors).forEach(([key]) => {
    if (key.startsWith('neutral')) {
      const index = neutralIncrements.indexOf(parseInt(key.replace('neutral', '')));
      const opposite = ('neutral' + neutralIncrements.at(-1 - index)) as keyof Theme['colors'];
      themeCopy.colors[key as keyof Theme['colors']] = theme.colors[opposite];
    }
  });

  themeCopy.colors.danger = theme.colors.dangerLight;
  themeCopy.colors.dangerLight = theme.colors.danger;

  themeCopy.colors.primary = theme.colors.primary75;
  themeCopy.colors.primary25 = '#343a40'; // same as bootstrap dark

  return themeCopy;
};
/**
 * Groups consectutive single-course requirements into one group requirement where all courses must be completed
 * @param requirements The raw course requirements, as returned from the API
 */
export function collapseSingletonRequirements(requirements: ProgramRequirement[]) {
  let builtGroup: TypedProgramRequirement<'Group'> | null = null;

  const computedRequirements: ProgramRequirement[] = [];

  const addBuiltGroup = () => {
    if (builtGroup?.requirements?.length === 1) {
      computedRequirements.push(builtGroup.requirements[0]);
    } else if (builtGroup) {
      const courseReqs: TypedProgramRequirement<'Course'> = {
        requirementType: 'Course',
        label: builtGroup.label,
        courseCount: builtGroup.requirementCount,
        courses: (builtGroup.requirements as TypedProgramRequirement<'Course'>[]).map((c) => c.courses[0]),
      };
      computedRequirements.push(courseReqs);
    }
  };

  for (const r of requirements) {
    if (r.requirementType !== 'Course' || r.courses.length !== 1) {
      addBuiltGroup();
      builtGroup = null;

      computedRequirements.push(r);
      continue;
    }

    builtGroup ??= {
      requirementType: 'Group',
      requirementCount: 0,
      label: COMPLETE_ALL_TEXT,
      requirements: [],
    };
    builtGroup.requirements.push(r);
    builtGroup.requirementCount++;
  }

  addBuiltGroup();
  return computedRequirements;
}

export function normalizeMajorName(program: MajorProgram | MinorProgram | MajorSpecialization) {
  return program.name.replace(/^(p[.\s]?h[.\s]?d[.\s]?|m[.\s]?a[.\s]?|major) in\s?/i, '');
}

export type CompletedCourseSet = {
  [k: string]: number; // course id => units
};

export interface CompletionStatus {
  required: number;
  completed: number;
  done: boolean;
}

function checkCourseListCompletion(
  completed: CompletedCourseSet,
  requirement: TypedProgramRequirement<'Course'>,
): CompletionStatus {
  const completedCount = requirement.courses.filter((c) => c in completed).length;
  const required = requirement.courseCount;
  return { required, completed: completedCount, done: completedCount >= required };
}

function checkGroupCompletion(
  completed: CompletedCourseSet,
  requirement: TypedProgramRequirement<'Group'>,
): CompletionStatus {
  const checkIsDone = (req: ProgramRequirement) => checkCompletion(completed, req).done;
  const completedGroups = requirement.requirements.filter(checkIsDone).length;
  const required = requirement.requirementCount;
  return { required, completed: completedGroups, done: completedGroups >= required };
}

function checkUnitCompletion(
  completed: CompletedCourseSet,
  requirement: TypedProgramRequirement<'Unit'>,
): CompletionStatus {
  const required = requirement.unitCount;
  const completedCourses = requirement.courses.filter((c) => c in completed);
  const completedUnits = completedCourses.map((c) => completed[c]).reduce((a, b) => a + b, 0);
  return { required, completed: completedUnits, done: completedUnits >= required };
}

export function checkCompletion(completed: CompletedCourseSet, requirement: ProgramRequirement): CompletionStatus {
  switch (requirement.requirementType) {
    case 'Group':
      return checkGroupCompletion(completed, requirement);
    case 'Course':
      return checkCourseListCompletion(completed, requirement);
    case 'Unit':
      return checkUnitCompletion(completed, requirement);
    case 'Marker':
      return { completed: 0, done: false, required: 0 };
  }
}
