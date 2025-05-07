import { MajorProgram, MajorSpecialization, MinorProgram, ProgramRequirement } from '@peterportal/types';
import { CourseGQLData } from '../types/types';
import { Theme } from 'react-select';
import { useAppSelector } from '../store/hooks';

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
  const themeCopy = { ...theme, colors: { ...theme.colors } };

  const getCssVariable = (variableName: string) => {
    const bodyStyles = getComputedStyle(document.body);
    return bodyStyles.getPropertyValue(variableName).trim();
  };

  themeCopy.colors.primary = getCssVariable('--blue-primary'); // box border
  themeCopy.colors.primary50 = getCssVariable('--blue-secondary'); // active
  themeCopy.colors.primary25 = getCssVariable('--blue-tertiary'); // hover

  if (darkMode) {
    const neutralIncrements = [0, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90];
    Object.entries(theme.colors).forEach(([key]) => {
      if (key.startsWith('neutral')) {
        const index = neutralIncrements.indexOf(parseInt(key.replace('neutral', '')));
        const opposite = ('neutral' + neutralIncrements.at(-1 - index)) as keyof Theme['colors'];
        themeCopy.colors[key as keyof Theme['colors']] = theme.colors[opposite];
      }
    });
  }

  return themeCopy;
};

/**
 * Groups consectutive single-course requirements into one group requirement where all courses must be completed
 * @param requirements The raw course requirements, as returned from the API
 */
export function collapseSingletonRequirements(requirements: ProgramRequirement[]) {
  let builtGroup: ProgramRequirement<'Group'> | null = null;

  const computedRequirements: ProgramRequirement[] = [];

  const addBuiltGroup = () => {
    if (builtGroup?.requirements?.length === 1) {
      computedRequirements.push(builtGroup.requirements[0]);
    } else if (builtGroup) {
      const courseReqs: ProgramRequirement<'Course'> = {
        requirementType: 'Course',
        label: builtGroup.label,
        courseCount: builtGroup.requirementCount,
        courses: (builtGroup.requirements as ProgramRequirement<'Course'>[]).map((c) => c.courses[0]),
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

export function flattenSingletonGroups(requirements: ProgramRequirement[]): ProgramRequirement[] {
  const res = requirements.flatMap((r) => {
    if (r.requirementType !== 'Group') return r;
    if (r.requirementCount !== r.requirements.length) return r;
    return flattenSingletonGroups(r.requirements);
  });
  return res;
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
  requirement: ProgramRequirement<'Course'>,
): CompletionStatus {
  const completedCount = requirement.courses.filter((c) => c in completed).length;
  const required = requirement.courseCount;
  return { required, completed: completedCount, done: completedCount >= required };
}

function checkGroupCompletion(
  completed: CompletedCourseSet,
  requirement: ProgramRequirement<'Group'>,
): CompletionStatus {
  const checkIsDone = (req: ProgramRequirement) => checkCompletion(completed, req).done;
  const completedGroups = requirement.requirements.filter(checkIsDone).length;
  const required = requirement.requirementCount;
  return { required, completed: completedGroups, done: completedGroups >= required };
}

function useGroupCompletionCheck(completed: CompletedCourseSet, requirement: ProgramRequirement): CompletionStatus {
  const useIsDone = (req: ProgramRequirement) => useCompletionCheck(completed, req).done;
  if (requirement.requirementType !== 'Group') return { required: 0, completed: 0, done: false };

  const completedGroups = requirement.requirements.filter(useIsDone).length;
  const required = requirement.requirementCount;
  return { required, completed: completedGroups, done: completedGroups >= required };
}

function checkUnitCompletion(completed: CompletedCourseSet, requirement: ProgramRequirement<'Unit'>): CompletionStatus {
  const required = requirement.unitCount;
  const completedCourses = requirement.courses.filter((c) => c in completed);
  const completedUnits = completedCourses.map((c) => completed[c]).reduce((a, b) => a + b, 0);
  return { required, completed: completedUnits, done: completedUnits >= required };
}

export function coerceEmptyRequirement(requirement: ProgramRequirement): ProgramRequirement {
  if (requirement.requirementType === 'Course' && !requirement.courses.length) {
    // Some course requirements don't provide a list of courses from the API. For these cases,
    // treat them as a Marker so the user can manually mark as complete.
    return { requirementType: 'Marker', label: requirement.label };
  } else {
    return requirement;
  }
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

export function useCompletionCheck(completed: CompletedCourseSet, requirement: ProgramRequirement): CompletionStatus {
  const completedMarkers = useAppSelector((state) => state.courseRequirements.completedMarkers);
  const parsedRequirement = coerceEmptyRequirement(requirement);
  const groupComplete = useGroupCompletionCheck(completed, parsedRequirement);

  switch (parsedRequirement.requirementType) {
    case 'Group':
      return groupComplete;
    case 'Course':
      return checkCourseListCompletion(completed, parsedRequirement);
    case 'Unit':
      return checkUnitCompletion(completed, parsedRequirement);
    case 'Marker':
      return { completed: 0, done: completedMarkers[parsedRequirement.label], required: 0 };
  }
}
