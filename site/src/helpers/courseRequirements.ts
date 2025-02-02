import {
  MajorProgram,
  MajorSpecialization,
  MinorProgram,
  ProgramRequirement,
  TypedProgramRequirement,
} from '@peterportal/types';

export const COMPLETE_ALL_TEXT = 'Complete all of the following';

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

export type CompletedCourseSet = Set<string>;
export interface CompletionStatus {
  required: number;
  completed: number;
  done: boolean;
}

function checkCourseListCompletion(
  completed: CompletedCourseSet,
  requirement: TypedProgramRequirement<'Course'>,
): CompletionStatus {
  const completedCount = requirement.courses.filter((c) => completed.has(c)).length;
  const required = requirement.courseCount;
  return { required, completed: completedCount, done: completedCount === required };
}
function checkGroupCompletion(
  completed: CompletedCourseSet,
  requirement: TypedProgramRequirement<'Group'>,
): CompletionStatus {
  const checkIsDone = (req: ProgramRequirement) => checkCompletion(completed, req).done;
  const completedGroups = requirement.requirements.filter(checkIsDone).length;
  const required = requirement.requirementCount;
  return { required, completed: completedGroups, done: completedGroups === required };
}
function checkUnitCompletion(
  completed: CompletedCourseSet,
  requirement: TypedProgramRequirement<'Unit'>,
): CompletionStatus {
  const required = requirement.unitCount;
  return { required, completed: 0, done: false }; // TEMP
}

export function checkCompletion(completed: CompletedCourseSet, requirement: ProgramRequirement): CompletionStatus {
  switch (requirement.requirementType) {
    case 'Group':
      return checkGroupCompletion(completed, requirement);
    case 'Course':
      return checkCourseListCompletion(completed, requirement);
    case 'Unit':
      return checkUnitCompletion(completed, requirement);
  }
}
