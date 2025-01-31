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
