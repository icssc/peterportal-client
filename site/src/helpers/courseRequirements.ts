import { components } from '@peterportal/types/src/generated/anteater-api-types';

export type ProgramRequirement = components['schemas']['programRequirement'];
export type TypedProgramRequirement<T extends string> = ProgramRequirement & { requirementType: T };

/**
 * Groups consectutive single-course requirements into one group requirement where all courses must be completed
 * @param requirements The raw course requirements, as returned from the API
 */
export function collapseSingletonRequirements(requirements: ProgramRequirement[]) {
  let builtGroup: TypedProgramRequirement<'Group'> | null = null;

  const computedRequirements: ProgramRequirement[] = [];

  for (const r of requirements) {
    if (r.requirementType !== 'Course' || r.courses.length !== 1) {
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
      builtGroup = null;

      computedRequirements.push(r);
      continue;
    }

    builtGroup ??= {
      requirementType: 'Group',
      requirementCount: 0,
      label: 'Complete all of the following',
      requirements: [],
    };
    builtGroup.requirements.push(r);
    builtGroup.requirementCount++;
  }

  return computedRequirements;
}
