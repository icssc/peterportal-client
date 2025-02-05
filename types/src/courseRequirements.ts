import { components, operations } from './generated/anteater-api-types';

export type MajorProgram = operations['getMajors']['responses']['200']['content']['application/json']['data'][0];
export type MinorProgram = operations['getMinors']['responses']['200']['content']['application/json']['data'][0];
export type MajorSpecialization =
  operations['getSpecializations']['responses']['200']['content']['application/json']['data'][0];

export type ProgramRequirement = components['schemas']['programRequirement'];
export type TypedProgramRequirement<T extends string> = ProgramRequirement & { requirementType: T };

export interface MajorSpecializationPair {
  majorId: string;
  specializationId?: string;
}
