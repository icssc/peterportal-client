import { components, operations } from './generated/anteater-api-types';

export type MajorProgram = operations['getMajors']['responses']['200']['content']['application/json']['data'][0];
export type MinorProgram = operations['getMinors']['responses']['200']['content']['application/json']['data'][0];
export type MajorSpecialization =
  operations['getSpecializations']['responses']['200']['content']['application/json']['data'][0];

type RequirementSchema = components['schemas']['programRequirement'];
type ReqType = RequirementSchema['requirementType'];
export type ProgramRequirement<T extends ReqType = ReqType> = RequirementSchema & { requirementType: T };

export interface MajorSpecializationPair {
  majorId: string;
  specializationId?: string;
}
