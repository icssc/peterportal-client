import { z } from 'zod';
import { operations } from './generated/anteater-api-types';

export type APExam = operations['apExams']['responses'][200]['content']['application/json']['data'][number];

export type GEName =
  operations['apExams']['responses'][200]['content']['application/json']['data'][number]['rewards'][number]['geCategories'][number];
export type GETitle =
  operations['courseById']['responses'][200]['content']['application/json']['data']['geList'][number];

// These type names are prefixed with zod so as not to conflict with the db table names
// (e.g. `transferredCourse` from `db/schema`)
export const zodTransferredCourse = z.object({
  courseName: z.string(),
  units: z.number(),
});
export type TransferredCourse = z.infer<typeof zodTransferredCourse>;

export const zodTransferredAPExam = z.object({
  examName: z.string(),
  score: z.number(),
  units: z.number(),
});
export type TransferredAPExam = z.infer<typeof zodTransferredAPExam>;

export const zodTransferredGE = z.object({
  geName: z.string(),
  numberOfCourses: z.number(),
  units: z.number(),
});
export type TransferredGE = {
  geName: GEName; // Not inferred from zod in order to preserve the GE name constraint
  numberOfCourses: number;
  units: number;
};

export const zodTransferredUncategorized = z.object({
  name: z.string().nullable(),
  units: z.number().nullable(),
});
export type TransferredUncategorized = z.infer<typeof zodTransferredUncategorized>;
