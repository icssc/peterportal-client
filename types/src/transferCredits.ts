import { operations } from './generated/anteater-api-types';

export type APExam = operations['apExams']['responses'][200]['content']['application/json']['data'][number];

export type GEName =
  operations['apExams']['responses'][200]['content']['application/json']['data'][number]['rewards'][number]['geCategories'][number];
export type GETitle =
  operations['courseById']['responses'][200]['content']['application/json']['data']['geList'][number];

export interface TransferredGE {
  geName: GEName;
  numberOfCourses: number;
  units: number;
}

export interface UserAPExam {
  examName: string;
  score: number;
  units: number;
}

export interface TransferredCourse {
  courseName: string;
  units: number;
}

export interface TransferredUncategorized {
  name: string | null;
  units: number | null;
}
