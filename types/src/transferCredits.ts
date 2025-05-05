import { operations } from './generated/anteater-api-types';
export type APExams = operations['apExams']['responses'][200]['content']['application/json']['data'];
export type APExam = APExams[number];
