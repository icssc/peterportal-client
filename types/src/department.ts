import { paths } from './generated/anteater-api-types';

export type ProfessorAAPIResponse =
  paths['/v2/rest/departments']['get']['responses'][200]['content']['application/json']['data'];

export type ProfessorBatchAAPIResponse = Record<string, ProfessorAAPIResponse>;
