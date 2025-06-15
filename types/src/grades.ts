import { paths } from './generated/anteater-api-types';

export type GradesRaw = paths['/v2/rest/grades/raw']['get']['responses'][200]['content']['application/json']['data'];

export type LetterGrade = 'A' | 'B' | 'C' | 'D' | 'F' | 'P' | 'NP';

export const letterGrades: LetterGrade[] = ['A', 'B', 'C', 'D', 'F', 'P', 'NP'] as const;
export const pnpGrades: LetterGrade[] = ['P', 'NP'] as const;
