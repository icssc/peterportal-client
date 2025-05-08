import { ProfessorGQLData } from '../types/types';

export function getProfessorTerms(professorGQLData: ProfessorGQLData) {
  return Object.values(professorGQLData.courses).flatMap((c) => c.terms);
}
