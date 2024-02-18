import { PlannerYearData, SavedPlannerYearData } from '../types/types';

export function defaultYear() {
  return {
    startYear: new Date().getFullYear(),
    name: 'Year 1',
    quarters: ['fall', 'winter', 'spring'].map((quarter) => {
      return { name: quarter, courses: [] };
    }),
  } as PlannerYearData | SavedPlannerYearData;
}
