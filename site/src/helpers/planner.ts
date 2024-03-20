import { PlannerYearData, QuarterName, SavedPlannerYearData } from '../types/types';

export function defaultYear() {
  const quarterNames: QuarterName[] = ['Fall', 'Winter', 'Spring'];
  return {
    startYear: new Date().getFullYear(),
    name: 'Year 1',
    quarters: quarterNames.map((quarter) => {
      return { name: quarter, courses: [] };
    }),
  } as PlannerYearData | SavedPlannerYearData;
}

export const quarterNames: QuarterName[] = ['Fall', 'Winter', 'Spring', 'Summer1', 'Summer2', 'Summer10wk'];
export const quarterDisplayNames: Record<QuarterName, string> = {
  Fall: 'Fall',
  Winter: 'Winter',
  Spring: 'Spring',
  Summer1: 'Summer I',
  Summer2: 'Summer II',
  Summer10wk: 'Summer 10 Week',
};

export function normalizeQuarterName(name: string): QuarterName {
  if (quarterNames.includes(name as QuarterName)) return name as QuarterName;
  const lookup: { [k: string]: QuarterName } = {
    fall: 'Fall',
    winter: 'Winter',
    spring: 'Spring',
    // Old Lowercase Display Names
    'summer I': 'Summer1',
    'summer II': 'Summer2',
    'summer 10 Week': 'Summer10wk',
    // Transcript Names
    'First Summer': 'Summer1',
    'Second Summer': 'Summer2',
    'Special / 10-Week Summer': 'Summer10wk',
  };
  if (!lookup[name]) throw TypeError('Invalid Quarter Name: ' + name);
  return lookup[name];
}
