export interface FilterOptions {
  levels: string[];
  geCategories: string[];
  departments: string[];
}

interface StringifiedFilterOptions {
  stringifiedLevels?: string;
  stringifiedGeCategories?: string;
  stringifiedDepartments?: string;
}

export const levels: Record<string, string> = {
  LowerDiv: 'Lower Division',
  UpperDiv: 'Upper Division',
  Graduate: 'Graduate',
};

export const geCategories: Record<string, string> = {
  'GE-1A': 'Lower Division Writing',
  'GE-1B': 'Upper Division Writing',
  'GE-2': 'Science and Technology',
  'GE-3': 'Social and Behavioral Sciences',
  'GE-4': 'Arts and Humanities',
  'GE-5A': 'Quantitative Literacy',
  'GE-5B': 'Formal Reasoning',
  'GE-6': 'Language Other Than English',
  'GE-7': 'Multicultural Studies',
  'GE-8': 'International/Global Issues',
};

export function stringifySearchFilters(filters: FilterOptions): StringifiedFilterOptions {
  return {
    stringifiedLevels: Object.entries(levels).find(([, label]) => label === filters.levels.join(','))?.[0],
    stringifiedGeCategories: Object.entries(geCategories).find(
      ([, label]) => label === filters.geCategories.join(','),
    )?.[0],
    stringifiedDepartments: filters.departments.length > 0 ? filters.departments.join(',') : undefined,
  };
}
