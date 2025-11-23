import { GE_TITLE_MAP } from './courseRequirements';

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

export function stringifySearchFilters(filters: FilterOptions): StringifiedFilterOptions {
  const removePrefix = (label: string) => label.replace(/^.*: /, '');
  const filterCategories = Object.entries(GE_TITLE_MAP)
    .filter(([, label]) => filters.geCategories.includes(removePrefix(label)))
    .map((e) => e[0])
    .join(',');

  return {
    // change empty string to undefined for API call purposes
    stringifiedLevels: filters.levels.join(',') || undefined,
    stringifiedGeCategories: filterCategories || undefined,
    stringifiedDepartments: filters.departments.join(',') || undefined,
  };
}
