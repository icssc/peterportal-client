import { FilterOptionsState } from '@mui/material';

const getAbbreviation = (name: string): string => {
  return name
    .replace(/^minor in\s?/i, '')
    .split(' ')
    .filter((word) => word[0] !== word[0].toLowerCase() && word[0] === word[0].toUpperCase())
    .map((word) => word[0])
    .join('');
};

export const mapAbbreviations = (items: { name: string }[]): Record<string, string[]> => {
  const map: Record<string, string[]> = {};
  for (const item of items) {
    const abbr = getAbbreviation(item.name);
    if (!map[abbr]) map[abbr] = [];
    map[abbr].push(item.name);
  }
  return map;
};

export const filterByAbbreviation = <T extends { label: string; value: { id: string } }>(
  options: T[],
  state: FilterOptionsState<T>,
  abbreviations: Record<string, string[]>,
): T[] => {
  const input = state.inputValue.trim().toUpperCase();
  const abbrMatches: string[] = [];

  if (input) {
    for (const [abbr, fullName] of Object.entries(abbreviations)) {
      if (abbr.startsWith(input)) {
        abbrMatches.push(...fullName);
      }
    }
  }

  const abbrFiltered = options.filter((option) =>
    abbrMatches.some((term) => option.label.toLowerCase().includes(term.toLowerCase())),
  );

  const filtered = options.filter(
    (option) =>
      option.label.toLowerCase().includes(state.inputValue.toLowerCase()) &&
      !abbrFiltered.some((a) => a.value.id === option.value.id),
  );

  return [...abbrFiltered, ...filtered];
};
