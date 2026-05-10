export type QuarterFilterName = 'Fall' | 'Winter' | 'Spring' | 'Summer';

const searchPrevYears = 2;

function quarterOfferedRecently(terms: string[], quarter: QuarterFilterName): boolean {
  const currentDate = new Date();
  const currentStartYear = currentDate.getMonth() >= 8 ? currentDate.getFullYear() : currentDate.getFullYear() - 1;
  const termYear = quarter === 'Fall' ? currentStartYear : currentStartYear + 1;

  for (let i = 0; i <= searchPrevYears; i++) {
    const year = termYear - i;
    if (quarter === 'Summer') {
      if (
        terms.includes(`${year} Summer10wk`) ||
        terms.includes(`${year} Summer1`) ||
        terms.includes(`${year} Summer2`)
      ) {
        return true;
      }
    } else {
      if (terms.includes(`${year} ${quarter}`)) {
        return true;
      }
    }
  }
  return false;
}

export function courseMatchesQuarterFilter(terms: string[], quarterFilters: QuarterFilterName[]): boolean {
  if (quarterFilters.length === 0) return true;
  if (terms.length === 0) return false;
  return quarterFilters.some((filter) => quarterOfferedRecently(terms, filter));
}
