import { ProgramRequirement } from '@peterportal/types';

export const labelSubstitutions: Record<string, string> = {
  Div: 'Division',
  "Int'l": 'International',
  US: 'United States',
};

export function expandAbbreviation(label: string): string {
  let expandedLabel = label;

  for (const [abbrev, substitution] of Object.entries(labelSubstitutions)) {
    const regex = new RegExp(`\\b${abbrev}\\b`, 'gi');
    expandedLabel = expandedLabel.replace(regex, substitution);
  }

  return expandedLabel;
}

export function findCommonLabelPrefix(labels: string[]): string {
  if (labels.length === 0) return '';

  const expandedLabels = labels.map(expandAbbreviation);

  expandedLabels.sort();
  /* remove all text after parentheses */
  const first = expandedLabels[0].replace(/\s\(.*/g, '').split(' ');
  const last = expandedLabels
    .at(-1)!
    .replace(/\s\(.*/g, '')
    .split(' ');

  let i = 0;
  while (i < first.length && first[i] === last[i]) {
    i++;
  }
  return first.slice(0, i).join(' ');
}

export function normalizeGERequirements(fetchedRequirements: ProgramRequirement[]): void {
  fetchedRequirements.forEach((geRequirement) => {
    /* if a top level "Select 1 of the following" group of requirements is seen, attempt to replace the label */
    if (geRequirement.requirementType !== 'Group' || geRequirement.label !== 'Select 1 of the following') return;

    const nestedLabels = geRequirement.requirements.map((req) => req.label);
    const commonLabelPrefix = findCommonLabelPrefix(nestedLabels);

    if (commonLabelPrefix) {
      geRequirement.label = commonLabelPrefix; // if a common label prefix is found, use it as the top level label
    }
  });
}
