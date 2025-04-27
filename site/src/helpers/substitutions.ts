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

  const expandedLabels = labels.map(expandAbbreviation); // expand any words in labels that may be abbreviated

  expandedLabels.sort();
  const first = expandedLabels[0].split(' ');
  const last = expandedLabels[expandedLabels.length - 1].split(' ').filter((word) => !word.startsWith('(')); // ignore any words within parentheses

  let i = 0;
  while (i < first.length && first[i] === last[i]) {
    i++;
  }
  return first.slice(0, i).join(' ');
}
