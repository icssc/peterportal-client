const labelSubstitutions: Record<string, string> = {
  'Upper-Div': 'Upper-Division',
  min: 'minimum',
};

export function expandAbbreviation(label: string): string {
  let updated = label;

  for (const [pattern, replacement] of Object.entries(labelSubstitutions)) {
    const regex = new RegExp(`\\b${pattern}\\b`, 'g');
    updated = updated.replace(regex, replacement);
  }

  return updated;
}

export function getCommonLabelPrefix(strings: string[]): string {
  if (strings.length === 0) return '';

  strings.sort();
  const first = strings[0].split(' ');
  const last = strings[strings.length - 1].split(' ').filter((word) => !word.startsWith('(')); // strip words within parentheses

  let i = 0;
  while (i < first.length && first[i] === last[i]) {
    i++;
  }
  return first.slice(0, i).join(' ');
}
