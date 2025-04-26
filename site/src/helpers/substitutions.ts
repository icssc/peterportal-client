export function getCommonPrefix(strings: string[]): string {
  if (strings.length === 0) return '';

  strings.sort();
  const first = strings[0];
  const last = strings[strings.length - 1];

  let i = 0;
  while (i < first.length && first[i] === last[i]) {
    i++;
  }
  return first.slice(0, i);
}

export function stripSecondParenthesis(label: string): string {
  const firstParenthesis = label.indexOf('(');
  if (firstParenthesis === -1) return label.trim();

  const secondParenthesis = label.indexOf('(', firstParenthesis + 1);
  if (secondParenthesis === -1) return label.trim();

  return label.slice(0, secondParenthesis).trim();
}
