export function datesToStrings<T>(object: Record<string, unknown>) {
  for (const key of Object.keys(object)) {
    const value = object[key];
    if (value instanceof Date) {
      object[key] = value.toISOString();
    }
  }

  return object as T;
}
