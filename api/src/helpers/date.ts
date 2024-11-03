export function datesToStrings<T>(object: Record<string, unknown>) {
  for (const key of Object.keys(object)) {
    if (object[key] instanceof Date) {
      object[key] = object[key].toISOString();
    }
  }

  return object as T;
}
