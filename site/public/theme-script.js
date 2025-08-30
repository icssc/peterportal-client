function setBodyThemeAttribute() {
  const isDeterministic = (theme) => theme === 'light' || theme === 'dark';
  const preloadedTheme = document.documentElement.dataset.theme;
  if (isDeterministic(preloadedTheme)) return;

  const localTheme = localStorage.getItem('theme');
  if (preloadedTheme !== 'system' && isDeterministic(localTheme)) {
    document.documentElement.dataset.theme = localTheme;
    return;
  }

  // Both preloaded and local are neither light nor dark
  const isSystemDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
  document.documentElement.dataset.theme = isSystemDark ? 'dark' : 'light';
}
setBodyThemeAttribute();
