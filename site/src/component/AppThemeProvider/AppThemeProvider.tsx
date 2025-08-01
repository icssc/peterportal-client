'use client';
import { FC, PropsWithChildren, useCallback, useEffect, useState } from 'react';
import ThemeContext from '../../style/theme-context';
import { Theme } from '@peterportal/types';
import { useIsLoggedIn } from '../../hooks/isLoggedIn';
import trpc from '../../trpc';
import { createTheme, ThemeProvider } from '@mui/material';

function isSystemDark() {
  if (typeof window === 'undefined') return false;
  return window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
}

function isLocalUsingSystemTheme() {
  if (typeof localStorage === 'undefined') return true;
  return localStorage.getItem('theme') === 'system' || !localStorage.getItem('theme');
}

const AppThemeProvider: FC<PropsWithChildren> = ({ children }) => {
  const isLoggedIn = useIsLoggedIn();

  // default darkMode to local or system preferences
  const [usingSystemTheme, setUsingSystemTheme] = useState(isLocalUsingSystemTheme());
  const [darkMode, setDarkMode] = useState(
    usingSystemTheme ? isSystemDark() : localStorage.getItem('theme') === 'dark',
  );

  const [prevDarkMode, setPrevDarkMode] = useState(false); // light theme is default on page load

  if (typeof document !== 'undefined') {
    // Theme styling is controlled by data-theme attribute on body being set to light or dark
    document.body.setAttribute('data-theme', darkMode ? 'dark' : 'light');
  }

  /**
   * we run this check at render-time and compare with previous state because a useEffect
   * would cause a flicker for dark mode users on page load since the first render would be without
   * the data-theme property set (light would be used by default)
   */
  if (darkMode != prevDarkMode) {
    setPrevDarkMode(darkMode);
  }

  /**
   * Sets the theme state and saves the users theme preference.
   * Saves to account if logged in, local storage if not
   * @param theme
   */
  const setTheme = (theme: Theme) => {
    setThemeState(theme);
    if (isLoggedIn) {
      trpc.users.setTheme.mutate({ theme });
    } else {
      localStorage.setItem('theme', theme);
    }
  };

  /**
   * Sets the theme state
   * @param theme
   */
  const setThemeState = useCallback((theme: Theme) => {
    if (theme === 'system') {
      setDarkMode(isSystemDark());
      setUsingSystemTheme(true);
    } else {
      setDarkMode(theme === 'dark');
      setUsingSystemTheme(false);
    }
  }, []);

  useEffect(() => {
    const setSystemTheme = () => setThemeState('system');
    const matcher = window.matchMedia('(prefers-color-scheme: dark)');

    if (usingSystemTheme) matcher.addEventListener('change', setSystemTheme);
    return () => matcher.removeEventListener('change', setSystemTheme);
  }, [setThemeState, usingSystemTheme]);

  useEffect(() => {
    // if logged in, load user theme from db
    if (!isLoggedIn) return;
    trpc.users.get.query().then((res) => {
      if (res.theme) setThemeState(res.theme);
    });
  }, [isLoggedIn, setThemeState]);

  const muiTheme = createTheme({ palette: { mode: darkMode ? 'dark' : 'light' } });

  return (
    <ThemeContext.Provider value={{ darkMode, usingSystemTheme, setTheme }}>
      <ThemeProvider theme={muiTheme}>{children}</ThemeProvider>
    </ThemeContext.Provider>
  );
};

export default AppThemeProvider;
