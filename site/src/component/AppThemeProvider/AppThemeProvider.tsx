'use client';
import { FC, PropsWithChildren, useEffect, useState } from 'react';
import ThemeContext from '../../style/theme-context';
import { Theme } from '@peterportal/types';
import { useIsLoggedIn } from '../../hooks/isLoggedIn';
import trpc from '../../trpc';
import { createTheme, ThemeProvider, useMediaQuery } from '@mui/material';

const AppThemeProvider: FC<PropsWithChildren> = ({ children }) => {
  const isLoggedIn = useIsLoggedIn();
  const isSystemDark = useMediaQuery('(prefers-color-scheme: dark)');

  // default darkMode to local or system preferences
  /** @todo see if there's a way to make it so loading on first render (via localStorage) doesn't get overridden */
  const [themePreference, setThemePreference] = useState<Theme | null>(null);
  const [darkMode, setDarkMode] = useState(isSystemDark);

  // either preferences or system change can trigger a recomputation of whether we are in dark mode
  useEffect(() => {
    const fallbackToSystem = !themePreference || themePreference === 'system';
    setDarkMode(fallbackToSystem ? isSystemDark : themePreference === 'dark');
  }, [themePreference, isSystemDark]);

  useEffect(() => {
    // Theme styling is controlled by data-theme attribute on body being set to light or dark
    document.body.setAttribute('data-theme', darkMode ? 'dark' : 'light');
  }, [darkMode]);

  /**
   * Sets and stores the new theme preference
   * @param theme
   */
  const setTheme = (theme: Theme) => {
    setThemePreference(theme);
    if (isLoggedIn) {
      trpc.users.setTheme.mutate({ theme });
    } else {
      localStorage.setItem('theme', theme);
    }
  };

  useEffect(() => {
    // if logged in, load user theme from db
    if (!isLoggedIn) {
      setThemePreference((localStorage.getItem('theme') ?? null) as Theme | null);
      return;
    }
    trpc.users.get.query().then((res) => {
      if (res.theme) setThemePreference(res.theme);
    });
  }, [isLoggedIn, setThemePreference]);

  const muiTheme = createTheme({ palette: { mode: darkMode ? 'dark' : 'light' } });

  return (
    <ThemeContext.Provider value={{ darkMode, usingSystemTheme: themePreference === 'system', setTheme }}>
      <ThemeProvider theme={muiTheme}>{children}</ThemeProvider>
    </ThemeContext.Provider>
  );
};

export default AppThemeProvider;
