import React from 'react';

export type Theme = 'dark' | 'light' | 'system';

const ThemeContext = React.createContext<{
  darkMode: boolean;
  usingSystemTheme: boolean;
  setTheme: (theme: Theme) => void;
}>({
  darkMode: false,
  usingSystemTheme: false,
  setTheme: () => {},
});

export default ThemeContext;
