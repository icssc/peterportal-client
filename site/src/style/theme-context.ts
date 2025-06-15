import { createContext } from 'react';
import { Theme } from '@peterportal/types';

const ThemeContext = createContext<{
  darkMode: boolean;
  usingSystemTheme: boolean;
  setTheme: (theme: Theme) => void;
}>({
  darkMode: false,
  usingSystemTheme: false,
  setTheme: () => {},
});

export default ThemeContext;
