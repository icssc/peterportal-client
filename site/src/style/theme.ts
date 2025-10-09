import { createTheme } from '@mui/material';

export const muiTheme = createTheme({
  cssVariables: { colorSchemeSelector: '[data-theme=%s]' },
  // colorSchemes: {
  //   dark: { palette: { mode: 'dark' } },
  //   light: { palette: { mode: 'light' } },
  // },
  palette: {
    primary: {
      main: 'var(--blue-primary)', // blue-primary
      // contrastText: '#fff',
    },
    secondary: {
      main: 'var(--blue-secondary)',
      // dark: 'var(--blue-primary)', // dark blue-secondary
    },
    error: {
      main: 'var(--red-primary)', // red-primary
      // dark: 'var(--blue-secondary)', // dark red-primary
    },
    warning: {
      main: 'var(--orange-secondary)', // orange-secondary
    },
    success: {
      main: 'var(--green-primary)', // green-primary
    },
    background: {
      default: 'var(--background)',
      paper: 'var(--overlay1)',
    },
    text: {
      primary: 'var(--text)',
      secondary: 'var(--text-secondary',
    },
  },
  typography: {
    fontFamily: ['Roboto', 'Open Sans'].join(','),
  },
  components: {},
});
