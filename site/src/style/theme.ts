import { createTheme } from '@mui/material';

export const muiTheme = createTheme({
  cssVariables: { colorSchemeSelector: '[data-theme=%s]' },
  colorSchemes: {
    dark: { palette: { mode: 'dark' } },
    light: { palette: { mode: 'light' } },
  },
  palette: {
    primary: {
      main: '#2484c6', // blue-primary
    },
    secondary: {
      main: '#5babe1', // blue-secondary
      dark: '#185680', // dark blue-secondary
    },
    error: {
      main: '#ce0000', // red-primary
      dark: '#ff3333', // dark red-primary
    },
    warning: {
      main: '#ecad6d', // orange-secondary
    },
    success: {
      main: '#008000', // green-primary
    },
  },
  typography: {
    fontFamily: ['Roboto', 'Open Sans'].join(','),
  },
});
