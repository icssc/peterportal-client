import { createTheme, PaletteOptions } from '@mui/material';

const palette: PaletteOptions = {
  primary: {
    main: 'var(--blue-primary)',
  },
  secondary: {
    main: 'var(--blue-secondary)',
  },
  error: {
    main: 'var(--red-primary)',
  },
  warning: {
    main: 'var(--orange-secondary)',
  },
  success: {
    main: 'var(--green-primary)',
  },
  background: {
    default: 'var(--background)',
    paper: 'var(--overlay1)',
  },
  text: {
    primary: 'var(--text)',
    secondary: 'var(--text-secondary)',
  },
};

export let theme = createTheme({
  cssVariables: { colorSchemeSelector: '[data-theme=%s]', nativeColor: true },
  colorSchemes: {
    dark: { palette: { mode: 'dark', ...palette } },
    light: { palette: { mode: 'light', ...palette } },
  },
  typography: {
    fontFamily: ['Roboto', 'Open Sans'].join(','),
  },
  spacing: 4,
});

theme = createTheme(theme, {
  components: {
    MuiCheckbox: {
      styleOverrides: {
        root: {
          width: '32px',
          height: '32px',
          margin: '-8px',

          '&.Mui-checked': {
            '& > input': {
              margin: '0px',
              top: '9px',
              appearance: 'none',
              left: '9px',
              width: 'calc(100% - 18px)',
              height: 'calc(100% - 18px)',
              background: 'white',
              zIndex: '0',
              borderRadius: '4px',
              opacity: '1',
            },
            '& > svg': {
              zIndex: '1',
            },
          },
        },
      },
    },
    MuiFormLabel: {
      styleOverrides: {
        root: {
          fontSize: '18px',
          fontWeight: '600',
          marginBottom: theme.spacing(2),
          color: theme.palette.text.primary,
          '&.Mui-focused': {
            color: 'inherit',
          },
        },
      },
    },
    MuiFormControl: {
      styleOverrides: {
        root: {
          display: 'flex',
          flexDirection: 'column',
          marginBottom: theme.spacing(2),
          '& .MuiInputBase-input': {
            padding: '5px 10px',
          },
          '& .MuiFormControlLabel-root': {
            display: 'flex',
            alignItems: 'center',
            paddingBlock: '2px',
            margin: '0',
          },
          '& .MuiFormControlLabel-label': {
            marginLeft: '10px',
            fontSize: '18px',
          },
          'input[type="file"]': {
            fontSize: '16px',
          },
          '> img': {
            width: '100%',
            height: 'auto',
          },
        },
      },
    },
    MuiInputBase: {
      styleOverrides: {
        input: {
          fontSize: '16px',
          backgroundColor: theme.palette.background.paper,
        },
      },
    },
  },
});
