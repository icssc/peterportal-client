import { createTheme, PaletteOptions } from '@mui/material';

const lightPalette: PaletteOptions = {
  mode: 'light',
  primary: {
    main: '#2484c6',
  },
  secondary: {
    main: '#5babe1',
  },
  error: {
    main: '#ce0000',
  },
  warning: {
    main: '#ecad6d',
  },
  success: {
    main: 'green',
  },
  background: {
    default: '#f5f6fc',
    paper: '#fff',
  },
  text: {
    primary: '#212529',
    secondary: '#606166',
  },
};

const darkPalette: PaletteOptions = {
  mode: 'dark',
  primary: {
    main: '#2484c6',
  },
  secondary: {
    main: '#185680',
  },
  error: {
    main: '#ff3333',
  },
  warning: {
    main: '#ecad6d',
  },
  success: {
    main: 'green',
  },
  background: {
    default: '#121212',
    paper: '#1e1e1e',
  },
  text: {
    primary: '#fff',
    secondary: '#99999f',
  },
};

export let theme = createTheme({
  cssVariables: { colorSchemeSelector: '[data-theme=%s]', nativeColor: true },
  colorSchemes: {
    dark: { palette: { ...darkPalette } },
    light: { palette: { ...lightPalette } },
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
          marginBottom: theme.spacing(1),
          color: 'var(--mui-palette-text-primary)',
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
          marginBottom: theme.spacing(4),
          '&:last-child': {
            marginBottom: 0,
          },
          '& .MuiInputBase-input': {
            padding: '7px 12px',
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
          backgroundColor: 'var(--mui-palette-background-paper)',
        },
      },
    },
    MuiOutlinedInput: {
      styleOverrides: {
        root: {
          '&:hover:not(:focus-within) .MuiOutlinedInput-notchedOutline': {
            borderColor: 'var(--mui-palette-text-secondary)',
          },
        },
      },
    },
    MuiRating: {
      styleOverrides: {
        sizeLarge: {
          fontSize: '44px',
        },
        iconEmpty: {
          color: 'white',
        },
        iconFilled: {
          color: 'var(--mui-palette-primary-main)',
        },
        iconHover: {
          opacity: '0.6',
        },
      },
    },
  },
});
