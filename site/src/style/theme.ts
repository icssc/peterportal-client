import { createTheme, PaletteOptions } from '@mui/material';

const sharedColors = {
  primary: {
    main: '#2484c6',
  },
  accent: {
    main: '#74d1f6',
  },
  warning: {
    main: '#ecad6d',
  },
  success: {
    main: 'green',
  },
  misc: {
    midGray: '#8d8d8d',
  },
  gradeDist: {
    pass: '#4ab486',
    noPass: '#e36436',
  },
  green: {
    primary: 'green',
  },
  yellow: {
    primary: 'yellow',
    secondary: '#f5d77f',
  },
  orange: {
    primary: 'orange',
    secondary: '#ecad6d',
  },
  red: {
    secondary: '#e7966d',
  },
};

const lightPalette: PaletteOptions = {
  mode: 'light',
  ...sharedColors,
  secondary: {
    main: '#5babe1',
  },
  tertiary: {
    main: '#a0ceee',
  },
  error: {
    main: '#ce0000',
  },
  overlay: {
    overlay1: '#fff',
    overlay2: '#f5f6fc',
    overlay3: '#fff',
  },
  background: {
    default: '#f5f6fc',
    paper: '#fff',
  },
  text: {
    primary: '#212529',
    secondary: '#606166',
  },
  green: {
    ...sharedColors.green,
    secondary: '#87c587',
  },
  red: {
    primary: '#ce0000',
    ...sharedColors.red,
  },
};

// each overlay is about 5% brighter than the previous
// roughly following: https://m2.material.io/design/color/dark-theme.html
const darkPalette: PaletteOptions = {
  mode: 'dark',
  ...sharedColors,
  secondary: {
    main: '#185680',
  },
  tertiary: {
    main: '#0b293c',
  },
  error: {
    main: '#ff3333',
  },
  overlay: {
    overlay1: '#1e1e1e',
    overlay2: '#292929',
    overlay3: '#363636',
  },
  background: {
    default: '#121212',
    paper: '#1e1e1e',
  },
  text: {
    primary: '#fff',
    secondary: '#99999f',
  },
  green: {
    ...sharedColors.green,
    secondary: '#295629',
  },
  red: {
    primary: '#ff3333',
    ...sharedColors.red,
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
          width: 32,
          height: 32,
          margin: -8,
          '&.Mui-checked': {
            '& > input': {
              margin: 0,
              top: 9,
              appearance: 'none',
              left: 9,
              width: 'calc(100% - 18px)',
              height: 'calc(100% - 18px)',
              background: 'white',
              zIndex: 0,
              borderRadius: 4,
              opacity: 1,
            },
            '& > svg': {
              zIndex: 1,
            },
          },
        },
      },
    },
    MuiFormLabel: {
      styleOverrides: {
        root: {
          fontSize: 18,
          fontWeight: 600,
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
            paddingBlock: 2,
            margin: 0,
          },
          '& .MuiFormControlLabel-label': {
            marginLeft: 10,
            fontSize: 18,
          },
          'input[type="file"]': {
            fontSize: 16,
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
          fontSize: 16,
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
          fontSize: 44,
        },
        iconEmpty: {
          color: 'white',
        },
        iconFilled: {
          color: 'var(--mui-palette-primary-main)',
        },
        iconHover: {
          opacity: 0.6,
        },
      },
    },
  },
});
