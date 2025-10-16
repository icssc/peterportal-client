import '@mui/material/styles';
import '@mui/material/Button';

declare module '@mui/material/styles' {
  interface Palette {
    tertiary: Palette['primary'];
    accent: Palette['primary'];
    danger: Palette['primary'];
    overlay: {
      overlay1: string;
      overlay2: string;
      overlay3: string;
    };
    misc: {
      midGray: string;
    };
    gradeDist: {
      pass: string;
      noPass: string;
    };
    green: {
      primary: string;
      secondary: string;
    };
    yellow: {
      primary: string;
      secondary: string;
    };
    orange: {
      primary: string;
      secondary: string;
    };
    red: {
      primary: string;
      secondary: string;
    };
  }
  interface PaletteOptions {
    tertiary?: PaletteOptions['primary'];
    accent?: PaletteOptions['primary'];
    danger?: PaletteOptions['primary'];
    overlay?: {
      overlay1?: string;
      overlay2?: string;
      overlay3?: string;
    };
    misc?: {
      midGray?: string;
    };
    gradeDist?: {
      pass?: string;
      noPass: string;
    };
    green?: {
      primary?: string;
      secondary?: string;
    };
    yellow?: {
      primary?: string;
      secondary?: string;
    };
    orange?: {
      primary?: string;
      secondary?: string;
    };
    red?: {
      primary?: string;
      secondary?: string;
    };
  }
}

declare module '@mui/material/Button' {
  interface ButtonPropsColorOverrides {
    danger: true;
  }
  interface ButtonPropsVariantOverrides {
    neutral: true;
  }
}
