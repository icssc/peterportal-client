import '@mui/material/styles';
import { PaletteColor, PaletteColorOptions } from '@mui/material/styles';

declare module '@mui/material/styles' {
  interface TwoToneColor {
    primary: string;
    secondary: string;
  }

  interface Palette {
    tertiary: PaletteColor;
    accent: PaletteColor;
    overlay: {
      overlay1: string;
      overlay2: string;
      overlay3: string;
    };
    misc: {
      midGray: string;
    };
    chart: {
      red: TwoToneColor;
      orange: TwoToneColor;
      yellow: TwoToneColor;
      green: TwoToneColor;
      pass: string;
      noPass: string;
    };
  }

  interface PaletteOptions {
    tertiary?: PaletteColorOptions;
    accent?: PaletteColorOptions;
    overlay?: {
      overlay1?: string;
      overlay2?: string;
      overlay3?: string;
    };
    misc?: {
      midGray?: string;
    };
    chart?: {
      red?: Partial<TwoToneColor>;
      orange?: Partial<TwoToneColor>;
      yellow?: Partial<TwoToneColor>;
      green?: Partial<TwoToneColor>;
      pass?: string;
      noPass: string;
    };
  }
}

declare module '@mui/material/Button' {
  interface ButtonPropsSizeOverrides {
    xsmall: true;
  }
}
