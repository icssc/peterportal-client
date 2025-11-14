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
      blue: TwoToneColor;
      red: TwoToneColor;
      orange: TwoToneColor;
      yellow: TwoToneColor;
      green: TwoToneColor;
    };
    chart: {
      blue: string;
      red: string;
      orange: string;
      yellow: string;
      green: string;
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
      blue?: Partial<TwoToneColor>;
      red?: Partial<TwoToneColor>;
      orange?: Partial<TwoToneColor>;
      yellow?: Partial<TwoToneColor>;
      green?: Partial<TwoToneColor>;
    };
    chart?: {
      red?: string;
      orange?: string;
      yellow?: string;
      green?: string;
      pass?: string;
      noPass: string;
    };
  }
}
