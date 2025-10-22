import '@mui/material/styles';
import { PaletteColor, PaletteColorOptions } from '@mui/material/styles';

declare module '@mui/material/styles' {
  interface Palette {
    tertiary: PaletteColor;
    accent: PaletteColor;
    danger: PaletteColor;
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
    tertiary?: PaletteColorOptions;
    accent?: PaletteColorOptions;
    danger?: PaletteColorOptions;
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
