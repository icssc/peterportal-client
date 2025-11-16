import '@mui/material/styles';
import { PaletteColor, PaletteColorOptions } from '@mui/material/styles';

declare module '@mui/material/styles' {
  interface TwoToneColor {
    primary: string;
    secondary: string;
  }

  interface ColorScale {
    red: string;
    orange: string;
    yellow: string;
    green: string;
    blue: string;
  }

  type ChartColorScale = ColorScale & { pass: string; noPass: string };

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
      success: TwoToneColor;
      error: string;
      warning: string;
    };
    reviews: ColorScale;
    chart: ChartColorScale;
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
      success?: Partial<TwoToneColor>;
      error?: string;
      warning?: string;
    };
    reviews?: Partial<ColorScale>;
    chart?: Partial<ChartColorScale>;
  }
}
