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
    };
    reviews?: Partial<ColorScale>;
    chart?: Partial<ChartColorScale>;
  }
}

declare module '@mui/material/Button' {
  interface ButtonPropsSizeOverrides {
    xsmall: true;
  }
}

declare module '@mui/material' {
  interface InputBasePropsSizeOverrides {
    xsmall: true;
  }
}

declare module '@mui/material/Autocomplete' {
  interface AutocompletePropsSizeOverrides {
    xsmall: true;
  }
}
