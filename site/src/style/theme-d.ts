import '@mui/material/styles';

declare module '@mui/material/styles' {
  interface Palette {
    tertiary: Palette['primary'];
    accent: Palette['primary'];
    overlay: {
      overlay1: string;
      overlay2: string;
      overlay3: string;
    };
    misc: {
      green: string;
      yellow: string;
      red: string;
      midGray: string;
    };
    gradeDist: {
      pass: string;
      noPass: string;
    };
  }
  interface PaletteOptions {
    tertiary?: PaletteOptions['primary'];
    accent?: PaletteOptions['primary'];
    overlay?: {
      overlay1?: string;
      overlay2?: string;
      overlay3?: string;
    };
    misc?: {
      green?: string;
      yellow?: string;
      red?: string;
      midGray?: string;
      // grayBlue?: string;
      // ringRoadWhite?: string;
      gradeDistP?: string;
      gradeDistNp?: string;
    };
    gradeDist?: {
      pass?: string;
      noPass: string;
    };
  }
}
