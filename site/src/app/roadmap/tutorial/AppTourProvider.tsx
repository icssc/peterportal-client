'use client';

import { FC, PropsWithChildren } from 'react';
import { TourProvider } from '@reactour/tour';

export const basePopoverStyle: React.CSSProperties = {
  backgroundColor: 'var(--mui-palette-overlay-overlay3)',
  color: 'var(--mui-palette-text-primary)',
  boxShadow: '0 0 10px #000',
};

const AppTourProvider: FC<PropsWithChildren> = ({ children }) => {
  return (
    <TourProvider
      steps={[]}
      padding={5}
      showBadge={false}
      showNavigation={false}
      showDots={false}
      showPrevNextButtons={false}
      styles={{
        maskArea: (base) => ({
          ...base,
          rx: 5,
        }),
        maskWrapper: (base) => ({
          ...base,
          color: 'rgba(0, 0, 0, 0.3)',
        }),
        popover: (base) => ({
          ...base,
          ...basePopoverStyle,
        }),
      }}
    >
      {children}
    </TourProvider>
  );
};

export default AppTourProvider;
