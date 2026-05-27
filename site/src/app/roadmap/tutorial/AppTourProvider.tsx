'use client';

import { FC, PropsWithChildren } from 'react';
import { TourProvider } from '@reactour/tour';

const AppTourProvider: FC<PropsWithChildren> = ({ children }) => {
  return (
    <TourProvider
      steps={[]}
      padding={5}
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
          background: '#fff',
          color: 'black',
          borderRadius: 5,
          boxShadow: '0 0 10px #000',
          padding: 20,
          paddingTop: 40,
          margin: '4px 20px 20px 20px',
        }),
      }}
    >
      {children}
    </TourProvider>
  );
};

export default AppTourProvider;
