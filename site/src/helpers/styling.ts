import { type Theme } from '@nivo/core';

export const getTheme = (darkMode: boolean): Theme => {
  return {
    axis: {
      ticks: {
        text: {
          fill: darkMode ? '#eee' : '#333',
        },
      },
      legend: {
        text: {
          fill: darkMode ? '#eee' : '#333',
        },
      },
    },
  };
};

export const getCssVariable = (name: string): string => {
  const bodyStyles = getComputedStyle(document.body);
  const valueFromBody = bodyStyles.getPropertyValue(name).trim();
  return valueFromBody || '#ffffff';
};
