import { FC } from 'react';
import { type Theme } from '@nivo/core';

const tooltipStyle: Theme = {
  tooltip: {
    container: {
      background: 'rgba(0,0,0,.87)',
      color: '#ffffff',
      fontSize: '1.2rem',
      outline: 'none',
      margin: 0,
      padding: '0.25em 0.5em',
      borderRadius: '2px',
    },
  },
};

const ChartTooltip: FC<{ label: string | number; value: string | number }> = ({ label, value }) => {
  return (
    <div style={tooltipStyle.tooltip?.container}>
      <strong>
        {label}: {value}
      </strong>
    </div>
  );
};

export default ChartTooltip;
